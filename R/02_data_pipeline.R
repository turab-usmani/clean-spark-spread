# ==============================================================================
# 02_data_pipeline.R — Data Alignment, Cleaning & CSS Computation
# Clean Spark Spread Statistical Arbitrage Engine
# ==============================================================================
# Functions to align multiple price series to a common daily frequency,
# handle missing values explicitly, and compute the Clean Spark Spread.
# ==============================================================================

library(tidyverse)
library(zoo)

# ------------------------------------------------------------------------------
# Missing Value Handling
# ------------------------------------------------------------------------------

#' Handle missing values with documented, justified approach
#'
#' Strategy: Last Observation Carried Forward (LOCF) for gaps <= max_gap days.
#' Gaps longer than max_gap are left as NA and flagged.
#'
#' Justification: In energy markets, prices are not published on weekends and
#' holidays. LOCF for short gaps (1-3 days) is standard practice because the
#' last known settlement price is the best available estimate. For longer gaps,
#' LOCF becomes unreliable (structural changes may have occurred), so we
#' preserve NA to avoid stale data propagation.
#'
#' @param df A tibble with a `date` column and one or more price columns.
#' @param max_gap Integer. Maximum consecutive NAs to fill with LOCF (default: 3).
#' @return The tibble with NAs handled, plus a summary printed to console.
handle_missing_values <- function(df, max_gap = 3L) {

  price_cols <- setdiff(names(df), "date")
  cat(sprintf(">> Handling missing values (LOCF for gaps <= %d days)...\n", max_gap))

  for (col in price_cols) {
    original_na <- sum(is.na(df[[col]]))

    if (original_na > 0) {
      # Identify gap lengths using run-length encoding
      is_na_vec <- is.na(df[[col]])
      rle_result <- rle(is_na_vec)

      # For short gaps (<= max_gap), apply LOCF
      # For long gaps, leave as NA
      filled <- na.locf(df[[col]], na.rm = FALSE, maxgap = max_gap)
      df[[col]] <- filled

      remaining_na <- sum(is.na(df[[col]]))
      filled_na <- original_na - remaining_na

      cat(sprintf("   %s: %d NAs found, %d filled (LOCF, gap <= %d), %d remaining\n",
                  col, original_na, filled_na, max_gap, remaining_na))
    } else {
      cat(sprintf("   %s: no missing values\n", col))
    }
  }

  return(df)
}


# ------------------------------------------------------------------------------
# Series Alignment
# ------------------------------------------------------------------------------

#' Align multiple price series to a common daily frequency
#'
#' Performs an inner join on date so we only keep dates where ALL three
#' price series have data. This is the most conservative approach —
#' no interpolation or gap-filling across series.
#'
#' @param power_df Tibble with date, power_price_eur_mwh.
#' @param gas_df Tibble with date, gas_price_eur_mwh.
#' @param carbon_df Tibble with date, carbon_price_eur_t.
#' @return A single aligned tibble.
align_daily_series <- function(power_df, gas_df, carbon_df) {

  cat(">> Aligning price series to common daily frequency (inner join)...\n")

  n_power  <- nrow(power_df)
  n_gas    <- nrow(gas_df)
  n_carbon <- nrow(carbon_df)

  aligned <- power_df |>
    inner_join(gas_df, by = "date") |>
    inner_join(carbon_df, by = "date") |>
    arrange(date)

  n_aligned <- nrow(aligned)

  cat(sprintf("   Power:  %d obs | Gas: %d obs | Carbon: %d obs\n",
              n_power, n_gas, n_carbon))
  cat(sprintf("   After alignment: %d common dates (%s to %s)\n",
              n_aligned, min(aligned$date), max(aligned$date)))
  cat(sprintf("   Dropped: Power=%d, Gas=%d, Carbon=%d dates with no match\n",
              n_power - n_aligned, n_gas - n_aligned, n_carbon - n_aligned))

  if (n_aligned < 252) {  # ~1 year of trading days
    warning("Fewer than 252 aligned observations. Statistical tests may lack power.")
  }

  return(aligned)
}


# ------------------------------------------------------------------------------
# CSS Computation
# ------------------------------------------------------------------------------

#' Compute the Clean Spark Spread
#'
#' CSS = Power Price - (Gas Price / Plant Efficiency) - (Carbon Price × Emissions Factor)
#'
#' The CSS measures the theoretical profit margin (EUR/MWh) of running a
#' gas-fired power plant after accounting for fuel and carbon costs.
#'
#' Note on units:
#'   - Power price: EUR/MWh_electric
#'   - Gas price: EUR/MWh_thermal → divide by efficiency to get fuel cost per MWh_electric
#'   - Carbon price: EUR/tCO2 → multiply by emissions factor (tCO2/MWh_thermal)
#'                   to get carbon cost per MWh_thermal of gas burned
#'
#' @param df Aligned tibble with power_price_eur_mwh, gas_price_eur_mwh, carbon_price_eur_t.
#' @param efficiency Numeric. Plant thermal efficiency (default from config).
#' @param emissions_factor Numeric. tCO2 per MWh_thermal (default from config).
#' @return The input tibble with an additional `css` column.
compute_css <- function(df,
                        efficiency = PLANT_EFFICIENCY,
                        emissions_factor = EMISSIONS_FACTOR) {

  cat(sprintf(">> Computing Clean Spark Spread (efficiency=%.0f%%, emissions=%.2f tCO2/MWh)...\n",
              efficiency * 100, emissions_factor))

  result <- df |>
    mutate(
      fuel_cost_eur_mwh   = gas_price_eur_mwh / efficiency,
      carbon_cost_eur_mwh = carbon_price_eur_t * emissions_factor,
      css = power_price_eur_mwh - fuel_cost_eur_mwh - carbon_cost_eur_mwh
    )

  cat(sprintf("   CSS summary: mean=%.2f, median=%.2f, sd=%.2f, min=%.2f, max=%.2f\n",
              mean(result$css, na.rm = TRUE),
              median(result$css, na.rm = TRUE),
              sd(result$css, na.rm = TRUE),
              min(result$css, na.rm = TRUE),
              max(result$css, na.rm = TRUE)))

  return(result)
}


# ------------------------------------------------------------------------------
# Master Pipeline
# ------------------------------------------------------------------------------

#' Build the complete CSS dataset
#'
#' Loads, aligns, cleans, and computes the CSS in one call.
#' Saves the processed dataset to data/processed/css_daily.csv.
#'
#' @return A tibble with the complete CSS dataset.
build_css_dataset <- function() {

  cat("\n============================================================\n")
  cat("BUILDING CSS DATASET\n")
  cat("============================================================\n\n")

  # 1. Load all price series
  prices <- load_all_prices()

  cat("\n")

  # 2. Align to common dates
  aligned <- align_daily_series(prices$power, prices$gas, prices$carbon)

  # 3. Handle missing values
  aligned <- handle_missing_values(aligned)

  # 4. Remove any remaining NAs (from gaps > max_gap)
  n_before <- nrow(aligned)
  aligned <- aligned |> filter(complete.cases(across(everything())))
  n_after <- nrow(aligned)
  if (n_before > n_after) {
    cat(sprintf(">> Removed %d rows with remaining NAs after gap-filling.\n",
                n_before - n_after))
  }

  # 5. Compute CSS
  css_data <- compute_css(aligned)

  # 6. Save processed dataset
  output_path <- file.path(PATH_PROCESSED_DATA, CSS_DAILY_FILENAME)
  write_csv(css_data, output_path)
  cat(sprintf("\n>> Processed dataset saved to: %s\n", output_path))

  cat("\n============================================================\n")
  cat(sprintf("CSS DATASET READY: %d observations, %s to %s\n",
              nrow(css_data), min(css_data$date), max(css_data$date)))
  cat("============================================================\n")

  return(css_data)
}
