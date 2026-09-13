# ==============================================================================
# 03_stationarity_tests.R — Augmented Dickey-Fuller Stationarity Tests
# Clean Spark Spread Statistical Arbitrage Engine
# ==============================================================================
# Tests whether each price series and the CSS are stationary (I(0)) or
# non-stationary (I(1)). This is a precondition for meaningful cointegration
# analysis: we need the individual prices to be I(1) and the CSS to be I(0).
# ==============================================================================

library(tseries)
library(tidyverse)

#' Run an Augmented Dickey-Fuller test on a single series
#'
#' The ADF test null hypothesis is: the series has a unit root (non-stationary).
#' If p < 0.05, we reject the null and conclude the series is stationary.
#'
#' @param series Numeric vector. The time series to test.
#' @param name Character. Name of the series (for reporting).
#' @return A list with test results and interpretation.
run_adf_test <- function(series, name) {

  # Remove NAs
  series_clean <- na.omit(series)

  if (length(series_clean) < 20) {
    warning(sprintf("Series '%s' has fewer than 20 observations. ADF test may be unreliable.", name))
  }

  # Run ADF test
  # Using default settings: type = "c" (with constant), lag order auto-selected
  test_result <- adf.test(series_clean)

  # Determine conclusion
  p_value <- test_result$p.value
  statistic <- test_result$statistic

  if (p_value < 0.01) {
    conclusion <- "Stationary (I(0)) — strong evidence (p < 0.01)"
    is_stationary <- TRUE
  } else if (p_value < 0.05) {
    conclusion <- "Stationary (I(0)) — moderate evidence (p < 0.05)"
    is_stationary <- TRUE
  } else if (p_value < 0.10) {
    conclusion <- "Borderline — weak evidence against unit root (p < 0.10)"
    is_stationary <- FALSE  # Conservative: treat as non-stationary
  } else {
    conclusion <- "Non-stationary (I(1)) — cannot reject unit root"
    is_stationary <- FALSE
  }

  result <- list(
    name          = name,
    n_obs         = length(series_clean),
    adf_statistic = as.numeric(statistic),
    p_value       = p_value,
    lag_order     = test_result$parameter[["Lag order"]],
    conclusion    = conclusion,
    is_stationary = is_stationary
  )

  return(result)
}


#' Run ADF tests on all series (power, gas, carbon, CSS)
#'
#' Expected results for a well-specified CSS model:
#'   - Power, Gas, Carbon: NON-stationary (I(1)) — they are trending price series
#'   - CSS: STATIONARY (I(0)) — if mean-reversion hypothesis holds
#'
#' @param df Tibble with columns: power_price_eur_mwh, gas_price_eur_mwh,
#'           carbon_price_eur_t, css.
#' @return A tibble summarizing all ADF test results.
test_all_stationarity <- function(df) {

  cat("\n============================================================\n")
  cat("STATIONARITY TESTS (Augmented Dickey-Fuller)\n")
  cat("============================================================\n")
  cat("H0: Series has a unit root (non-stationary)\n")
  cat("H1: Series is stationary\n")
  cat("Reject H0 if p-value < 0.05\n")
  cat("------------------------------------------------------------\n\n")

  series_list <- list(
    list(data = df$power_price_eur_mwh, name = "Power Price (EUR/MWh)"),
    list(data = df$gas_price_eur_mwh,   name = "Gas Price (EUR/MWh)"),
    list(data = df$carbon_price_eur_t,  name = "Carbon Price (EUR/t)"),
    list(data = df$css,                 name = "Clean Spark Spread")
  )

  results <- map(series_list, function(s) {
    res <- run_adf_test(s$data, s$name)
    cat(sprintf("  %-28s | ADF = %7.3f | p = %.4f | %s\n",
                res$name, res$adf_statistic, res$p_value, res$conclusion))
    return(res)
  })

  # Convert to tibble for reporting
  results_df <- tibble(
    series        = map_chr(results, "name"),
    n_obs         = map_int(results, "n_obs"),
    adf_statistic = map_dbl(results, "adf_statistic"),
    p_value       = map_dbl(results, "p_value"),
    lag_order     = map_int(results, "lag_order"),
    conclusion    = map_chr(results, "conclusion"),
    is_stationary = map_lgl(results, "is_stationary")
  )

  # Interpretation
  cat("\n------------------------------------------------------------\n")
  cat("INTERPRETATION:\n")

  prices_nonstationary <- !any(results_df |>
                                 filter(series != "Clean Spark Spread") |>
                                 pull(is_stationary))
  css_stationary <- results_df |>
    filter(series == "Clean Spark Spread") |>
    pull(is_stationary)

  if (prices_nonstationary && css_stationary) {
    cat("  ✓ Individual prices are non-stationary (I(1)) as expected.\n")
    cat("  ✓ CSS is stationary (I(0)), supporting mean-reversion hypothesis.\n")
    cat("  → Conditions for meaningful cointegration analysis are met.\n")
  } else if (!prices_nonstationary) {
    cat("  ⚠ Some individual price series appear stationary.\n")
    cat("    This is unexpected and may affect cointegration analysis.\n")
  } else if (!css_stationary) {
    cat("  ⚠ CSS does NOT appear stationary based on ADF test.\n")
    cat("    Mean-reversion hypothesis is not supported at the 5% level.\n")
    cat("    Cointegration testing will provide additional evidence.\n")
  }
  cat("------------------------------------------------------------\n")

  return(results_df)
}
