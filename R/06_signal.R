# ==============================================================================
# 06_signal.R — Rolling Z-Score Signal Construction
# Clean Spark Spread Statistical Arbitrage Engine
# ==============================================================================
# Constructs a mean-reversion trading signal based on rolling z-scores.
# CRITICAL: All rolling statistics use only past data (align = "right")
# to prevent look-ahead bias.
# ==============================================================================

library(tidyverse)
library(zoo)

#' Compute rolling z-score of the CSS spread
#'
#' Z-score_t = (CSS_t - rolling_mean_t) / rolling_sd_t
#'
#' where rolling_mean_t and rolling_sd_t use ONLY data from
#' [t - window + 1, t] — i.e., strictly backward-looking.
#'
#' This is enforced by zoo::rollapply(..., align = "right").
#' The first (window - 1) observations will be NA because there isn't
#' enough history to compute the rolling statistics.
#'
#' @param css_series Numeric vector. The CSS time series.
#' @param window Integer. Rolling window length in trading days.
#' @return A tibble with rolling_mean, rolling_sd, and zscore columns.
compute_rolling_zscore <- function(css_series, window) {

  window <- as.integer(unlist(window)[1])
  cat(sprintf(">> Computing rolling z-score (window = %d days, right-aligned)...\n", window))

  n <- length(css_series)

  if (window >= n) {
    stop(sprintf("Window (%d) must be smaller than the series length (%d).", window, n))
  }

  # Rolling mean using ONLY past data (align = "right" means the window
  # ends at the current observation, never looks ahead)
  rolling_mean <- rollapply(css_series, width = window, FUN = mean,
                             align = "right", fill = NA)

  # Rolling standard deviation — same alignment
  rolling_sd <- rollapply(css_series, width = window, FUN = sd,
                           align = "right", fill = NA)

  # Z-score: how many standard deviations from the rolling mean
  zscore <- (css_series - rolling_mean) / rolling_sd

  # Replace any Inf/-Inf (from sd = 0) with NA
  zscore[!is.finite(zscore)] <- NA

  n_valid <- sum(!is.na(zscore))
  cat(sprintf("   %d valid z-score observations (first %d are NA due to window warm-up)\n",
              n_valid, window - 1))

  return(tibble(
    rolling_mean = rolling_mean,
    rolling_sd   = rolling_sd,
    zscore       = zscore
  ))
}


#' Generate trading signals from z-scores using a state machine
#'
#' State transitions:
#'   FLAT → LONG  when zscore < -entry_threshold  (spread is cheap)
#'   FLAT → SHORT when zscore > +entry_threshold   (spread is rich)
#'   LONG → FLAT  when zscore > -exit_threshold    (reverted enough)
#'   SHORT → FLAT when zscore < +exit_threshold    (reverted enough)
#'
#' No look-ahead: signal at time t depends only on z-score at time t,
#' which itself is computed from data up to time t only.
#'
#' @param zscore Numeric vector. Z-score series.
#' @param entry_threshold Numeric. Absolute z-score level to enter (default: 1.5).
#' @param exit_threshold Numeric. Absolute z-score level to exit (default: 0.5).
#' @return A numeric vector: +1 (long), -1 (short), 0 (flat).
generate_signals <- function(zscore,
                              entry_threshold = ENTRY_THRESHOLD,
                              exit_threshold = EXIT_THRESHOLD) {

  cat(sprintf(">> Generating trading signals (entry=±%.1f, exit=±%.1f)...\n",
              entry_threshold, exit_threshold))

  n <- length(zscore)
  position <- rep(0, n)  # Initialize all flat
  state <- 0  # 0 = flat, 1 = long, -1 = short

  for (i in seq_len(n)) {
    if (is.na(zscore[i])) {
      position[i] <- state  # Maintain current position if z-score is NA
      next
    }

    if (state == 0) {
      # FLAT: check for entry
      if (zscore[i] < -entry_threshold) {
        state <- 1   # Go LONG (spread is abnormally low, expect reversion up)
      } else if (zscore[i] > entry_threshold) {
        state <- -1  # Go SHORT (spread is abnormally high, expect reversion down)
      }
    } else if (state == 1) {
      # LONG: check for exit
      if (zscore[i] > -exit_threshold) {
        state <- 0  # Exit long (reverted enough)
      }
    } else if (state == -1) {
      # SHORT: check for exit
      if (zscore[i] < exit_threshold) {
        state <- 0  # Exit short (reverted enough)
      }
    }

    position[i] <- state
  }

  n_long  <- sum(position == 1, na.rm = TRUE)
  n_short <- sum(position == -1, na.rm = TRUE)
  n_flat  <- sum(position == 0, na.rm = TRUE)
  n_trades <- sum(diff(position) != 0, na.rm = TRUE)

  cat(sprintf("   Days: %d long, %d short, %d flat\n", n_long, n_short, n_flat))
  cat(sprintf("   Total position changes (trades): %d\n", n_trades))

  return(position)
}


#' Add signal columns to the CSS dataframe
#'
#' @param df Tibble with a `css` column.
#' @param window Integer. Rolling window length.
#' @param entry_threshold Numeric. Entry z-score threshold.
#' @param exit_threshold Numeric. Exit z-score threshold.
#' @return The input df with additional columns: rolling_mean, rolling_sd,
#'         zscore, position, signal_type.
add_signals_to_df <- function(df, window = ZSCORE_WINDOW,
                               entry_threshold = ENTRY_THRESHOLD,
                               exit_threshold = EXIT_THRESHOLD) {

  # Compute rolling z-scores
  zscore_data <- compute_rolling_zscore(df$css, window)

  # Generate position signals
  position <- generate_signals(zscore_data$zscore, entry_threshold, exit_threshold)

  # Add to dataframe
  result <- df |>
    mutate(
      rolling_mean = zscore_data$rolling_mean,
      rolling_sd   = zscore_data$rolling_sd,
      zscore       = zscore_data$zscore,
      position     = position,
      # Track entry/exit points for visualization
      position_change = c(0, diff(position)),
      signal_type = case_when(
        position_change > 0 & position == 1  ~ "Long Entry",
        position_change < 0 & position == -1 ~ "Short Entry",
        position_change != 0 & position == 0 & lag(position) == 1  ~ "Long Exit",
        position_change != 0 & position == 0 & lag(position) == -1 ~ "Short Exit",
        TRUE ~ NA_character_
      )
    )

  # Count signal events
  signal_counts <- result |>
    filter(!is.na(signal_type)) |>
    count(signal_type)

  cat("\nSignal summary:\n")
  for (i in seq_len(nrow(signal_counts))) {
    cat(sprintf("   %s: %d\n", signal_counts$signal_type[i], signal_counts$n[i]))
  }

  return(result)
}
