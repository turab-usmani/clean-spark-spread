# ==============================================================================
# 05_mean_reversion.R — Mean-Reversion Speed Estimation
# Clean Spark Spread Statistical Arbitrage Engine
# ==============================================================================
# Estimates the half-life of mean reversion using an AR(1) model.
# The half-life determines how quickly the spread reverts to its mean,
# which directly informs the z-score window length in signal construction.
# ==============================================================================

library(tidyverse)

#' Estimate the half-life of mean reversion for a spread series
#'
#' Uses an AR(1) regression on the spread:
#'   CSS_t = alpha + beta * CSS_{t-1} + epsilon_t
#'
#' The mean-reversion speed is derived from beta:
#'   - If 0 < beta < 1: mean-reverting. Half-life = -ln(2) / ln(beta)
#'   - If beta >= 1: no mean-reversion (random walk or explosive)
#'   - If beta <= 0: oscillatory (unusual for price spreads)
#'
#' The half-life represents the expected number of days for the spread
#' to revert halfway back to its mean from any deviation.
#'
#' @param css_series Numeric vector. The CSS time series.
#' @return A list with AR(1) results and half-life estimate.
estimate_halflife <- function(css_series) {

  cat("\n============================================================\n")
  cat("MEAN-REVERSION SPEED ESTIMATION (AR(1) Half-Life)\n")
  cat("============================================================\n")
  cat("Model: CSS_t = alpha + beta * CSS_{t-1} + epsilon\n")
  cat("Half-life = -ln(2) / ln(beta)\n")
  cat("------------------------------------------------------------\n\n")

  # Extract css column if a data.frame was passed
  if (is.data.frame(css_series)) {
    if ("css" %in% names(css_series)) {
      css_series <- css_series[["css"]]
    } else {
      css_series <- css_series[[1]]
    }
  }

  # Remove NAs and ensure numeric vector
  css_clean <- as.numeric(na.omit(css_series))
  n <- length(css_clean)

  if (n < 30) {
    warning("Fewer than 30 observations. Half-life estimate may be unreliable.")
  }

  # AR(1) regression: CSS_t on CSS_{t-1}
  css_lag <- css_clean[-n]       # CSS_{t-1} (lagged)
  css_now <- css_clean[-1]       # CSS_t     (current)

  ar1_model <- lm(css_now ~ css_lag)
  ar1_summary <- summary(ar1_model)

  alpha <- coef(ar1_model)[1]
  beta  <- coef(ar1_model)[2]
  beta_se <- ar1_summary$coefficients[2, 2]
  beta_pvalue <- ar1_summary$coefficients[2, 4]
  r_squared <- ar1_summary$r.squared

  cat(sprintf("  alpha (intercept) = %.4f\n", alpha))
  cat(sprintf("  beta  (AR coeff)  = %.4f (SE = %.4f, p < %.4f)\n",
              beta, beta_se, beta_pvalue))
  cat(sprintf("  R²                = %.4f\n", r_squared))
  cat(sprintf("  N observations    = %d\n\n", n - 1))

  # Compute half-life
  if (beta > 0 && beta < 1) {
    half_life <- -log(2) / log(beta)

    # 95% confidence interval for half-life via delta method
    # d(half_life)/d(beta) = -ln(2) / (beta * (ln(beta))^2)
    # But more practical: use beta +/- 1.96*SE to get CI bounds
    beta_lo <- beta - 1.96 * beta_se
    beta_hi <- beta + 1.96 * beta_se

    hl_lo <- if (beta_hi > 0 && beta_hi < 1) -log(2) / log(beta_hi) else NA
    hl_hi <- if (beta_lo > 0 && beta_lo < 1) -log(2) / log(beta_lo) else NA

    cat(sprintf("  Half-life = %.1f days\n", half_life))
    if (!is.na(hl_lo) && !is.na(hl_hi)) {
      cat(sprintf("  95%% CI    = [%.1f, %.1f] days\n", hl_lo, hl_hi))
    }
    cat(sprintf("\n  → The spread takes ~%.0f trading days to revert halfway to its mean.\n",
                half_life))
    suggested_win <- max(round(2 * half_life), 20L)
    cat(sprintf("  → Suggested z-score window: %d days (2 × half-life, min 20 days for statistical stability).\n",
                suggested_win))

    is_mean_reverting <- TRUE

  } else if (beta >= 1) {
    half_life <- Inf
    hl_lo <- NA
    hl_hi <- NA
    cat("  ⚠ beta >= 1: No mean-reversion detected.\n")
    cat("    The series behaves like a random walk or is explosive.\n")
    cat("    Strategy based on mean-reversion is NOT supported.\n")
    is_mean_reverting <- FALSE

  } else {
    half_life <- NA
    hl_lo <- NA
    hl_hi <- NA
    cat("  ⚠ beta <= 0: Oscillatory behavior detected.\n")
    cat("    This is unusual for price spreads. Check data quality.\n")
    is_mean_reverting <- FALSE
  }

  # Implied long-run mean
  if (beta != 1) {
    long_run_mean <- alpha / (1 - beta)
    cat(sprintf("\n  Implied long-run mean CSS = %.2f EUR/MWh\n", long_run_mean))
  }

  cat("============================================================\n")

  return(list(
    model           = ar1_model,
    alpha           = as.numeric(alpha),
    beta            = as.numeric(beta),
    beta_se         = beta_se,
    r_squared       = r_squared,
    half_life       = half_life,
    half_life_ci_lo = hl_lo,
    half_life_ci_hi = hl_hi,
    is_mean_reverting = is_mean_reverting,
    suggested_window = if (is_mean_reverting) as.integer(max(round(2 * half_life), 20L)) else as.integer(ZSCORE_WINDOW),
    long_run_mean   = if (beta != 1) as.numeric(alpha / (1 - beta)) else NA,
    n_obs           = n - 1
  ))
}
