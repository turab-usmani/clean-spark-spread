# ==============================================================================
# 04_cointegration_tests.R — Johansen & Engle-Granger Cointegration Tests
# Clean Spark Spread Statistical Arbitrage Engine
# ==============================================================================
# Tests whether power, gas, and carbon prices share a stable long-run
# equilibrium relationship (cointegration), which would provide the
# theoretical foundation for a mean-reverting spread trade.
#
# Why two tests?
# - Johansen test: Multivariate, tests for the NUMBER of cointegrating
#   relationships among all three series simultaneously. Preferred because
#   the CSS is a function of three variables, not two.
# - Engle-Granger test: Simpler two-step procedure (OLS → ADF on residuals).
#   Used as a cross-check. Limitation: only tests for ONE cointegrating
#   relationship and is sensitive to the choice of dependent variable.
# ==============================================================================

library(urca)
library(tseries)
library(tidyverse)

# ------------------------------------------------------------------------------
# Johansen Cointegration Test
# ------------------------------------------------------------------------------

#' Run Johansen cointegration test on power, gas, and carbon prices
#'
#' Tests for the existence and number of cointegrating vectors among the
#' three price series. Uses both trace and maximum eigenvalue statistics.
#'
#' @param df Tibble with columns: power_price_eur_mwh, gas_price_eur_mwh,
#'           carbon_price_eur_t.
#' @param K Integer. Number of lags in the VAR model (default: 2).
#' @return A list containing the ca.jo result object and summary tibble.
run_johansen_test <- function(df, K = 2) {

  cat("\n============================================================\n")
  cat("JOHANSEN COINTEGRATION TEST\n")
  cat("============================================================\n")
  cat("Testing for cointegrating relationships among:\n")
  cat("  Power Price, Gas Price, Carbon Price\n")
  cat("H0(r=0): No cointegrating relationship\n")
  cat("H0(r≤1): At most 1 cointegrating relationship\n")
  cat("H0(r≤2): At most 2 cointegrating relationships\n")
  cat("------------------------------------------------------------\n\n")

  # Prepare matrix of price levels
  price_matrix <- df |>
    select(power_price_eur_mwh, gas_price_eur_mwh, carbon_price_eur_t) |>
    as.matrix()

  # Run Johansen test with TRACE statistic
  johansen_trace <- ca.jo(price_matrix, type = "trace", ecdet = "const", K = K)

  # Run Johansen test with MAX EIGENVALUE statistic
  johansen_eigen <- ca.jo(price_matrix, type = "eigen", ecdet = "const", K = K)

  # Extract results
  cat("TRACE TEST:\n")
  trace_summary <- summary(johansen_trace)
  print(trace_summary)

  cat("\nMAXIMUM EIGENVALUE TEST:\n")
  eigen_summary <- summary(johansen_eigen)
  print(eigen_summary)

  # Build results tibble
  trace_stats <- johansen_trace@teststat
  trace_cv_10 <- johansen_trace@cval[, 1]  # 10% critical value
  trace_cv_5  <- johansen_trace@cval[, 2]  # 5% critical value
  trace_cv_1  <- johansen_trace@cval[, 3]  # 1% critical value

  eigen_stats <- johansen_eigen@teststat
  eigen_cv_10 <- johansen_eigen@cval[, 1]
  eigen_cv_5  <- johansen_eigen@cval[, 2]
  eigen_cv_1  <- johansen_eigen@cval[, 3]

  # Determine number of cointegrating vectors (trace test)
  n_coint <- sum(trace_stats > trace_cv_5)

  results_df <- tibble(
    hypothesis = c("r = 0", "r ≤ 1", "r ≤ 2"),
    trace_stat = rev(trace_stats),
    trace_cv_5pct = rev(trace_cv_5),
    trace_reject = rev(trace_stats) > rev(trace_cv_5),
    eigen_stat = rev(eigen_stats),
    eigen_cv_5pct = rev(eigen_cv_5),
    eigen_reject = rev(eigen_stats) > rev(eigen_cv_5)
  )

  cat("\n------------------------------------------------------------\n")
  cat("SUMMARY TABLE:\n")
  print(results_df, n = Inf)

  cat(sprintf("\n>> Number of cointegrating vectors (5%% level, trace): %d\n", n_coint))

  if (n_coint >= 1) {
    cat(">> CONCLUSION: Evidence of cointegration found.\n")
    cat("   A stable long-run equilibrium among power, gas, and carbon prices\n")
    cat("   exists, supporting the theoretical basis for CSS mean-reversion.\n")
  } else {
    cat(">> CONCLUSION: No significant cointegration found at 5% level.\n")
    cat("   There is insufficient evidence of a stable long-run equilibrium.\n")
    cat("   The CSS may not exhibit reliable mean-reversion.\n")
    cat("   Strategy results should be interpreted with caution.\n")
  }
  cat("------------------------------------------------------------\n")

  return(list(
    trace_result = johansen_trace,
    eigen_result = johansen_eigen,
    results_df   = results_df,
    n_cointegrating_vectors = n_coint
  ))
}


# ------------------------------------------------------------------------------
# Engle-Granger Two-Step Test
# ------------------------------------------------------------------------------

#' Run Engle-Granger cointegration test as a cross-check
#'
#' Two-step procedure:
#' 1. OLS regression: power ~ gas + carbon (regress dependent on independent)
#' 2. ADF test on residuals: if residuals are stationary, cointegration exists
#'
#' Limitation: Tests only for a single cointegrating relationship and the
#' result depends on which variable is the dependent variable.
#' We use power price as the dependent variable because in the CSS formula,
#' power is the "output" and gas/carbon are "inputs."
#'
#' @param df Tibble with price columns.
#' @return A list with OLS coefficients, ADF results on residuals, and conclusion.
run_engle_granger_test <- function(df) {

  cat("\n============================================================\n")
  cat("ENGLE-GRANGER COINTEGRATION TEST (Cross-Check)\n")
  cat("============================================================\n")
  cat("Step 1: OLS regression — Power ~ Gas + Carbon\n")
  cat("Step 2: ADF test on residuals\n")
  cat("H0: Residuals have a unit root (no cointegration)\n")
  cat("------------------------------------------------------------\n\n")

  # Step 1: OLS regression
  ols_model <- lm(power_price_eur_mwh ~ gas_price_eur_mwh + carbon_price_eur_t,
                   data = df)

  cat("OLS Regression Results:\n")
  cat(sprintf("  Power = %.3f + %.3f × Gas + %.3f × Carbon\n",
              coef(ols_model)[1], coef(ols_model)[2], coef(ols_model)[3]))
  cat(sprintf("  R² = %.4f\n", summary(ols_model)$r.squared))
  cat(sprintf("  Adj. R² = %.4f\n", summary(ols_model)$adj.r.squared))

  # Step 2: ADF test on residuals
  residuals_vec <- residuals(ols_model)

  # Note: Critical values for the EG test are different from standard ADF
  # because we're testing residuals from a regression. However, for a
  # cross-check, we use the standard ADF test and note this limitation.
  adf_resid <- adf.test(residuals_vec)

  cat(sprintf("\nADF Test on Residuals:\n"))
  cat(sprintf("  ADF statistic = %.4f\n", adf_resid$statistic))
  cat(sprintf("  p-value = %.4f\n", adf_resid$p.value))
  cat(sprintf("  Lag order = %d\n", adf_resid$parameter[["Lag order"]]))

  # Note about critical values
  cat("\n  ⚠ Note: Standard ADF critical values are used here.\n")
  cat("    Engle-Granger-specific critical values are more conservative.\n")
  cat("    The Johansen test above is the primary evidence.\n")

  is_cointegrated <- adf_resid$p.value < 0.05

  cat(sprintf("\n>> CONCLUSION (Engle-Granger): %s\n",
              ifelse(is_cointegrated,
                     "Residuals are stationary → evidence of cointegration.",
                     "Residuals are non-stationary → no evidence of cointegration.")))
  cat("------------------------------------------------------------\n")

  return(list(
    ols_model      = ols_model,
    ols_r_squared  = summary(ols_model)$r.squared,
    adf_statistic  = as.numeric(adf_resid$statistic),
    adf_p_value    = adf_resid$p.value,
    is_cointegrated = is_cointegrated,
    residuals      = residuals_vec
  ))
}


# ------------------------------------------------------------------------------
# Combined Summary
# ------------------------------------------------------------------------------

#' Summarize cointegration evidence from both tests
#'
#' @param johansen_result List from run_johansen_test().
#' @param eg_result List from run_engle_granger_test().
#' @return A tibble summarizing both tests.
summarize_cointegration <- function(johansen_result, eg_result) {

  cat("\n============================================================\n")
  cat("COINTEGRATION EVIDENCE SUMMARY\n")
  cat("============================================================\n")

  johansen_evidence <- johansen_result$n_cointegrating_vectors > 0
  eg_evidence <- eg_result$is_cointegrated

  cat(sprintf("  Johansen (trace, 5%%): %s (%d cointegrating vectors)\n",
              ifelse(johansen_evidence, "YES — cointegration found", "NO — no cointegration"),
              johansen_result$n_cointegrating_vectors))
  cat(sprintf("  Engle-Granger:        %s (ADF p = %.4f)\n",
              ifelse(eg_evidence, "YES — cointegration found", "NO — no cointegration"),
              eg_result$adf_p_value))

  if (johansen_evidence && eg_evidence) {
    cat("\n  → BOTH tests support cointegration.\n")
    cat("    Strong evidence for a stable long-run equilibrium.\n")
    overall <- "Strong evidence of cointegration"
  } else if (johansen_evidence || eg_evidence) {
    cat("\n  → MIXED evidence: one test supports cointegration, the other does not.\n")
    cat("    Moderate evidence — results should be interpreted with caution.\n")
    overall <- "Mixed evidence of cointegration"
  } else {
    cat("\n  → NEITHER test supports cointegration.\n")
    cat("    Weak evidence for mean-reversion in the CSS.\n")
    cat("    Strategy may not be theoretically well-founded.\n")
    overall <- "No significant evidence of cointegration"
  }
  cat("============================================================\n")

  summary_df <- tibble(
    test = c("Johansen (trace)", "Engle-Granger"),
    evidence = c(
      ifelse(johansen_evidence, "Cointegration found", "No cointegration"),
      ifelse(eg_evidence, "Cointegration found", "No cointegration")
    ),
    detail = c(
      sprintf("%d cointegrating vector(s)", johansen_result$n_cointegrating_vectors),
      sprintf("ADF p-value = %.4f", eg_result$adf_p_value)
    ),
    overall_conclusion = overall
  )

  return(summary_df)
}
