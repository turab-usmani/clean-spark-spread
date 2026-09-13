# ==============================================================================
# main.R — Master Orchestration Script
# Clean Spark Spread Statistical Arbitrage Engine
# ==============================================================================
# This script runs the complete analysis pipeline:
#   1. Load and prepare data
#   2. Stationarity tests (ADF)
#   3. Cointegration tests (Johansen + Engle-Granger)
#   4. Mean-reversion speed estimation (AR(1) half-life)
#   5. Signal construction (rolling z-score)
#   6. Backtest with transaction costs
#   7. Performance comparison vs. baseline
#   8. Generate all visualizations
#
# Usage:
#   1. Place required data files in data/raw/ (see README.md)
#   2. Set ENTSO-E API key if using API: Sys.setenv(ENTSOE_API_KEY = "your-key")
#   3. Run this script: source("main.R")
# ==============================================================================

cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║   CLEAN SPARK SPREAD — Statistical Arbitrage Engine         ║\n")
cat("║   European Power Markets Mean-Reversion Analysis            ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

# --- Setup -------------------------------------------------------------------

# Set working directory to project root (if running from RStudio)
if (interactive() && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Source all modules in order
cat(">> Sourcing R modules...\n")
source("R/00_config.R")
source("R/01_data_loaders.R")
source("R/02_data_pipeline.R")
source("R/03_stationarity_tests.R")
source("R/04_cointegration_tests.R")
source("R/05_mean_reversion.R")
source("R/06_signal.R")
source("R/07_backtest.R")
source("R/08_visualization.R")

# Create output directories
dir.create(PATH_PLOTS, recursive = TRUE, showWarnings = FALSE)
dir.create(PATH_RESULTS, recursive = TRUE, showWarnings = FALSE)
dir.create(PATH_PROCESSED_DATA, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# STEP 1: DATA PIPELINE & CHRONOLOGICAL TRAIN/TEST SPLIT
# ==============================================================================

css_data <- build_css_dataset()

# Split dataset into In-Sample (training) and Out-of-Sample (testing)
css_train <- css_data |> filter(date < TRAIN_TEST_SPLIT_DATE)
css_test  <- css_data |> filter(date >= TRAIN_TEST_SPLIT_DATE)

cat(sprintf("\n>> Chronological Data Split at %s:\n", TRAIN_TEST_SPLIT_DATE))
cat(sprintf("   In-Sample (Train/Crisis):     %d days (%s to %s, ~%.0f%%)\n",
            nrow(css_train), min(css_train$date), max(css_train$date),
            100 * nrow(css_train) / nrow(css_data)))
cat(sprintf("   Out-of-Sample (Test/Norm):    %d days (%s to %s, ~%.0f%%)\n",
            nrow(css_test), min(css_test$date), max(css_test$date),
            100 * nrow(css_test) / nrow(css_data)))

# ==============================================================================
# STEP 2: STATIONARITY TESTS (IN-SAMPLE)
# ==============================================================================

cat("\n>> Running stationarity tests on In-Sample (training) dataset...\n")
stationarity_results <- test_all_stationarity(css_train)

# Save results
write_csv(stationarity_results, file.path(PATH_RESULTS, "stationarity_tests.csv"))
cat(">> In-Sample stationarity results saved to output/results/stationarity_tests.csv\n")

# ==============================================================================
# STEP 3: COINTEGRATION TESTS (IN-SAMPLE)
# ==============================================================================

cat("\n>> Running cointegration tests on In-Sample (training) dataset...\n")
johansen_results <- run_johansen_test(css_train)
eg_results       <- run_engle_granger_test(css_train)
coint_summary    <- summarize_cointegration(johansen_results, eg_results)

# Save results
write_csv(johansen_results$results_df, file.path(PATH_RESULTS, "johansen_test.csv"))
write_csv(coint_summary, file.path(PATH_RESULTS, "cointegration_summary.csv"))
saveRDS(johansen_results, file.path(PATH_RESULTS, "johansen_results.rds"))
saveRDS(eg_results, file.path(PATH_RESULTS, "engle_granger_results.rds"))
cat(">> In-Sample cointegration results saved to output/results/\n")

# ==============================================================================
# STEP 4: MEAN-REVERSION SPEED (IN-SAMPLE)
# ==============================================================================

cat("\n>> Estimating half-life on In-Sample (training) dataset...\n")
halflife_results <- estimate_halflife(css_train$css)

# Update z-score window based on estimated in-sample half-life
if (halflife_results$is_mean_reverting) {
  ZSCORE_WINDOW <- halflife_results$suggested_window
  cat(sprintf("\n>> Z-score window calibrated from In-Sample half-life: %d days\n", ZSCORE_WINDOW))
} else {
  cat("\n>> WARNING: No mean-reversion detected. Using default window of 60 days.\n")
  cat("   Strategy results should be interpreted with extreme caution.\n")
}

# Save results
saveRDS(halflife_results, file.path(PATH_RESULTS, "halflife_results.rds"))

# ==============================================================================
# STEP 5: SIGNAL CONSTRUCTION (FULL DATASET, RIGHT-ALIGNED, NO LOOK-AHEAD)
# ==============================================================================

cat("\n============================================================\n")
cat("SIGNAL CONSTRUCTION\n")
cat("============================================================\n")

# Parameters calibrated strictly on In-Sample are applied sequentially
css_data <- add_signals_to_df(css_data, window = ZSCORE_WINDOW)

# ==============================================================================
# STEP 6: BACKTEST
# ==============================================================================

css_data <- run_backtest(css_data)
css_data <- run_baseline(css_data)

# ==============================================================================
# STEP 7: PERFORMANCE COMPARISON & IN-SAMPLE VS OUT-OF-SAMPLE VALIDATION
# ==============================================================================

performance <- compare_strategies(css_data)
is_oos_perf <- compare_is_oos(css_data, split_date = TRAIN_TEST_SPLIT_DATE)

# Save comparisons
write_csv(performance$comparison, file.path(PATH_RESULTS, "performance_comparison.csv"))
write_csv(is_oos_perf$comparison, file.path(PATH_RESULTS, "is_oos_performance.csv"))
saveRDS(is_oos_perf, file.path(PATH_RESULTS, "is_oos_results.rds"))
cat(">> Performance comparisons saved to output/results/\n")

# Save full processed dataset
write_csv(css_data |> select(-position_change),
          file.path(PATH_PROCESSED_DATA, "css_full_backtest.csv"))

# ==============================================================================
# STEP 8: VISUALIZATION
# ==============================================================================

plots <- generate_all_plots(css_data)

# ==============================================================================
# DONE
# ==============================================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║   ANALYSIS COMPLETE                                         ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║   Observations:  %-40s ║\n", nrow(css_data)))
cat(sprintf("║   Date range:    %-40s ║\n",
            paste(min(css_data$date), "to", max(css_data$date))))
cat(sprintf("║   Half-life:     %-40s ║\n",
            ifelse(halflife_results$is_mean_reverting,
                   sprintf("%.1f days", halflife_results$half_life),
                   "Not detected")))
cat(sprintf("║   Sharpe ratio:  %-40s ║\n",
            sprintf("%.2f (strategy) vs %.2f (baseline)",
                    performance$strategy$sharpe_ratio,
                    performance$baseline$sharpe_ratio)))
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║   Outputs saved to:                                         ║\n")
cat("║     output/plots/      — 6 publication-quality charts       ║\n")
cat("║     output/results/    — statistical test results            ║\n")
cat("║     data/processed/    — processed CSS dataset               ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║   To generate the full report:                              ║\n")
cat("║     rmarkdown::render('report.Rmd')                         ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")
