# ==============================================================================
# 07_backtest.R — Vectorized Backtest Engine
# Clean Spark Spread Statistical Arbitrage Engine
# ==============================================================================
# Simulates trading the CSS based on the z-score signal, including
# transaction costs. Computes standard performance metrics and compares
# against a naive buy-and-hold baseline.
# ==============================================================================

library(tidyverse)
library(PerformanceAnalytics)
library(xts)

#' Run the vectorized backtest
#'
#' Daily P&L = position_t × (CSS_t - CSS_{t-1}) - cost × |position_t - position_{t-1}|
#'
#' The first term captures profit from holding the spread.
#' The second term penalizes each trade (entry, exit, or reversal).
#'
#' @param df Tibble with columns: date, css, position.
#' @param cost_per_trade Numeric. Transaction cost per trade in EUR/MWh (default from config).
#' @return The input df with additional columns: daily_pnl, cumulative_pnl, equity.
run_backtest <- function(df, cost_per_trade = TRANSACTION_COST) {

  cat("\n============================================================\n")
  cat("RUNNING BACKTEST\n")
  cat(sprintf("Transaction cost: %.2f EUR/MWh per round-trip\n", cost_per_trade))
  cat("============================================================\n\n")

  result <- df |>
    mutate(
      # Daily change in CSS
      css_change = css - dplyr::lag(css),
      # Position change (detects trades)
      position_change = position - dplyr::lag(position, default = 0),
      # Execution timing: position decided at close of bar t-1 is held during bar t
      # This strictly eliminates lookahead bias and ensures trades capture next-day moves
      position_held = dplyr::lag(position, default = 0),
      # Daily P&L: position_held × ΔCSS - cost × |Δposition|
      # Cost is applied per unit of position change (each leg of a trade)
      daily_pnl = position_held * css_change -
                  cost_per_trade * abs(position_change) / 2,
      # Handle first row (no lag available)
      daily_pnl = ifelse(is.na(daily_pnl), 0, daily_pnl),
      # Cumulative P&L (equity curve)
      cumulative_pnl = cumsum(daily_pnl)
    )

  # Summary statistics
  total_pnl <- tail(result$cumulative_pnl, 1)
  n_trades  <- sum(abs(result$position_change) > 0, na.rm = TRUE)
  n_days    <- nrow(result)

  cat(sprintf("  Total P&L:          %.2f EUR/MWh\n", total_pnl))
  cat(sprintf("  Number of trades:   %d\n", n_trades))
  cat(sprintf("  Trading days:       %d\n", n_days))

  return(result)
}


#' Run the buy-and-hold baseline
#'
#' Always holds a long position in the spread (position = 1).
#' This is the simplest possible strategy — if the z-score strategy
#' can't beat this, it adds no value over passive exposure.
#'
#' @param df Tibble with columns: date, css.
#' @param cost_per_trade Numeric. Transaction cost (applied once at entry).
#' @return The input df with baseline P&L columns.
run_baseline <- function(df, cost_per_trade = TRANSACTION_COST) {

  cat("\n>> Running buy-and-hold baseline...\n")

  result <- df |>
    mutate(
      baseline_position = 1,
      baseline_css_change = css - lag(css),
      baseline_daily_pnl = baseline_css_change,
      baseline_daily_pnl = ifelse(is.na(baseline_daily_pnl), -cost_per_trade / 2, baseline_daily_pnl),
      baseline_cumulative_pnl = cumsum(baseline_daily_pnl)
    )

  total_pnl <- tail(result$baseline_cumulative_pnl, 1)
  cat(sprintf("  Baseline total P&L: %.2f EUR/MWh\n", total_pnl))

  return(result)
}


#' Compute standard performance metrics using PerformanceAnalytics
#'
#' @param daily_pnl Numeric vector. Daily P&L series.
#' @param dates Date vector. Corresponding dates.
#' @param label Character. Name for this strategy.
#' @return A named list of performance metrics.
compute_performance_metrics <- function(daily_pnl, dates, label = "Strategy") {

  # Convert to xts for PerformanceAnalytics
  # We use daily P&L as "returns" (in EUR/MWh, not percentage returns)
  # This is appropriate for a spread trade where notional is fixed
  pnl_xts <- xts(daily_pnl, order.by = dates)

  # For Sharpe ratio calculation, we treat daily P&L as returns
  # Risk-free rate assumed to be 0 for spread strategies
  cumulative <- cumsum(daily_pnl)
  total_return <- tail(cumulative, 1)

  # Annualized Sharpe Ratio (assuming ~252 trading days per year)
  daily_mean <- mean(daily_pnl, na.rm = TRUE)
  daily_sd   <- sd(daily_pnl, na.rm = TRUE)
  sharpe     <- if (daily_sd > 0) (daily_mean / daily_sd) * sqrt(252) else 0

  # Maximum Drawdown
  running_max <- cummax(cumulative)
  drawdown    <- cumulative - running_max
  max_drawdown <- min(drawdown)

  # Hit Rate (% of profitable trading days when in a position)
  # Only count days when a position is held
  active_pnl <- daily_pnl[daily_pnl != 0]
  if (length(active_pnl) > 0) {
    hit_rate <- sum(active_pnl > 0) / length(active_pnl)
  } else {
    hit_rate <- NA
  }

  # Calmar Ratio (annualized return / max drawdown)
  n_years <- as.numeric(diff(range(dates))) / 365.25
  annualized_return <- total_return / n_years
  calmar <- if (max_drawdown < 0) annualized_return / abs(max_drawdown) else NA

  # Profit Factor
  gross_profit <- sum(daily_pnl[daily_pnl > 0], na.rm = TRUE)
  gross_loss   <- abs(sum(daily_pnl[daily_pnl < 0], na.rm = TRUE))
  profit_factor <- if (gross_loss > 0) gross_profit / gross_loss else Inf

  metrics <- list(
    label              = label,
    total_pnl          = total_return,
    annualized_pnl     = annualized_return,
    sharpe_ratio       = sharpe,
    max_drawdown       = max_drawdown,
    calmar_ratio       = calmar,
    hit_rate           = hit_rate,
    profit_factor      = profit_factor,
    n_trading_days     = length(daily_pnl),
    daily_mean_pnl     = daily_mean,
    daily_sd_pnl       = daily_sd,
    best_day           = max(daily_pnl, na.rm = TRUE),
    worst_day          = min(daily_pnl, na.rm = TRUE)
  )

  return(metrics)
}


#' Compare strategy vs baseline performance side-by-side
#'
#' @param df Tibble with backtest and baseline columns.
#' @return A tibble with performance comparison.
compare_strategies <- function(df) {

  cat("\n============================================================\n")
  cat("PERFORMANCE COMPARISON\n")
  cat("============================================================\n\n")

  # Compute metrics for strategy
  strat_metrics <- compute_performance_metrics(
    df$daily_pnl, df$date, label = "Z-Score Strategy"
  )

  # Compute metrics for baseline
  baseline_metrics <- compute_performance_metrics(
    df$baseline_daily_pnl, df$date, label = "Buy-and-Hold"
  )

  # Build comparison table
  comparison <- tibble(
    metric = c("Total P&L (EUR/MWh)",
               "Annualized P&L (EUR/MWh/yr)",
               "Sharpe Ratio (annualized)",
               "Maximum Drawdown (EUR/MWh)",
               "Calmar Ratio",
               "Hit Rate (%)",
               "Profit Factor",
               "Best Day (EUR/MWh)",
               "Worst Day (EUR/MWh)"),
    strategy = c(
      sprintf("%.2f", strat_metrics$total_pnl),
      sprintf("%.2f", strat_metrics$annualized_pnl),
      sprintf("%.2f", strat_metrics$sharpe_ratio),
      sprintf("%.2f", strat_metrics$max_drawdown),
      sprintf("%.2f", strat_metrics$calmar_ratio),
      sprintf("%.1f%%", strat_metrics$hit_rate * 100),
      sprintf("%.2f", strat_metrics$profit_factor),
      sprintf("%.2f", strat_metrics$best_day),
      sprintf("%.2f", strat_metrics$worst_day)
    ),
    baseline = c(
      sprintf("%.2f", baseline_metrics$total_pnl),
      sprintf("%.2f", baseline_metrics$annualized_pnl),
      sprintf("%.2f", baseline_metrics$sharpe_ratio),
      sprintf("%.2f", baseline_metrics$max_drawdown),
      sprintf("%.2f", baseline_metrics$calmar_ratio),
      sprintf("%.1f%%", baseline_metrics$hit_rate * 100),
      sprintf("%.2f", baseline_metrics$profit_factor),
      sprintf("%.2f", baseline_metrics$best_day),
      sprintf("%.2f", baseline_metrics$worst_day)
    )
  )

  print(comparison, n = Inf)
  cat("============================================================\n")

  return(list(
    comparison   = comparison,
    strategy     = strat_metrics,
    baseline     = baseline_metrics
  ))
}


#' Compare In-Sample vs Out-of-Sample performance
#'
#' Evaluates strategy metrics on the training/crisis period (IS) versus
#' the unseen post-crisis test period (OOS).
#'
#' @param df Tibble with daily_pnl, baseline_daily_pnl, date columns.
#' @param split_date Date. Cutoff separating IS and OOS (default: TRAIN_TEST_SPLIT_DATE).
#' @return A list with is_metrics, oos_metrics, full_metrics, and a comparison tibble.
compare_is_oos <- function(df, split_date = TRAIN_TEST_SPLIT_DATE) {

  cat("\n============================================================\n")
  cat("IN-SAMPLE VS. OUT-OF-SAMPLE PERFORMANCE EVALUATION\n")
  cat(sprintf("Split Date: %s\n", split_date))
  cat("============================================================\n\n")

  df_is  <- df |> filter(date < split_date)
  df_oos <- df |> filter(date >= split_date)

  is_strat   <- compute_performance_metrics(df_is$daily_pnl, df_is$date, label = "In-Sample Strategy")
  oos_strat  <- compute_performance_metrics(df_oos$daily_pnl, df_oos$date, label = "Out-of-Sample Strategy")
  full_strat <- compute_performance_metrics(df$daily_pnl, df$date, label = "Full-Sample Strategy")

  is_base   <- compute_performance_metrics(df_is$baseline_daily_pnl, df_is$date, label = "In-Sample Baseline")
  oos_base  <- compute_performance_metrics(df_oos$baseline_daily_pnl, df_oos$date, label = "Out-of-Sample Baseline")
  full_base <- compute_performance_metrics(df$baseline_daily_pnl, df$date, label = "Full-Sample Baseline")

  comparison <- tibble(
    metric = c("Date Range",
               "Trading Days",
               "Strategy Total P&L (EUR/MWh)",
               "Strategy Annualized P&L (EUR/MWh)",
               "Strategy Sharpe Ratio",
               "Strategy Max Drawdown (EUR/MWh)",
               "Strategy Hit Rate (%)",
               "Strategy Profit Factor",
               "Baseline Total P&L (EUR/MWh)",
               "Baseline Sharpe Ratio"),
    in_sample = c(
      sprintf("%s to %s", min(df_is$date), max(df_is$date)),
      as.character(is_strat$n_trading_days),
      sprintf("%.2f", is_strat$total_pnl),
      sprintf("%.2f", is_strat$annualized_pnl),
      sprintf("%.2f", is_strat$sharpe_ratio),
      sprintf("%.2f", is_strat$max_drawdown),
      sprintf("%.1f%%", is_strat$hit_rate * 100),
      sprintf("%.2f", is_strat$profit_factor),
      sprintf("%.2f", is_base$total_pnl),
      sprintf("%.2f", is_base$sharpe_ratio)
    ),
    out_of_sample = c(
      sprintf("%s to %s", min(df_oos$date), max(df_oos$date)),
      as.character(oos_strat$n_trading_days),
      sprintf("%.2f", oos_strat$total_pnl),
      sprintf("%.2f", oos_strat$annualized_pnl),
      sprintf("%.2f", oos_strat$sharpe_ratio),
      sprintf("%.2f", oos_strat$max_drawdown),
      sprintf("%.1f%%", oos_strat$hit_rate * 100),
      sprintf("%.2f", oos_strat$profit_factor),
      sprintf("%.2f", oos_base$total_pnl),
      sprintf("%.2f", oos_base$sharpe_ratio)
    ),
    full_sample = c(
      sprintf("%s to %s", min(df$date), max(df$date)),
      as.character(full_strat$n_trading_days),
      sprintf("%.2f", full_strat$total_pnl),
      sprintf("%.2f", full_strat$annualized_pnl),
      sprintf("%.2f", full_strat$sharpe_ratio),
      sprintf("%.2f", full_strat$max_drawdown),
      sprintf("%.1f%%", full_strat$hit_rate * 100),
      sprintf("%.2f", full_strat$profit_factor),
      sprintf("%.2f", full_base$total_pnl),
      sprintf("%.2f", full_base$sharpe_ratio)
    )
  )

  print(comparison, n = Inf)
  cat("============================================================\n")

  return(list(
    comparison  = comparison,
    is_strat    = is_strat,
    oos_strat   = oos_strat,
    full_strat  = full_strat,
    is_base     = is_base,
    oos_base    = oos_base,
    full_base   = full_base
  ))
}

