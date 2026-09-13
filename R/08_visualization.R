# ==============================================================================
# 08_visualization.R — Publication-Quality ggplot2 Charts
# Clean Spark Spread Statistical Arbitrage Engine
# ==============================================================================
# All visualizations use a consistent custom theme and color palette.
# Designed to be portfolio/resume-ready without modification.
# ==============================================================================

library(tidyverse)
library(scales)
library(patchwork)

# ------------------------------------------------------------------------------
# Custom Theme
# ------------------------------------------------------------------------------

#' Custom ggplot2 theme for consistent, publication-quality visuals
theme_css <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      # Text
      plot.title       = element_text(face = "bold", size = rel(1.2), hjust = 0,
                                       margin = margin(b = 10)),
      plot.subtitle    = element_text(color = "#636e72", size = rel(0.85), hjust = 0,
                                       margin = margin(b = 15)),
      plot.caption     = element_text(color = "#95a5a6", size = rel(0.7), hjust = 1),
      axis.title       = element_text(color = "#2d3436", size = rel(0.85)),
      axis.text        = element_text(color = "#636e72"),
      legend.title     = element_text(face = "bold", size = rel(0.8)),
      legend.text      = element_text(size = rel(0.75)),
      legend.position  = "bottom",
      # Grid
      panel.grid.major = element_line(color = "#f0f0f0", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      # Background
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      # Margins
      plot.margin      = margin(15, 15, 15, 15)
    )
}


# ------------------------------------------------------------------------------
# Plot 1: Input Price Series (Faceted)
# ------------------------------------------------------------------------------

#' Plot all three input price series
#'
#' @param df Tibble with date and price columns.
#' @param save Logical. Whether to save the plot (default: TRUE).
#' @return A ggplot object.
plot_input_prices <- function(df, save = TRUE) {

  cat(">> Generating: Input price series plot...\n")

  price_long <- df |>
    select(date, power_price_eur_mwh, gas_price_eur_mwh, carbon_price_eur_t) |>
    pivot_longer(-date, names_to = "series", values_to = "price") |>
    mutate(series = case_when(
      series == "power_price_eur_mwh" ~ "Power Price (EUR/MWh)",
      series == "gas_price_eur_mwh"   ~ "Gas Price (EUR/MWh)",
      series == "carbon_price_eur_t"  ~ "Carbon Price (EUR/t)"
    ))

  color_map <- c(
    "Power Price (EUR/MWh)"  = COLOR_POWER,
    "Gas Price (EUR/MWh)"    = COLOR_GAS,
    "Carbon Price (EUR/t)"   = COLOR_CARBON
  )

  p <- ggplot(price_long, aes(x = date, y = price, color = series)) +
    geom_line(linewidth = 0.5) +
    facet_wrap(~ series, scales = "free_y", ncol = 1) +
    scale_color_manual(values = color_map) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
    labs(
      title    = "European Energy Market Prices",
      subtitle = "Germany (DE-LU) power, Dutch TTF gas, and EU ETS carbon prices",
      x = NULL, y = "Price",
      caption  = "Sources: ENTSO-E, Yahoo Finance (TTF=F), Investing.com (EU Carbon)"
    ) +
    theme_css() +
    theme(legend.position = "none",
          strip.text = element_text(face = "bold", size = 11))

  if (save) {
    ggsave(file.path(PATH_PLOTS, "01_input_prices.png"), p,
           width = 12, height = 8, dpi = 300, bg = "white")
    cat("   Saved: output/plots/01_input_prices.png\n")
  }

  return(p)
}


# ------------------------------------------------------------------------------
# Plot 2: CSS Time Series with Entry/Exit Signals
# ------------------------------------------------------------------------------

#' Plot CSS spread with trading signal markers
#'
#' @param df Tibble with date, css, signal_type columns.
#' @param save Logical.
#' @return A ggplot object.
plot_css_with_signals <- function(df, save = TRUE) {

  cat(">> Generating: CSS spread with signals plot...\n")

  signal_points <- df |> filter(!is.na(signal_type))

  shape_map <- c("Long Entry" = 24, "Short Entry" = 25,
                 "Long Exit" = 21, "Short Exit" = 21)
  color_map <- c("Long Entry" = COLOR_LONG, "Short Entry" = COLOR_SHORT,
                 "Long Exit" = COLOR_EXIT, "Short Exit" = COLOR_EXIT)
  fill_map  <- c("Long Entry" = COLOR_LONG, "Short Entry" = COLOR_SHORT,
                 "Long Exit" = "white", "Short Exit" = "white")

  p <- ggplot(df, aes(x = date, y = css)) +
    geom_line(color = COLOR_CSS_LINE, linewidth = 0.4, alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#bdc3c7", linewidth = 0.3) +
    geom_point(data = signal_points,
               aes(shape = signal_type, color = signal_type, fill = signal_type),
               size = 2.5, stroke = 0.8) +
    scale_shape_manual(values = shape_map) +
    scale_color_manual(values = color_map) +
    scale_fill_manual(values = fill_map) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
    labs(
      title    = "Clean Spark Spread with Trading Signals",
      subtitle = sprintf("CSS = Power - Gas/%.0f%% - Carbon × %.2f | ▲ Long entry, ▼ Short entry, ○ Exit",
                          PLANT_EFFICIENCY * 100, EMISSIONS_FACTOR),
      x = NULL, y = "CSS (EUR/MWh)",
      shape = "Signal", color = "Signal", fill = "Signal",
      caption = "Entry: |z-score| > 1.5 | Exit: |z-score| < 0.5"
    ) +
    theme_css()

  if (save) {
    ggsave(file.path(PATH_PLOTS, "02_css_with_signals.png"), p,
           width = 12, height = 6, dpi = 300, bg = "white")
    cat("   Saved: output/plots/02_css_with_signals.png\n")
  }

  return(p)
}


# ------------------------------------------------------------------------------
# Plot 3: Z-Score with Threshold Bands
# ------------------------------------------------------------------------------

#' Plot z-score with entry/exit threshold bands
#'
#' @param df Tibble with date and zscore columns.
#' @param save Logical.
#' @return A ggplot object.
plot_zscore <- function(df, save = TRUE) {

  cat(">> Generating: Z-score plot...\n")

  p <- ggplot(df |> filter(!is.na(zscore)), aes(x = date, y = zscore)) +
    # Shaded bands for entry zones
    annotate("rect", xmin = min(df$date), xmax = max(df$date),
             ymin = ENTRY_THRESHOLD, ymax = Inf,
             fill = COLOR_SHORT, alpha = 0.08) +
    annotate("rect", xmin = min(df$date), xmax = max(df$date),
             ymin = -Inf, ymax = -ENTRY_THRESHOLD,
             fill = COLOR_LONG, alpha = 0.08) +
    # Threshold lines
    geom_hline(yintercept = c(-ENTRY_THRESHOLD, ENTRY_THRESHOLD),
               linetype = "dashed", color = "#e74c3c", linewidth = 0.4) +
    geom_hline(yintercept = c(-EXIT_THRESHOLD, EXIT_THRESHOLD),
               linetype = "dotted", color = "#95a5a6", linewidth = 0.3) +
    geom_hline(yintercept = 0, color = "#bdc3c7", linewidth = 0.3) +
    # Z-score line
    geom_line(color = COLOR_STRATEGY, linewidth = 0.4) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
    labs(
      title    = "Rolling Z-Score of Clean Spark Spread",
      subtitle = sprintf("Dashed lines: ±%.1f entry threshold | Dotted: ±%.1f exit threshold",
                          ENTRY_THRESHOLD, EXIT_THRESHOLD),
      x = NULL, y = "Z-Score (σ)",
      caption = "Red shading: short zone | Green shading: long zone"
    ) +
    theme_css()

  if (save) {
    ggsave(file.path(PATH_PLOTS, "03_zscore_bands.png"), p,
           width = 12, height = 5, dpi = 300, bg = "white")
    cat("   Saved: output/plots/03_zscore_bands.png\n")
  }

  return(p)
}


# ------------------------------------------------------------------------------
# Plot 4: Equity Curve — Strategy vs Baseline
# ------------------------------------------------------------------------------

#' Plot cumulative P&L for strategy and baseline
#'
#' @param df Tibble with date, cumulative_pnl, baseline_cumulative_pnl columns.
#' @param save Logical.
#' @return A ggplot object.
plot_equity_curve <- function(df, split_date = TRAIN_TEST_SPLIT_DATE, save = TRUE) {

  cat(">> Generating: Equity curve plot...\n")

  equity_long <- df |>
    select(date, cumulative_pnl, baseline_cumulative_pnl) |>
    pivot_longer(-date, names_to = "strategy", values_to = "pnl") |>
    mutate(strategy = ifelse(strategy == "cumulative_pnl",
                              "Z-Score Strategy", "Buy-and-Hold Baseline"))

  pnl_min <- min(equity_long$pnl, na.rm = TRUE)
  pnl_max <- max(equity_long$pnl, na.rm = TRUE)
  label_y <- pnl_max - 0.1 * (pnl_max - pnl_min)

  p <- ggplot(equity_long, aes(x = date, y = pnl, color = strategy)) +
    geom_line(linewidth = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#bdc3c7", linewidth = 0.3) +
    geom_vline(xintercept = split_date, linetype = "dashed",
               color = "#e74c3c", linewidth = 0.8, alpha = 0.8) +
    annotate("label", x = split_date - 120, y = label_y,
             label = "◄ In-Sample (Train)", fill = "#fdf2e9", color = "#d35400",
             size = 3.5, fontface = "bold") +
    annotate("label", x = split_date + 120, y = label_y,
             label = "Out-of-Sample (Test) ►", fill = "#e8f8f5", color = "#16a085",
             size = 3.5, fontface = "bold") +
    scale_color_manual(values = c("Z-Score Strategy" = COLOR_STRATEGY,
                                   "Buy-and-Hold Baseline" = COLOR_BASELINE)) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
    labs(
      title    = "Cumulative P&L: In-Sample vs. Out-of-Sample Validation",
      subtitle = sprintf("Transaction cost: %.2f EUR/MWh | Vertical divider: Split date (%s)",
                         TRANSACTION_COST, split_date),
      x = NULL, y = "Cumulative P&L (EUR/MWh)",
      color = NULL,
      caption = "Train: 2022-2023 (Crisis) | Test: 2024-2025 (Normalization, Unseen Data)"
    ) +
    theme_css()

  if (save) {
    ggsave(file.path(PATH_PLOTS, "04_equity_curve.png"), p,
           width = 12, height = 6, dpi = 300, bg = "white")
    cat("   Saved: output/plots/04_equity_curve.png\n")
  }

  return(p)
}


# ------------------------------------------------------------------------------
# Plot 5: Drawdown Chart
# ------------------------------------------------------------------------------

#' Plot drawdown (underwater) chart for the strategy
#'
#' @param df Tibble with date and cumulative_pnl columns.
#' @param save Logical.
#' @return A ggplot object.
plot_drawdown <- function(df, save = TRUE) {

  cat(">> Generating: Drawdown chart...\n")

  drawdown_df <- df |>
    mutate(
      running_max = cummax(cumulative_pnl),
      drawdown    = cumulative_pnl - running_max
    )

  p <- ggplot(drawdown_df, aes(x = date, y = drawdown)) +
    geom_area(fill = COLOR_SHORT, alpha = 0.3) +
    geom_line(color = COLOR_SHORT, linewidth = 0.4) +
    geom_hline(yintercept = 0, color = "#2d3436", linewidth = 0.3) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
    labs(
      title    = "Strategy Drawdown (Underwater Chart)",
      subtitle = sprintf("Maximum drawdown: %.2f EUR/MWh",
                          min(drawdown_df$drawdown)),
      x = NULL, y = "Drawdown (EUR/MWh)"
    ) +
    theme_css()

  if (save) {
    ggsave(file.path(PATH_PLOTS, "05_drawdown.png"), p,
           width = 12, height = 4, dpi = 300, bg = "white")
    cat("   Saved: output/plots/05_drawdown.png\n")
  }

  return(p)
}


# ------------------------------------------------------------------------------
# Plot 6: Rolling Statistics (Mean ± 1σ Bands)
# ------------------------------------------------------------------------------

#' Plot CSS with rolling mean and ±1σ bands
#'
#' @param df Tibble with date, css, rolling_mean, rolling_sd columns.
#' @param save Logical.
#' @return A ggplot object.
plot_rolling_statistics <- function(df, save = TRUE) {

  cat(">> Generating: Rolling statistics plot...\n")

  ribbon_df <- df |>
    filter(!is.na(rolling_mean)) |>
    mutate(
      upper_band = rolling_mean + rolling_sd,
      lower_band = rolling_mean - rolling_sd
    )

  p <- ggplot() +
    geom_ribbon(data = ribbon_df,
                aes(x = date, ymin = lower_band, ymax = upper_band),
                fill = COLOR_BAND_FILL, alpha = 0.5) +
    geom_line(data = df, aes(x = date, y = css),
              color = COLOR_CSS_LINE, linewidth = 0.4, alpha = 0.6) +
    geom_line(data = ribbon_df, aes(x = date, y = rolling_mean),
              color = COLOR_STRATEGY, linewidth = 0.6, linetype = "solid") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
    labs(
      title    = "CSS with Rolling Mean and ±1σ Bands",
      subtitle = sprintf("Rolling window: %d trading days", ZSCORE_WINDOW),
      x = NULL, y = "CSS (EUR/MWh)",
      caption = "Dark line: rolling mean | Shaded area: ±1 standard deviation"
    ) +
    theme_css()

  if (save) {
    ggsave(file.path(PATH_PLOTS, "06_rolling_statistics.png"), p,
           width = 12, height = 5, dpi = 300, bg = "white")
    cat("   Saved: output/plots/06_rolling_statistics.png\n")
  }

  return(p)
}


# ------------------------------------------------------------------------------
# Generate All Plots
# ------------------------------------------------------------------------------

#' Generate and save all six plots
#'
#' @param df Fully processed tibble with all backtest columns.
#' @return A list of ggplot objects (invisible).
generate_all_plots <- function(df) {

  cat("\n============================================================\n")
  cat("GENERATING VISUALIZATIONS\n")
  cat("============================================================\n\n")

  # Ensure output directory exists
  dir.create(PATH_PLOTS, recursive = TRUE, showWarnings = FALSE)

  plots <- list(
    input_prices      = plot_input_prices(df),
    css_signals       = plot_css_with_signals(df),
    zscore            = plot_zscore(df),
    equity_curve      = plot_equity_curve(df),
    drawdown          = plot_drawdown(df),
    rolling_stats     = plot_rolling_statistics(df)
  )

  cat("\n>> All 6 plots generated and saved to output/plots/\n")
  cat("============================================================\n")

  return(invisible(plots))
}
