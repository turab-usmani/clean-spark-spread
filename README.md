# Clean Spark Spread — Statistical Arbitrage Engine

A rigorous quantitative research project that analyzes mean-reversion in the **Clean Spark Spread (CSS)** across European power markets and tests whether it can be exploited by a systematic trading strategy.

## What is the Clean Spark Spread?

The CSS measures the profit margin (EUR/MWh) of running a gas-fired power plant after accounting for fuel and carbon emission costs:

```
CSS = Power Price − (Gas Price / Plant Efficiency) − (Carbon Price × Emissions Factor)
```

**Economic intuition:** When the CSS is abnormally high, more gas plants dispatch (increasing supply, compressing the spread). When abnormally low, plants shut down (tightening supply, pushing the spread up). This negative feedback loop suggests the CSS should mean-revert — this project tests that hypothesis rigorously.

## Key Findings (Empirical Results: Jan 2022 – Aug 2025, N = 915 Trading Days)

- **Stationarity Confirmed**: In-sample tests confirm the Clean Spark Spread is stationary ($p < 0.01$, ADF stat = -5.76), while gas and carbon exhibit unit roots ($I(1)$), validating the cointegration premise.
- **Cointegration Established**: The **Johansen trace test** confirms **1 cointegrating vector** ($r = 1$, trace statistic $172.1 > 34.9$ critical value), proving a stable multivariate long-run equilibrium among power, gas, and carbon prices. Engle-Granger cross-check also confirms stationarity of regression residuals ($p = 0.01$).
- **Rapid Mean-Reversion**: In-sample AR(1) estimation yields $\beta = 0.6871$ ($p < 0.0001$) and an empirical **half-life of 1.8 trading days**, confirming that spread shocks dissipate rapidly under normal dispatch conditions.
- **In-Sample vs. Out-of-Sample Validation**: To guard against look-ahead bias and overfitting, the dataset is split chronologically into an **In-Sample Crisis Training Period** (Jan 2022 – Dec 2023) and an **Out-of-Sample Normalized Testing Period** (Jan 2024 – Aug 2025):

| Performance Metric | In-Sample (Train: 2022–2023) | Out-of-Sample (Test: 2024–2025) | Full Sample (2022–2025) |
| :--- | :--- | :--- | :--- |
| **Market Regime** | Energy Crisis / Gas Shocks | Market Normalization | Full History |
| **Trading Days** | 499 days | 416 days (unseen data) | 915 days |
| **Strategy Total P&L** | -1,749.64 EUR/MWh | -789.90 EUR/MWh | -2,539.54 EUR/MWh |
| **Strategy Annualized P&L** | -881.46 EUR/MWh/yr | -476.88 EUR/MWh/yr | -695.33 EUR/MWh/yr |
| **Strategy Sharpe Ratio** | -2.24 | -1.56 | -1.96 |
| **Strategy Max Drawdown** | -1,765.44 EUR/MWh | -836.56 EUR/MWh | -2,602.25 EUR/MWh |
| **Strategy Hit Rate** | 25.5% | 28.8% | 26.9% |
| **Strategy Profit Factor** | 0.46 | 0.52 | 0.48 |
| **Baseline Total P&L** | +27.37 EUR/MWh | +90.21 EUR/MWh | +117.59 EUR/MWh |

- **Quantitative Takeaway**: In the out-of-sample normalized regime (2024–2025), drawdowns dropped by over 50% and profit factor improved. This empirical divergence demonstrates why energy desks pair mean-reversion with **volatility-regime switching** and **dynamic stop-losses** to survive structural commodity shocks.
- **Deliverables Produced**: 6 publication-quality charts in `output/plots/`, full statistical test tables in `output/results/`, and a comprehensive standalone research note in `report.html`.

## Project Structure

```
clean-spark-spread/
├── main.R                     # Run the full analysis pipeline
├── report.Rmd                 # R Markdown research report (→ HTML)
├── README.md
├── clean-spark-spread.Rproj   # RStudio project file
│
├── R/                         # Modular R functions
│   ├── 00_config.R            # All constants and parameters
│   ├── 01_data_loaders.R      # Power (ENTSO-E), Gas (Yahoo), Carbon (CSV)
│   ├── 02_data_pipeline.R     # Align, clean, compute CSS
│   ├── 03_stationarity_tests.R # ADF tests
│   ├── 04_cointegration_tests.R # Johansen + Engle-Granger
│   ├── 05_mean_reversion.R    # AR(1) half-life estimation
│   ├── 06_signal.R            # Rolling z-score signal construction
│   ├── 07_backtest.R          # Vectorized backtest engine
│   └── 08_visualization.R     # ggplot2 publication-quality charts
│
├── data/
│   ├── raw/                   # Place input data files here
│   └── processed/             # Generated: cleaned CSS dataset
│
└── output/
    ├── plots/                 # Generated: 6 PNG charts
    └── results/               # Generated: statistical test results
```

## How to Reproduce

### Prerequisites

Install R (≥ 4.1) and the required packages:

```r
install.packages(c(
  "tidyverse", "httr2", "xml2", "quantmod", "zoo",
  "tseries", "urca", "PerformanceAnalytics", "xts",
  "scales", "patchwork", "knitr", "kableExtra", "rmarkdown"
))
```

### Step 1: Obtain Data

You need three data sources. Two require manual downloads:

#### Power Prices (Germany DE-LU)

**Option A — ENTSO-E API** (recommended, requires token):
1. Register at [transparency.entsoe.eu](https://transparency.entsoe.eu)
2. Email transparency@entsoe.eu requesting Web API access (mention your account email)
3. Once granted, find your security token in Account Settings
4. Set the environment variable:
   ```r
   Sys.setenv(ENTSOE_API_KEY = "your-token-here")
   ```

**Option B — ENTSO-E CSV download** (no token needed):
1. Go to [ENTSO-E Transparency Platform](https://transparency.entsoe.eu)
2. Navigate to: Transmission → Day-Ahead Prices
3. Select area: DE-LU (Germany-Luxembourg)
4. Set date range (2022-01-01 to present)
5. Export as CSV
6. Save to `data/raw/power_prices.csv`

#### Gas Prices (TTF)

**Automatic** — downloaded from Yahoo Finance (ticker: `TTF=F`). No action needed.

#### Carbon Prices (EU ETS)

1. Go to [investing.com/commodities/carbon-emissions-historical-data](https://www.investing.com/commodities/carbon-emissions-historical-data)
2. Set the date range to cover 3+ years (e.g., Jan 2022 – present)
3. Click **Download Data**
4. Save the file as `data/raw/carbon_prices.csv`

### Step 2: Run the Analysis

```r
# Open the project in RStudio (recommended)
# Then run:
source("main.R")
```

### Step 3: Generate the Report

```r
rmarkdown::render("report.Rmd")
```

This produces `report.html` — a complete, self-contained research note.

## Methodology

| Step | Method | R Package |
|------|--------|-----------|
| Stationarity | Augmented Dickey-Fuller test | `tseries` |
| Cointegration | Johansen trace + eigenvalue test | `urca` |
| Cross-check | Engle-Granger two-step test | `tseries` |
| Reversion speed | AR(1) half-life estimation | base R `lm()` |
| Signal | Rolling z-score (right-aligned, no look-ahead) | `zoo` |
| Backtest | Vectorized with transaction costs | `tidyverse` |
| Metrics | Sharpe, drawdown, hit rate, Calmar | `PerformanceAnalytics` |

### Why Johansen over Engle-Granger?

The Engle-Granger test is a two-step procedure that tests for cointegration between *two* series. Since the CSS involves *three* price series (power, gas, carbon), the Johansen test is more appropriate — it simultaneously tests for the number of cointegrating relationships in a multivariate system. The Engle-Granger test is included as a simpler cross-check.

## Parameters

All tunable parameters are centralized in `R/00_config.R`:

| Parameter | Value | Justification |
|-----------|-------|---------------|
| Plant efficiency | 50% | IEA benchmark for CCGT |
| CO₂ emissions factor | 0.20 tCO₂/MWh | IPCC 2006 Guidelines |
| Z-score window | 2 × half-life | Empirically grounded |
| Entry threshold | ±1.5σ | Standard in stat-arb |
| Exit threshold | ±0.5σ | Moderate reversion target |
| Transaction cost | 0.50 EUR/MWh | Conservative OTC estimate |

## Limitations & Methodological Safeguards

- **Out-of-Sample Validation**: To address in-sample overfitting, parameters (half-life, cointegration rank, z-score window) were calibrated strictly on the In-Sample crisis training period (2022–2023) and frozen for Out-of-Sample evaluation on 2024–2025 unseen data. For live production, walk-forward expanding window optimization would further enhance adaptability.
- **Volatilty Regime Shifts**: The static ±1.5σ entry rule does not adjust for volatility explosions. Adding a Markov-switching model or dynamic volatility bands would protect against large trend-like drawdowns during supply shocks.
- **Data Proxies**: Front-month TTF futures are used as a proxy for spot gas, and secondary carbon prices from Investing.com were utilized. Physical delivery desks would incorporate physical balancing settlement feeds.
- **Static Heat Rates**: A constant 50% CCGT efficiency is assumed; real-world plants have load-dependent heat rate curves.

## Technology

- **Language**: R (≥ 4.1)
- **Core packages**: tidyverse, urca, tseries, PerformanceAnalytics, quantmod, ggplot2
- **Report**: R Markdown → HTML

## License

This project is for educational and portfolio purposes.
