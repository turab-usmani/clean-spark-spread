# ==============================================================================
# 00_config.R — Central Configuration & Constants
# Clean Spark Spread Statistical Arbitrage Engine
# ==============================================================================
# All tunable parameters and physical constants are defined here.
# This is the ONLY file that needs modification to change assumptions.
# ==============================================================================

# --- Plant Physical Constants ------------------------------------------------

# Combined Cycle Gas Turbine (CCGT) thermal efficiency
# Source: IEA World Energy Outlook 2023, typical new-build CCGT efficiency
# Range in practice: 45-62% (modern CCGTs). We use 50% as a conservative,
# widely-cited benchmark value.
PLANT_EFFICIENCY <- 0.50  # dimensionless (MWh_electric / MWh_thermal)

# CO2 emissions factor for natural gas combustion in a CCGT
# Source: IPCC 2006 Guidelines for National Greenhouse Gas Inventories,
#         Volume 2, Chapter 2, Table 2.2 (natural gas default emission factor)
# Natural gas: ~56.1 kg CO2/GJ = ~0.202 tCO2/MWh_thermal
# For a 50% efficient plant: emissions per MWh_electric = 0.202 / 0.50 = 0.404 tCO2/MWh_e
# However, the CSS formula uses emissions_factor * carbon_price directly,
# where emissions_factor is in tCO2 per MWh_electric output.
# We use the THERMAL emission factor here because the formula already divides
# gas price by efficiency separately.
EMISSIONS_FACTOR <- 0.20  # tCO2 per MWh_thermal (natural gas)

# --- Date Range ---------------------------------------------------------------

# Target date range for analysis
# Aim for 3+ years to have sufficient observations for meaningful statistical testing
DATE_START <- as.Date("2022-01-01")
DATE_END   <- as.Date("2025-08-31")

# Chronological Train / Test (In-Sample / Out-of-Sample) split date
# Training (In-Sample): 2022-01-01 to 2023-12-31 (~70% of sample, crisis period)
# Testing (Out-of-Sample): 2024-01-01 to 2025-08-31 (~30% of sample, post-crisis normalization)
TRAIN_TEST_SPLIT_DATE <- as.Date("2024-01-01")

# --- ENTSO-E API Configuration -----------------------------------------------

# Germany-Luxembourg bidding zone (DE-LU) area code
# Source: ENTSO-E area code list
# https://transparency.entsoe.eu/content/static_content/Static%20content/
# web%20api/Guide.html#_areas
ENTSOE_AREA_CODE <- "10Y1001A1001A82H"

# API token: read from environment variable for security
# Set via: Sys.setenv(ENTSOE_API_KEY = "your-token-here") or in .Renviron
ENTSOE_API_KEY <- Sys.getenv("ENTSOE_API_KEY", unset = "")

# --- Signal Construction Parameters ------------------------------------------

# Rolling z-score window length (in trading days)
# This will be updated after half-life estimation in step 5.
# Initial default: 60 days (~3 months), a common choice for energy spreads.
# After AR(1) estimation, we set window = 2 * half_life (rounded).
ZSCORE_WINDOW <- 60L

# Entry and exit thresholds for z-score signal
# Entry at +/- 1.5 sigma: captures moves ~2x the typical standard deviation,
# balancing signal frequency against quality. Common in stat-arb literature.
ENTRY_THRESHOLD <- 1.5
EXIT_THRESHOLD  <- 0.5  # exit when z-score reverts toward zero

# --- Backtest Parameters ------------------------------------------------------

# Transaction cost per round-trip trade (EUR/MWh)
# This represents the combined bid-ask spread + slippage for entering and
# exiting a position in the CSS (i.e., trading power, gas, and carbon legs).
# Justification: In OTC European energy markets, bid-ask spreads for
# day-ahead power are ~0.10-0.25 EUR/MWh, TTF gas ~0.05-0.15, and
# EUA carbon ~0.10-0.20. Combined with slippage, 0.50 EUR/MWh per
# round-trip is a conservative but realistic estimate.
TRANSACTION_COST <- 0.50  # EUR/MWh per round-trip

# --- Data Source Paths --------------------------------------------------------

# Relative paths (from project root)
PATH_RAW_DATA       <- "data/raw"
PATH_PROCESSED_DATA <- "data/processed"
PATH_PLOTS          <- "output/plots"
PATH_RESULTS        <- "output/results"

# Expected CSV filenames in data/raw/
POWER_CSV_FILENAME  <- "power_prices.csv"
CARBON_CSV_FILENAME <- "carbon_prices.csv"

# Processed output
CSS_DAILY_FILENAME  <- "css_daily.csv"

# --- Plotting Theme Constants ------------------------------------------------

# Color palette for visualizations
COLOR_STRATEGY  <- "#1a5276"  # deep blue
COLOR_BASELINE  <- "#95a5a6"  # grey
COLOR_LONG      <- "#27ae60"  # green (long entry)
COLOR_SHORT     <- "#e74c3c"  # red (short entry)
COLOR_EXIT      <- "#7f8c8d"  # dark grey (exit)
COLOR_BAND_FILL <- "#d5e8f0"  # light blue (z-score bands)
COLOR_CSS_LINE  <- "#2c3e50"  # dark navy (CSS line)
COLOR_POWER     <- "#e67e22"  # orange (power price)
COLOR_GAS       <- "#3498db"  # blue (gas price)
COLOR_CARBON    <- "#2ecc71"  # green (carbon price)

cat(">> Configuration loaded successfully.\n")
