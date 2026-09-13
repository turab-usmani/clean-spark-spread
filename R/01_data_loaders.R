# ==============================================================================
# 01_data_loaders.R — Data Loading Functions
# Clean Spark Spread Statistical Arbitrage Engine
# ==============================================================================
# Functions to load power, gas, and carbon price data from multiple sources.
# Each loader returns a tidy tibble with columns: date, price.
# ==============================================================================

library(tidyverse)
library(httr2)
library(xml2)
library(quantmod)
library(zoo)

# ------------------------------------------------------------------------------
# POWER PRICES — ENTSO-E Transparency Platform
# ------------------------------------------------------------------------------

#' Load power prices from ENTSO-E REST API
#'
#' Fetches day-ahead prices for a given bidding zone.
#' The API limits each request to ~1 year, so we chunk the date range.
#'
#' @param api_key Character. ENTSO-E security token.
#' @param area_code Character. EIC code for the bidding zone (default: DE-LU).
#' @param start_date Date. Start of the period.
#' @param end_date Date. End of the period.
#' @return A tibble with columns: date (Date), power_price_eur_mwh (numeric).
load_power_prices_api <- function(api_key,
                                   area_code = ENTSOE_AREA_CODE,
                                   start_date = DATE_START,
                                   end_date = DATE_END) {

  if (nchar(api_key) == 0) {
    stop("ENTSO-E API key is not set. Set it via Sys.setenv(ENTSOE_API_KEY = 'your-key')
         or provide it as an argument. See README.md for instructions on obtaining a key.")
  }

  cat(">> Fetching power prices from ENTSO-E API...\n")

  # ENTSO-E API requires UTC timestamps in format: YYYYMMDDHHmm
  # and limits to ~1 year per request, so we chunk by year
  all_data <- tibble()

  # Create yearly chunks
  chunk_starts <- seq.Date(start_date, end_date, by = "365 days")
  chunk_ends   <- c(chunk_starts[-1] - 1, end_date)

  for (i in seq_along(chunk_starts)) {
    s <- format(chunk_starts[i], "%Y%m%d0000")
    e <- format(chunk_ends[i] + 1, "%Y%m%d0000")  # end is exclusive

    cat(sprintf("   Chunk %d/%d: %s to %s\n", i, length(chunk_starts),
                chunk_starts[i], chunk_ends[i]))

    tryCatch({
      resp <- request("https://web-api.tp.entsoe.eu/api") |>
        req_url_query(
          securityToken = api_key,
          documentType  = "A44",           # Day-ahead prices
          in_Domain     = area_code,
          out_Domain    = area_code,
          periodStart   = s,
          periodEnd     = e
        ) |>
        req_retry(max_tries = 3, backoff = ~5) |>
        req_perform()

      # Parse XML response
      xml_body <- resp_body_xml(resp)
      ns <- xml_ns(xml_body)

      # Extract all TimeSeries > Period > Point elements
      points <- xml_find_all(xml_body, ".//d1:Point", ns = c(d1 = "urn:iec62325.351:tc57wg16:451-3:publicationdocument:7:3"))

      if (length(points) == 0) {
        # Try without namespace
        points <- xml_find_all(xml_body, "//Point")
      }

      if (length(points) > 0) {
        # Extract periods to get start timestamps
        periods <- xml_find_all(xml_body, ".//d1:Period",
                                ns = c(d1 = "urn:iec62325.351:tc57wg16:451-3:publicationdocument:7:3"))
        if (length(periods) == 0) {
          periods <- xml_find_all(xml_body, "//Period")
        }

        chunk_data <- tibble()
        for (period in periods) {
          period_start_text <- xml_text(xml_find_first(period, ".//d1:start",
                                                        ns = c(d1 = "urn:iec62325.351:tc57wg16:451-3:publicationdocument:7:3")))
          if (is.na(period_start_text)) {
            period_start_text <- xml_text(xml_find_first(period, ".//start"))
          }

          period_start <- as.POSIXct(period_start_text, format = "%Y-%m-%dT%H:%MZ", tz = "UTC")

          period_points <- xml_find_all(period, ".//d1:Point",
                                         ns = c(d1 = "urn:iec62325.351:tc57wg16:451-3:publicationdocument:7:3"))
          if (length(period_points) == 0) {
            period_points <- xml_find_all(period, ".//Point")
          }

          for (pt in period_points) {
            pos_text <- xml_text(xml_find_first(pt, ".//d1:position",
                                                 ns = c(d1 = "urn:iec62325.351:tc57wg16:451-3:publicationdocument:7:3")))
            price_text <- xml_text(xml_find_first(pt, ".//d1:price.amount",
                                                   ns = c(d1 = "urn:iec62325.351:tc57wg16:451-3:publicationdocument:7:3")))
            if (is.na(pos_text)) {
              pos_text <- xml_text(xml_find_first(pt, ".//position"))
              price_text <- xml_text(xml_find_first(pt, ".//price.amount"))
            }

            pos <- as.integer(pos_text)
            price <- as.numeric(price_text)
            timestamp <- period_start + (pos - 1) * 3600  # hourly resolution

            chunk_data <- bind_rows(chunk_data, tibble(
              datetime = timestamp,
              price    = price
            ))
          }
        }

        all_data <- bind_rows(all_data, chunk_data)
      }

      Sys.sleep(1)  # Rate limiting: be polite to the API

    }, error = function(e) {
      warning(sprintf("Failed to fetch chunk %d: %s", i, e$message))
    })
  }

  if (nrow(all_data) == 0) {
    stop("No power price data retrieved from ENTSO-E API. Check your API key and date range.")
  }

  # Aggregate hourly prices to daily mean
  result <- all_data |>
    mutate(date = as.Date(datetime)) |>
    group_by(date) |>
    summarise(
      power_price_eur_mwh = mean(price, na.rm = TRUE),
      n_hours = n(),
      .groups = "drop"
    ) |>
    filter(date >= start_date, date <= end_date) |>
    select(date, power_price_eur_mwh)

  cat(sprintf(">> Power prices loaded: %d daily observations (%s to %s)\n",
              nrow(result), min(result$date), max(result$date)))

  return(result)
}


#' Load power prices from CSV (ENTSO-E web export, SMARD export, or custom format)
#'
#' Expects a CSV with at minimum a date column and a price column.
#' Supports ENTSO-E web export format, SMARD.de export format, and custom formats.
#'
#' @param filepath Character. Path to the CSV file.
#' @return A tibble with columns: date (Date), power_price_eur_mwh (numeric).
load_power_prices_csv <- function(filepath = file.path(PATH_RAW_DATA, POWER_CSV_FILENAME)) {

  if (!file.exists(filepath)) {
    # Check if file was saved with double extension on Windows (e.g. .csv.csv)
    alt_filepath <- paste0(filepath, ".csv")
    if (file.exists(alt_filepath)) {
      filepath <- alt_filepath
    } else {
      stop(sprintf("Power price CSV not found at: %s\nSee README.md for download instructions.", filepath))
    }
  }

  cat(sprintf(">> Loading power prices from CSV: %s\n", filepath))

  # Auto-detect delimiter (semicolon for SMARD/German exports, comma for ENTSO-E)
  first_line <- readLines(filepath, n = 1, warn = FALSE)
  delim <- if (grepl(";", first_line)) ";" else if (grepl("\t", first_line)) "\t" else ","

  raw <- read_delim(filepath, delim = delim, show_col_types = FALSE)

  # Try to auto-detect format
  col_names_lower <- tolower(names(raw))

  # Look for date column (including start date, date, mtu, day, datum)
  date_idx <- which(grepl("date|datum|mtu|day|start", col_names_lower))[1]
  date_col <- if (!is.na(date_idx)) names(raw)[date_idx] else NA

  # Look for price column (including germany/luxembourg, de/lu, price, day-ahead, wert)
  price_idx <- which(grepl("germany|de/lu|de-lu|price|preis|day-ahead|power_price|mwh", col_names_lower) &
                     !grepl("date|datum|neighbour|export|import", col_names_lower))[1]
  price_col <- if (!is.na(price_idx)) names(raw)[price_idx] else NA

  if (is.na(date_col) || is.na(price_col)) {
    # Fallback: assume first column is date, 3rd if 2nd is end date, else 2nd
    date_col <- names(raw)[1]
    price_col <- if (ncol(raw) >= 3 && grepl("end|bis", tolower(names(raw)[2]))) names(raw)[3] else names(raw)[2]
    cat("   Warning: Could not auto-detect columns. Using fallback columns.\n")
  }

  cat(sprintf("   Using columns: date='%s', price='%s'\n", date_col, price_col))

  result <- raw |>
    transmute(
      date_raw = !!sym(date_col),
      price_raw = !!sym(price_col)
    ) |>
    mutate(
      price_clean = gsub(",", ".", gsub("[^0-9,.-]", "", as.character(price_raw))),
      price_clean = ifelse(price_clean %in% c("", "-", "."), NA_character_, price_clean),
      power_price_eur_mwh = as.numeric(price_clean),
      date = coalesce(
        as.Date(date_raw, format = "%b %d, %Y"),   # "Jan 1, 2022" (SMARD English)
        as.Date(date_raw, format = "%B %d, %Y"),   # "January 1, 2022"
        as.Date(date_raw, format = "%d.%m.%Y"),   # "01.01.2022" (SMARD German)
        as.Date(date_raw, format = "%Y-%m-%d"),   # ISO format
        as.Date(date_raw, format = "%m/%d/%Y"),   # US format
        as.Date(date_raw, format = "%d/%m/%Y")    # EU format
      )
    ) |>
    filter(!is.na(date), !is.na(power_price_eur_mwh)) |>
    group_by(date) |>
    summarise(power_price_eur_mwh = mean(power_price_eur_mwh, na.rm = TRUE),
              .groups = "drop") |>
    arrange(date)

  cat(sprintf(">> Power prices loaded: %d daily observations (%s to %s)\n",
              nrow(result), min(result$date), max(result$date)))

  return(result)
}


# ------------------------------------------------------------------------------
# GAS PRICES — Yahoo Finance (Dutch TTF Natural Gas Futures)
# ------------------------------------------------------------------------------

#' Load TTF gas prices from Yahoo Finance
#'
#' Uses quantmod to download Dutch TTF Natural Gas Futures (front-month).
#' Ticker: TTF=F on Yahoo Finance.
#'
#' Note: This is the front-month futures price, not the spot price.
#' For the CSS calculation this is an acceptable proxy, as TTF futures
#' are the most liquid European gas benchmark and closely track spot.
#'
#' @param start_date Date. Start of the period.
#' @param end_date Date. End of the period.
#' @return A tibble with columns: date (Date), gas_price_eur_mwh (numeric).
load_gas_prices <- function(start_date = DATE_START, end_date = DATE_END) {

  cat(">> Fetching gas prices from Yahoo Finance (TTF=F)...\n")

  # Suppress quantmod's verbose output
  tryCatch({
    suppressWarnings({
      getSymbols("TTF=F",
                 src = "yahoo",
                 from = start_date,
                 to = end_date + 1,  # Yahoo's 'to' is exclusive
                 auto.assign = TRUE,
                 verbose = FALSE)
    })
  }, error = function(e) {
    stop(sprintf("Failed to download TTF gas prices from Yahoo Finance: %s\n
                  Check your internet connection and whether Yahoo Finance is accessible.", e$message))
  })

  # quantmod stores data in an xts object with a mangled name
  ttf_data <- get("TTF=F")

  result <- tibble(
    date = index(ttf_data) |> as.Date(),
    gas_price_eur_mwh = as.numeric(Cl(ttf_data))  # Use closing price
  ) |>
    filter(!is.na(gas_price_eur_mwh),
           date >= start_date,
           date <= end_date) |>
    arrange(date)

  cat(sprintf(">> Gas prices loaded: %d daily observations (%s to %s)\n",
              nrow(result), min(result$date), max(result$date)))

  return(result)
}


# ------------------------------------------------------------------------------
# CARBON PRICES — CSV (Investing.com or similar)
# ------------------------------------------------------------------------------

#' Load EU ETS carbon (EUA) prices from Investing.com CSV export
#'
#' Investing.com CSV format has columns: "Date","Price","Open","High","Low","Vol.","Change %"
#' Date format: "MM/dd/yyyy" (US style), Price uses comma as thousands separator.
#'
#' Also supports a simple two-column CSV (date, price).
#'
#' @param filepath Character. Path to the CSV file.
#' @return A tibble with columns: date (Date), carbon_price_eur_t (numeric).
load_carbon_prices <- function(filepath = file.path(PATH_RAW_DATA, CARBON_CSV_FILENAME)) {

  if (!file.exists(filepath)) {
    alt_filepath <- paste0(filepath, ".csv")
    if (file.exists(alt_filepath)) {
      filepath <- alt_filepath
    } else {
      stop(sprintf("Carbon price CSV not found at: %s
    
Download instructions:
  1. Go to https://www.investing.com/commodities/carbon-emissions-historical-data
  2. Set date range to cover 3+ years
  3. Click 'Download Data'
  4. Save the file as: %s

See README.md for detailed instructions.", filepath, filepath))
    }
  }

  cat(sprintf(">> Loading carbon prices from CSV: %s\n", filepath))

  raw <- read_csv(filepath, show_col_types = FALSE)
  col_names_lower <- tolower(names(raw))

  # Detect Investing.com format
  if ("price" %in% col_names_lower && "date" %in% col_names_lower) {
    date_col <- names(raw)[which(col_names_lower == "date")[1]]
    price_col <- names(raw)[which(col_names_lower == "price")[1]]
  } else {
    # Fallback: first column = date, second = price
    cat("   Warning: Could not auto-detect columns. Using first two columns.\n")
    date_col <- names(raw)[1]
    price_col <- names(raw)[2]
  }

  cat(sprintf("   Using columns: date='%s', price='%s'\n", date_col, price_col))

  result <- raw |>
    transmute(
      date_raw = !!sym(date_col),
      price_raw = !!sym(price_col)
    ) |>
    mutate(
      # Remove commas from numbers (Investing.com uses "1,234.56" format)
      price_clean = gsub(",", "", as.character(price_raw)),
      carbon_price_eur_t = as.numeric(price_clean),
      # Try multiple date formats
      date = coalesce(
        as.Date(date_raw, format = "%m/%d/%Y"),   # US format (Investing.com)
        as.Date(date_raw, format = "%Y-%m-%d"),    # ISO format
        as.Date(date_raw, format = "%d/%m/%Y"),    # European format
        as.Date(date_raw, format = "%d-%m-%Y"),    # European with dashes
        as.Date(date_raw, format = "%b %d, %Y")    # "Jan 01, 2022" format
      )
    ) |>
    filter(!is.na(date), !is.na(carbon_price_eur_t)) |>
    select(date, carbon_price_eur_t) |>
    arrange(date)

  cat(sprintf(">> Carbon prices loaded: %d daily observations (%s to %s)\n",
              nrow(result), min(result$date), max(result$date)))

  return(result)
}


# ------------------------------------------------------------------------------
# MASTER LOADER — Loads all data with appropriate source selection
# ------------------------------------------------------------------------------

#' Load all three price series
#'
#' Automatically selects the best available source for each series.
#' Power: API if ENTSO-E key available, otherwise CSV.
#' Gas: Yahoo Finance (always).
#' Carbon: CSV (always).
#'
#' @return A list with three tibbles: power, gas, carbon.
load_all_prices <- function() {

  cat("============================================================\n")
  cat("LOADING PRICE DATA\n")
  cat("============================================================\n\n")

  # --- Power ---
  if (nchar(ENTSOE_API_KEY) > 0) {
    cat(">> ENTSO-E API key detected — using API for power prices.\n")
    power <- load_power_prices_api(api_key = ENTSOE_API_KEY)
  } else if (file.exists(file.path(PATH_RAW_DATA, POWER_CSV_FILENAME))) {
    cat(">> No ENTSO-E API key — loading power prices from CSV.\n")
    power <- load_power_prices_csv()
  } else {
    stop("No power price data available.
         Either set ENTSOE_API_KEY environment variable,
         or place a CSV file at data/raw/power_prices.csv.
         See README.md for instructions.")
  }

  cat("\n")

  # --- Gas ---
  gas <- load_gas_prices()

  cat("\n")

  # --- Carbon ---
  carbon <- load_carbon_prices()

  cat("\n============================================================\n")
  cat("ALL DATA LOADED SUCCESSFULLY\n")
  cat("============================================================\n")

  return(list(power = power, gas = gas, carbon = carbon))
}
