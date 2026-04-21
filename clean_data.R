# clean_data.R
# ------------
# Reads raw StatCan CSVs, cleans and filters them, and saves
# analysis-ready CSVs to data/processed/.
#
# Run after fetch_data.R.

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)

# ── Directory setup ────────────────────────────────────────────────────────────
script_dir <- dirname(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE))
if (is.null(script_dir) || script_dir == ".") script_dir <- getwd()

RAW_DIR  <- file.path(script_dir, "raw")
PROC_DIR <- file.path(script_dir, "processed")
dir.create(PROC_DIR, showWarnings = FALSE, recursive = TRUE)

PROVINCES <- c(
  "British Columbia", "Alberta", "Saskatchewan", "Manitoba",
  "Ontario", "Quebec", "New Brunswick", "Nova Scotia",
  "Prince Edward Island", "Newfoundland and Labrador"
)

START_YEAR <- 2014
END_YEAR   <- 2024

# ── Helpers ────────────────────────────────────────────────────────────────────

load_raw <- function(name) {
  path <- file.path(RAW_DIR, paste0(name, ".csv"))
  df   <- read_csv(path, show_col_types = FALSE)
  # Strip whitespace from column names
  names(df) <- str_trim(names(df))
  df
}

inspect <- function(name) {
  df   <- load_raw(name)
  skip <- c("REF_DATE", "GEO", "DGUID", "UOM", "UOM_ID", "SCALAR_FACTOR",
            "SCALAR_ID", "VECTOR", "COORDINATE", "VALUE", "STATUS",
            "SYMBOL", "TERMINATED", "DECIMALS")
  cat(sprintf("\n=== %s columns ===\n", name))
  print(names(df))
  for (col in names(df)) {
    if (!col %in% skip) {
      vals <- head(unique(df[[col]]), 12)
      cat(sprintf("  [%s]: %s\n", col, paste(vals, collapse = ", ")))
    }
  }
}

parse_ref_date <- function(df) {
  df %>%
    mutate(
      REF_DATE = parse_date_time(as.character(REF_DATE), orders = c("Ym", "Y")),
      year     = year(REF_DATE),
      month    = month(REF_DATE)
    )
}

filter_years <- function(df) {
  df %>% filter(year >= START_YEAR, year <= END_YEAR)
}

# ── 1. Employment by industry (14-10-0023-01) ──────────────────────────────────

clean_employment <- function() {
  message("Cleaning employment data...")
  df <- load_raw("employment") %>%
    parse_ref_date() %>%
    filter_years()

  message(sprintf("  Rows after year filter: %s", scales::comma(nrow(df))))

  df <- df %>%
    filter(
      GEO                          == "Canada",
      `Labour force characteristics` == "Employment",
      Gender                       == "Total - Gender",
      `Age group`                  == "15 years and over"
    ) %>%
    select(
      date     = REF_DATE,
      year,
      month,
      industry = `North American Industry Classification System (NAICS)`,
      employed_thousands = VALUE
    ) %>%
    drop_na(employed_thousands)

  if (nrow(df) == 0) {
    message("  WARNING: Empty after filter — run inspect('employment') to debug.")
    return(invisible(NULL))
  }

  annual <- df %>%
    group_by(year, industry) %>%
    summarise(avg_employed_thousands = mean(employed_thousands, na.rm = TRUE),
              .groups = "drop")

  write_csv(df,     file.path(PROC_DIR, "employment_monthly.csv"))
  write_csv(annual, file.path(PROC_DIR, "employment_annual.csv"))
  message(sprintf("  Saved employment_monthly.csv  (%s rows)", scales::comma(nrow(df))))
  message(sprintf("  Saved employment_annual.csv   (%s rows)", scales::comma(nrow(annual))))
}

# ── 2. CPI (18-10-0005-01) ────────────────────────────────────────────────────

clean_cpi <- function() {
  message("Cleaning CPI data...")
  df <- load_raw("cpi") %>%
    parse_ref_date() %>%
    filter_years()

  prod_col        <- "Products and product groups"
  all_products    <- unique(df[[prod_col]])
  shelter_candidates <- all_products[str_detect(str_to_lower(all_products),
                                                "shelter|housing|accommodation")]
  message(sprintf("  Shelter candidates: %s", paste(shelter_candidates, collapse = "; ")))

  if (length(shelter_candidates) == 0) {
    message("  ERROR: No shelter/housing product found.")
    return(invisible(NULL))
  }

  shelter_label <- shelter_candidates[1]
  message(sprintf("  Using shelter label: '%s'", shelter_label))

  df <- df %>%
    filter(
      .data[[prod_col]] %in% c("All-items", shelter_label),
      GEO %in% c("Canada", PROVINCES),
      UOM == "2002=100"
    ) %>%
    select(
      date      = REF_DATE,
      year,
      month,
      geo       = GEO,
      component = all_of(prod_col),
      cpi       = VALUE
    ) %>%
    mutate(component = if_else(component == "All-items", "All-items", "Shelter")) %>%
    drop_na(cpi)

  if (nrow(df) == 0) {
    message("  WARNING: No rows matched CPI filter.")
    return(invisible(NULL))
  }

  annual <- df %>%
    group_by(year, geo, component) %>%
    summarise(avg_cpi = mean(cpi, na.rm = TRUE), .groups = "drop")

  write_csv(df,     file.path(PROC_DIR, "cpi_monthly.csv"))
  write_csv(annual, file.path(PROC_DIR, "cpi_annual.csv"))
  message(sprintf("  Saved cpi_monthly.csv   (%s rows)", scales::comma(nrow(df))))
  message(sprintf("  Saved cpi_annual.csv    (%s rows)", scales::comma(nrow(annual))))
}

# ── 3. Wages (14-10-0064-01) ──────────────────────────────────────────────────

clean_wages <- function() {
  message("Cleaning wages data...")
  df <- load_raw("wages") %>%
    parse_ref_date() %>%
    filter_years()

  wages_vals   <- unique(df$Wages)
  hourly_label <- wages_vals[str_detect(str_to_lower(wages_vals), "average hourly")][1]

  if (is.na(hourly_label)) {
    message("  WARNING: No 'average hourly' label found. Using first available.")
    hourly_label <- wages_vals[1]
  }
  message(sprintf("  Using wages label: '%s'", hourly_label))

  df <- df %>%
    filter(
      GEO %in% c("Canada", PROVINCES),
      Wages == hourly_label,
      `Type of work`  == "Both full- and part-time employees",
      Gender          == "Total - Gender",
      `Age group`     == "15 years and over",
      str_detect(`North American Industry Classification System (NAICS)`,
                 "Total employees, all industries")
    ) %>%
    select(
      date            = REF_DATE,
      year,
      month,
      geo             = GEO,
      avg_hourly_wage = VALUE
    ) %>%
    drop_na(avg_hourly_wage)

  if (nrow(df) == 0) {
    message("  WARNING: No rows matched wages filter.")
    return(invisible(NULL))
  }

  annual <- df %>%
    group_by(year, geo) %>%
    summarise(avg_hourly_wage = mean(avg_hourly_wage, na.rm = TRUE), .groups = "drop")

  write_csv(df,     file.path(PROC_DIR, "wages_monthly.csv"))
  write_csv(annual, file.path(PROC_DIR, "wages_annual.csv"))
  message(sprintf("  Saved wages_monthly.csv   (%s rows)", scales::comma(nrow(df))))
  message(sprintf("  Saved wages_annual.csv    (%s rows)", scales::comma(nrow(annual))))
}

# ── 4. Merged dataset ─────────────────────────────────────────────────────────

build_merged <- function() {
  message("Building merged wages + CPI dataset...")

  wages_path <- file.path(PROC_DIR, "wages_annual.csv")
  cpi_path   <- file.path(PROC_DIR, "cpi_annual.csv")

  if (!file.exists(wages_path) || !file.exists(cpi_path)) {
    message("  SKIP: wages_annual.csv or cpi_annual.csv missing — fix earlier errors first.")
    return(invisible(NULL))
  }

  wages <- read_csv(wages_path, show_col_types = FALSE)
  cpi   <- read_csv(cpi_path,   show_col_types = FALSE)

  # Pivot CPI so All-items and Shelter are separate columns
  cpi_wide <- cpi %>%
    pivot_wider(
      id_cols     = c(year, geo),
      names_from  = component,
      values_from = avg_cpi
    ) %>%
    rename(cpi_all_items = `All-items`, cpi_shelter = Shelter)

  merged <- wages %>%
    inner_join(cpi_wide, by = c("year", "geo"))

  if (nrow(merged) == 0) {
    message("  WARNING: Merged dataset is empty — check that geo values match.")
    return(invisible(NULL))
  }

  # Index everything to START_YEAR = 100
  base <- merged %>%
    filter(year == START_YEAR) %>%
    select(geo, wage_base = avg_hourly_wage,
           cpi_base = cpi_all_items, shelter_base = cpi_shelter)

  merged <- merged %>%
    left_join(base, by = "geo") %>%
    mutate(
      wage_index    = (avg_hourly_wage / wage_base)    * 100,
      cpi_index     = (cpi_all_items   / cpi_base)     * 100,
      shelter_index = (cpi_shelter     / shelter_base) * 100
    ) %>%
    select(-wage_base, -cpi_base, -shelter_base)

  write_csv(merged, file.path(PROC_DIR, "wages_vs_cpi.csv"))
  message(sprintf("  Saved wages_vs_cpi.csv  (%s rows)", scales::comma(nrow(merged))))
}

# ── Main ───────────────────────────────────────────────────────────────────────

args <- commandArgs(trailingOnly = TRUE)

if ("inspect" %in% args) {
  for (name in c("employment", "cpi", "wages")) inspect(name)
} else {
  cat(strrep("=", 50), "\n")
  cat("StatCan Data Cleaner (R)\n")
  cat(strrep("=", 50), "\n")
  clean_employment(); cat("\n")
  clean_cpi();        cat("\n")
  clean_wages();      cat("\n")
  build_merged()
  message("\nDone! Processed files are in data/processed/")
}
