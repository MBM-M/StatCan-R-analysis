# fetch_data.R
# ------------
# Downloads raw CSVs from the StatCan Web Data Service and saves them locally.
# Run this once before clean_data.R and 01_analysis.Rmd.
#
# Tables used:
#   14-10-0023-01  Employment by industry (seasonally adjusted)
#   18-10-0005-01  CPI - All-items + shelter component, by province
#   14-10-0064-01  Average hourly wages by province and job type

library(httr)
library(jsonlite)

# ── Directory setup ────────────────────────────────────────────────────────────
script_dir <- dirname(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE))
if (is.null(script_dir) || script_dir == ".") script_dir <- getwd()

RAW_DIR <- file.path(script_dir, "raw")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

# ── StatCan WDS endpoint ───────────────────────────────────────────────────────
WDS_URL <- "https://www150.statcan.gc.ca/t1/wds/rest/getFullTableDownloadCSV/{pid}/en"

TABLES <- list(
  employment = "14100023",   # 14-10-0023-01 Employment by industry
  cpi        = "18100005",   # 18-10-0005-01 CPI all-items + shelter
  wages      = "14100064"    # 14-10-0064-01 Average hourly wages by province
)

# ── Download helper ────────────────────────────────────────────────────────────
download_table <- function(name, pid) {
  out_path <- file.path(RAW_DIR, paste0(name, ".csv"))

  if (file.exists(out_path)) {
    message(sprintf("  [skip] %s.csv already exists", name))
    return(invisible(NULL))
  }

  message(sprintf("  Downloading %s (pid=%s) ...", name, pid), appendLF = FALSE)

  # Step 1: get the actual ZIP download URL from the WDS API
  api_url  <- gsub("\\{pid\\}", pid, WDS_URL)
  api_resp <- GET(api_url, timeout(30))
  stop_for_status(api_resp)
  zip_url  <- content(api_resp, as = "parsed", type = "application/json")$object

  # Step 2: download the ZIP into a temp file
  tmp_zip <- tempfile(fileext = ".zip")
  GET(zip_url, write_disk(tmp_zip, overwrite = TRUE), timeout(300))

  # Step 3: extract the data CSV (not the MetaData one)
  zip_contents <- unzip(tmp_zip, list = TRUE)$Name
  csv_name     <- zip_contents[grepl("\\.csv$", zip_contents) & !grepl("MetaData", zip_contents)][1]

  tmp_dir <- tempdir()
  unzip(tmp_zip, files = csv_name, exdir = tmp_dir, overwrite = TRUE)
  file.copy(file.path(tmp_dir, csv_name), out_path, overwrite = TRUE)

  size_mb <- file.info(out_path)$size / 1048576
  message(sprintf(" done (%.1f MB)", size_mb))

  unlink(tmp_zip)
  invisible(NULL)
}

# ── Main ───────────────────────────────────────────────────────────────────────
message("Fetching StatCan datasets...\n")
for (name in names(TABLES)) {
  download_table(name, TABLES[[name]])
}
message("\nAll datasets saved to data/raw/")
