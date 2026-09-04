##### Setup Directories Utility #####
# Ported from manual_verification_tool/R/setup_directories_from_upload.R
# Handles creating directory structure and parsing uploaded raw data

#' Create the full directory structure needed for the verification pipeline
#' @param base_path Base path for the year cycle
#' @return List of directory paths created
create_pipeline_directories <- function(base_path) {

  dirs <- list(
    in_progress = file.path(base_path, "in_progress"),
    all_data = file.path(base_path, "in_progress", "all_data_directory"),
    pre_verification = file.path(base_path, "in_progress", "pre_verification_directory"),
    intermediary = file.path(base_path, "in_progress", "intermediary_directory"),
    verified = file.path(base_path, "in_progress", "verified_directory"),
    raw_data = file.path(base_path, "in_progress", "raw_data"),
    meta = file.path(base_path, "in_progress", "meta"),
    back_calibration = file.path(base_path, "hydro_vu_pull", "back_calibration"),
    flagged_data = file.path(base_path, "hydro_vu_pull", "flagged_data"),
    flagged_temp = file.path(base_path, "hydro_vu_pull", "flagged_data_temp"),
    raw_api = file.path(base_path, "hydro_vu_pull", "raw_data"),
    post_verification = file.path(base_path, "post_verification")
  )

  # Create all directories
  walk(dirs, ~ if (!dir.exists(.x)) dir.create(.x, recursive = TRUE))

  return(dirs)
}


#' Parse a raw uploaded data file and validate columns
#' @param file_path Path to the uploaded file
#' @param timezone Timezone string for datetime parsing
#' @return A parsed data frame, or a character error message
parse_raw_file <- function(file_path, timezone = "UTC") {
  file_type <- tools::file_ext(file_path)

  raw_data <- tryCatch({
    switch(file_type,
      "feather" = arrow::read_feather(file_path),
      "csv" = readr::read_csv(file_path, show_col_types = FALSE),
      "rds" = readr::read_rds(file_path),
      "xlsx" = readxl::read_xlsx(file_path),
      "parquet" = arrow::read_parquet(file_path),
      stop(paste0("File type ", file_type, " not supported. Please upload a .feather, .csv, .rds, .xlsx, or .parquet file."))
    )
  }, error = function(e) {
    return(paste0("Error reading file: ", e$message))
  })

  if (is.character(raw_data)) return(raw_data)

  # Validate required columns
  required_cols <- c("site", "parameter", "DT_round", "mean")
  if (!all(required_cols %in% colnames(raw_data))) {
    missing_cols <- paste(setdiff(required_cols, colnames(raw_data)), collapse = ", ")
    return(paste0("Missing required columns: ", missing_cols))
  }

  # Parse datetime and add missing columns
  raw_data_parsed <- raw_data %>%
    mutate(DT_round = anytime::anytime(DT_round, tz = timezone)) %>%
    ross.wq.tools::add_column_if_not_exists("flag", NA) %>%
    ross.wq.tools::add_column_if_not_exists("auto_flag", NA) %>%
    ross.wq.tools::add_column_if_not_exists("mal_flag", NA) %>%
    ross.wq.tools::add_column_if_not_exists("units", NA_character_) %>%
    ross.wq.tools::add_column_if_not_exists("n_obs", NA_real_) %>%
    ross.wq.tools::add_column_if_not_exists("spread", NA_real_) %>%
    ross.wq.tools::add_column_if_not_exists("DT_join", as.character(DT_round)) %>%
    ross.wq.tools::add_column_if_not_exists("mean_pre_cal", mean)

  return(raw_data_parsed)
}


#' Process uploaded data into the verification directory structure
#' @param uploaded_file_paths Vector of uploaded file paths
#' @param timezone Timezone for datetime parsing
#' @param all_path Path to all_data directory
#' @param pre_path Path to pre_verification directory
#' @param raw_path Path to raw_data directory
#' @param meta_path Path to meta directory
#' @param year Year for season calculation
#' @return Status message
setup_directories_from_upload <- function(uploaded_file_paths, timezone, all_path,
                                          pre_path, raw_path, meta_path, year = NULL) {
  # Parse all uploaded files
  all_data_parsed <- map(uploaded_file_paths, ~ parse_raw_file(.x, timezone)) %>%
    keep(is.data.frame) %>%
    bind_rows()

  if (nrow(all_data_parsed) == 0) {
    return("No valid data found in uploaded files")
  }

  if (is.null(year)) year <- year(min(all_data_parsed$DT_round, na.rm = TRUE))

  # Split into site-parameter combinations
  site_params <- expand_grid(
    site = unique(all_data_parsed$site),
    parameter = unique(all_data_parsed$parameter)
  )

  flagged_list <- pmap(site_params, ~ all_data_parsed %>% filter(site == ..1, parameter == ..2))
  names(flagged_list) <- paste(site_params$site, site_params$parameter, sep = "-")
  flagged_list <- keep(flagged_list, ~ nrow(.x) > 0)

  # Add verification columns
  pre_processed_data <- map(.x = flagged_list, \(x)
    x %>%
      mutate(
        mean_verified = mean,
        is_verified = FALSE,
        verification_status = NA,
        year = year(DT_round),
        week = week(DT_round),
        weekday = lubridate::wday(DT_round, week_start = 7),
        y_w = paste(year, "-", week),
        day = yday(DT_round),
        y_d = paste(year, "-", day),
        season = case_when(
          month(DT_round) %in% c(12, 1, 2, 3, 4) ~ "winter_baseflow",
          month(DT_round) %in% c(5, 6) ~ "snowmelt",
          month(DT_round) %in% c(7, 8, 9) ~ "monsoon",
          month(DT_round) %in% c(10, 11) ~ "fall_baseflow"
        ),
        flag = case_when(
          !is.na(auto_flag) & !is.na(mal_flag) ~ paste(auto_flag, mal_flag, sep = ";"),
          !is.na(auto_flag) & is.na(mal_flag) ~ auto_flag,
          !is.na(mal_flag) & is.na(auto_flag) ~ mal_flag
        ),
        user_flag = flag,
        brush_omit = FALSE,
        user = NA,
        final_status = NA,
        week_decision = NA,
        is_finalized = FALSE
      )
  )

  # Save to all_data and pre_verification directories
  iwalk(pre_processed_data, \(x, idx) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    data_hash <- digest::digest(x)
    new_filename <- glue("{idx}_{timestamp}_{data_hash}.parquet")
    write_parquet(x, file.path(all_path, new_filename))
  })

  R.utils::copyDirectory(all_path, pre_path)
  return("Files saved to all and pre directory")
}
