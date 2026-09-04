##### Data Synchronization Utilities #####
# Ported from manual_verification_tool/R/data_synchronization.R
# Handles file system sync, duplicate resolution, and directory moves

#' Split a filename into its component parts
#' Expected format: "site-parameter_timestamp_hash.parquet" or "site-parameter_FINAL_timestamp_hash.parquet"
#' @param filename Character vector of filenames to parse
#' @return A tibble with columns: filename, site, parameter, datetime, hash, duplicate_alert
split_filename <- function(filename) {

  # Handle vector input
  if (length(filename) > 1) {
    return(map_dfr(filename, split_filename))
  }

  # Remove extension
  base_name <- tools::file_path_sans_ext(filename)

  # Check for FINAL marker
  is_final <- grepl("_FINAL_", base_name)
  if (is_final) {
    base_name <- gsub("_FINAL_", "_", base_name)
  }

  # Split on first hyphen for site
  parts <- strsplit(base_name, "-", fixed = TRUE)[[1]]
  site <- parts[1]

  # Remaining after site- is parameter_timestamp_hash
  remainder <- paste(parts[-1], collapse = "-")

  # Split remainder on underscores
  sub_parts <- strsplit(remainder, "_")[[1]]

  if (length(sub_parts) >= 3) {
    # Last part is hash, second to last + third to last are timestamp
    hash <- sub_parts[length(sub_parts)]
    datetime <- paste(sub_parts[(length(sub_parts) - 2):(length(sub_parts) - 1)], collapse = "_")
    parameter <- paste(sub_parts[1:(length(sub_parts) - 3)], collapse = "_")
  } else {
    # Simplified format without timestamp/hash
    parameter <- paste(sub_parts, collapse = "_")
    datetime <- NA_character_
    hash <- NA_character_
  }

  # Clean parameter: replace underscores with spaces for display
  parameter <- gsub("_", " ", parameter)

  tibble(
    filename = filename,
    site = site,
    parameter = parameter,
    datetime = datetime,
    hash = hash,
    duplicate_alert = NA
  )
}


#' Get list of filenames across all verification directories
#' @param all_path Path to all_data directory
#' @param pre_path Path to pre_verification directory
#' @param int_path Path to intermediary directory
#' @param ver_path Path to verified directory
#' @return A tibble with filename and directory columns
get_filenames <- function(all_path = NULL, pre_path = NULL, int_path = NULL, ver_path = NULL) {

  all_dir_names <- if (!is.null(all_path) && dir.exists(all_path)) {
    list.files(all_path, pattern = "\\.parquet$")
  } else { character(0) }

  pre_dir_names <- if (!is.null(pre_path) && dir.exists(pre_path)) {
    list.files(pre_path, pattern = "\\.parquet$")
  } else { character(0) }

  int_dir_names <- if (!is.null(int_path) && dir.exists(int_path)) {
    list.files(int_path, pattern = "\\.parquet$")
  } else { character(0) }

  ver_dir_names <- if (!is.null(ver_path) && dir.exists(ver_path)) {
    list.files(ver_path, pattern = "\\.parquet$")
  } else { character(0) }

  tibble(
    filename = c(all_dir_names, pre_dir_names, int_dir_names, ver_dir_names),
    directory = c(
      rep("all_data", length(all_dir_names)),
      rep("pre_verification", length(pre_dir_names)),
      rep("intermediary", length(int_dir_names)),
      rep("verified", length(ver_dir_names))
    )
  )
}


#' Sync file system — resolve duplicates and clean directories
#' @param pre_path Path to pre_verification directory
#' @param int_path Path to intermediary directory
#' @param ver_path Path to verified directory
sync_file_system <- function(pre_path, int_path, ver_path) {
  # Check for duplicates in each directory
  for (dir_path in c(pre_path, int_path, ver_path)) {
    if (!dir.exists(dir_path)) next

    files <- list.files(dir_path, pattern = "\\.parquet$")
    if (length(files) == 0) next

    parsed <- split_filename(files) %>%
      mutate(full_file_path = file.path(dir_path, filename))

    # Group by site-parameter to find duplicates
    grouped <- parsed %>%
      group_by(site, parameter) %>%
      filter(n() > 1) %>%
      ungroup()

    if (nrow(grouped) > 0) {
      # Keep the largest/newest file, remove others
      grouped %>%
        group_by(site, parameter) %>%
        arrange(desc(file.size(full_file_path))) %>%
        slice(-1) %>%
        pull(full_file_path) %>%
        file.remove()
    }
  }
}


#' Move file from pre-verification to intermediary directory
#' @param pre_to_int_filename Current filename
#' @param pre_to_int_df Data frame to save
#' @param pre_path Pre-verification directory path
#' @param int_path Intermediary directory path
#' @return New filename
move_file_to_intermediary <- function(pre_to_int_filename, pre_to_int_df, pre_path, int_path) {
  file_meta <- split_filename(pre_to_int_filename)
  site <- file_meta$site
  parameter <- file_meta$parameter

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  data_hash <- digest::digest(pre_to_int_df)
  # Replace spaces with underscores for filename
  param_file <- gsub(" ", "_", parameter)
  new_filename <- glue("{site}-{param_file}_{timestamp}_{data_hash}.parquet")

  tryCatch({
    # Remove existing intermediary files for this site-param
    existing_files <- list.files(int_path, pattern = glue("^{site}-{param_file}"), full.names = TRUE)
    if (length(existing_files) > 0) file.remove(existing_files)

    # Save new version
    write_parquet(pre_to_int_df, file.path(int_path, new_filename))

    # Remove from pre-verification
    pre_files <- list.files(pre_path, pattern = glue("^{site}-{param_file}"), full.names = TRUE)
    if (length(pre_files) > 0) file.remove(pre_files)

    return(new_filename)
  }, error = function(e) {
    if (file.exists(file.path(int_path, new_filename))) {
      file.remove(file.path(int_path, new_filename))
    }
    return(list(success = FALSE, error = e$message))
  })
}


#' Update intermediary data file
#' @param int_df_filename Current filename
#' @param updated_df Updated data frame
#' @param int_path Intermediary directory path
#' @return New filename
update_intermediary_data <- function(int_df_filename, updated_df, int_path) {
  file_meta <- split_filename(int_df_filename)
  site <- file_meta$site
  parameter <- file_meta$parameter

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  data_hash <- digest::digest(updated_df)
  param_file <- gsub(" ", "_", parameter)
  new_filename <- glue("{site}-{param_file}_{timestamp}_{data_hash}.parquet")

  tryCatch({
    # Remove previous versions
    existing_files <- list.files(int_path, pattern = glue("^{site}-{param_file}"), full.names = TRUE)
    if (length(existing_files) > 0) file.remove(existing_files)

    # Save new version
    write_parquet(updated_df, file.path(int_path, new_filename))

    return(new_filename)
  }, error = function(e) {
    return(list(success = FALSE, error = e$message))
  })
}


#' Move file from intermediary to verified directory
#' @param int_to_fin_filename Current filename
#' @param int_to_fin_df Finalized data frame
#' @param int_path Intermediary directory path
#' @param ver_path Verified directory path
#' @return New filename
move_file_to_verified <- function(int_to_fin_filename, int_to_fin_df, int_path, ver_path) {
  # Validate all data is verified
 if (any(int_to_fin_df$verification_status == 'SKIP', na.rm = TRUE) ||
      any(!int_to_fin_df$is_verified, na.rm = TRUE)) {
    stop("Cannot move unverified data to final directory")
  }

  file_meta <- split_filename(int_to_fin_filename)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  data_hash <- digest::digest(int_to_fin_df)
  param_file <- gsub(" ", "_", file_meta$parameter)
  new_filename <- glue("{file_meta$site}-{param_file}_FINAL_{timestamp}_{data_hash}.parquet")

  tryCatch({
    # Remove existing verified files for this site-param
    ver_files <- list.files(ver_path, pattern = glue("^{file_meta$site}-{param_file}"), full.names = TRUE)
    if (length(ver_files) > 0) file.remove(ver_files)

    # Save to verified
    write_parquet(int_to_fin_df, file.path(ver_path, new_filename))

    # Remove from intermediary
    int_files <- list.files(int_path, pattern = glue("^{file_meta$site}-{param_file}"), full.names = TRUE)
    if (length(int_files) > 0) file.remove(int_files)

    return(new_filename)
  }, error = function(e) {
    if (file.exists(file.path(ver_path, new_filename))) {
      file.remove(file.path(ver_path, new_filename))
    }
    return(list(success = FALSE, error = e$message))
  })
}


#' Unfinalize a site-parameter dataset (move from verified back to intermediary)
#' @param site The site name
#' @param parameter The parameter name
#' @param ver_path Verified directory path
#' @param int_path Intermediary directory path
#' @return Path to the new intermediary file
unfinalize_site_parameter <- function(site, parameter, ver_path, int_path) {
  param_file <- gsub(" ", "_", parameter)
  pattern <- paste0("^", site, "-", param_file, "_FINAL_")
  files <- list.files(ver_path, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    stop("No finalized file found for site: ", site, " parameter: ", parameter)
  }

  # Read file
  df <- read_parquet(files[length(files)])  # Use most recent if multiple

  # Set is_finalized to FALSE
  df <- df %>% mutate(is_finalized = FALSE)

  # Build new filename without _FINAL_
  new_filename <- basename(files[length(files)]) %>% str_replace("_FINAL_", "_")
  output_path <- file.path(int_path, new_filename)

  # Save to intermediary directory
  write_parquet(df, output_path)
  file.remove(files)

  message("Unfinalized file saved to: ", output_path)
  invisible(output_path)
}
