##### Site/Parameter Selector Utilities #####
# Ported from manual_verification_tool/R/selectors.R
# Provides site-to-site mapping for comparison plots and parameter autoselection

#' Get upstream/downstream sites for comparison plotting
#' @param site_arg The current site being viewed
#' @return Character vector of related site names
relevant_sonde_selector <- function(site_arg) {
  site_map <- list(
    joei = c("cbri"),
    cbri = c("joei", "chd"),
    chd = c("cbri", "pfal"),
    pfal = c("chd", "pbr"),
    pbr = c("pfal", "pman"),
    sfm = c("pfal", "pbr"),
    pman = c("pbr", "pbd"),
    pbd = c("pman", "bellvue"),
    bellvue = c("pbd", "salyer"),
    salyer = c("bellvue", "riverbend"),
    udall = c("salyer", "riverbend"),
    riverbend = c("salyer", "cottonwood"),
    riverbend_virridy = c("udall", "riverbend", "cottonwood"),
    springcreek = c("riverbend", "cottonwood"),
    cottonwood = c("riverbend", "cottonwood_virridy", "elc"),
    cottonwood_virridy = c("cottonwood", "elc"),
    elc = c("cottonwood", "archery"),
    boxcreek = c("archery", "archery_virridy"),
    archery = c("elc", "archery_virridy", "riverbluffs"),
    archery_virridy = c("elc", "archery", "riverbluffs"),
    riverbluffs = c("archery", "archery_virridy"),
    mtncampus = c("sfm")
  )

  plot_filter <- site_map[[site_arg]]
  if (is.null(plot_filter)) return(character(0))
  return(plot_filter)
}


#' Get auto-selected sub-parameters for a main parameter
#' @param parameter The main parameter name
#' @param meta_path Path to meta directory containing parameter_autoselections.csv
#' @return Character vector of sub-parameter names
get_auto_parameters <- function(parameter, meta_path) {
  tryCatch({
    read_csv(file.path(meta_path, "parameter_autoselections.csv"), show_col_types = FALSE) %>%
      filter(main_parameter == parameter) %>%
      pull(sub_parameters) %>%
      first() %>%
      str_split(",", simplify = TRUE) %>%
      as.character() %>%
      str_trim() %>%
      .[. != ""]
  }, error = function(e) character(0))
}


#' Determine which data source directory a site-parameter combo is in
#' @param df_name The site-parameter name (e.g., "archery-Temperature")
#' @param year_week The year-week string (e.g., "2025 - 13")
#' @param pre_data Pre-verification data list
#' @param int_data Intermediary data list
#' @param ver_data Verified data list
#' @return Character string: "verified_data", "intermediary_data", or "pre_verification_data"
retrieve_relevant_data_name <- function(df_name, year_week = NULL,
                                        pre_data = NULL, int_data = NULL, ver_data = NULL) {
  if (!is.null(ver_data) && df_name %in% names(ver_data) &&
      any(year_week %in% ver_data[[df_name]]$y_w)) {
    return("verified_data")
  }
  if (!is.null(int_data) && df_name %in% names(int_data) &&
      any(year_week %in% int_data[[df_name]]$y_w)) {
    return("intermediary_data")
  }
  if (!is.null(pre_data) && df_name %in% names(pre_data) &&
      any(year_week %in% pre_data[[df_name]]$y_w)) {
    return("pre_verification_data")
  }
  return(NULL)
}
