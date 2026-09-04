##### Drift Correction Functions #####
# Ported from drift_correction_tool/R/
# Contains: drift math, scenario grid, fallback windows, stitch corrections, drift token removal

#' Remove drift token from a flag string
#' @param flag_str Semi-colon delimited flag string
#' @return Cleaned flag string with drift removed, or NA
remove_drift_token <- function(flag_str) {
  if (is.na(flag_str) || flag_str == "") return(NA_character_)
  tokens <- str_split(flag_str, ";")[[1]] %>% str_replace_all("\\n", " ") %>% str_trim()
  cleaned <- tokens[tokens != "drift" & tokens != ""]
  if (length(cleaned) == 0) return(NA_character_)
  return(paste(cleaned, collapse = "; "))
}


#' Calculate fallback drift windows from flagged data
#' @param df Data frame with drift and mean_analysis columns
#' @return Summary tibble of drift windows
calculate_fallback_windows <- function(df) {
  df %>%
    filter(drift & !is.na(mean_analysis)) %>%
    arrange(DT_round) %>%
    mutate(gap = as.numeric(DT_round - lag(DT_round), units = "days")) %>%
    mutate(window_id = cumsum(if_else(is.na(gap) | gap > 1, 1, 0))) %>%
    group_by(window_id) %>%
    summarise(
      start_dt = min(DT_round),
      end_dt = max(DT_round),
      end_data_val = last(mean_analysis),
      .groups = 'drop'
    ) %>%
    mutate(
      arg_drift_type = "None",
      arg_correction_type = "additive"
    )
}


#' Stitch final corrections from scenario grid and window decisions
#' @param df_scenarios Data frame from generate_scenario_grid
#' @param windows_df Data frame of user-selected window parameters
#' @return Finalized data frame with corrections applied
stitch_final_corrections <- function(df_scenarios, windows_df) {
  df_final <- df_scenarios %>%
    mutate(mean_drift_trans = mean_analysis, correction_type = "raw")

  in_any_verified_window <- rep(FALSE, nrow(df_final))

  for (i in seq_len(nrow(windows_df))) {
    s_dt <- windows_df$start_dt[i]
    e_dt <- windows_df$end_dt[i]
    d_type <- windows_df$arg_drift_type[i]
    corr_type <- windows_df$arg_correction_type[i]

    in_window <- df_final$DT_round >= s_dt & df_final$DT_round <= e_dt
    in_any_verified_window <- in_any_verified_window | in_window

    if (d_type == "non_resolved") {
      df_final$mean_analysis[in_window] <- NA_real_
      df_final$mean_drift_trans[in_window] <- NA_real_
      df_final$correction_type[in_window] <- "non_resolved"
      df_final$verification_status[in_window] <- "OMIT"
      df_final$final_status[in_window] <- "OMIT"
      if ("pre_post_source" %in% names(df_final)) df_final$pre_post_source[in_window] <- NA_real_

    } else if (d_type == "unflag") {
      df_final$mean_drift_trans[in_window] <- df_final$mean_analysis[in_window]
      df_final$correction_type[in_window] <- "raw"
      if ("pre_post_source" %in% names(df_final)) df_final$pre_post_source[in_window] <- NA_real_
      df_final$user_flag[in_window] <- map_chr(df_final$user_flag[in_window], remove_drift_token)
      df_final$drift[in_window] <- FALSE
      df_final$verification_status[in_window] <- if_else(
        is.na(df_final$user_flag[in_window]) & df_final$verification_status[in_window] != "OMIT",
        "PASS", df_final$verification_status[in_window])
      df_final$final_status[in_window] <- if_else(
        is.na(df_final$user_flag[in_window]) & df_final$final_status[in_window] != "OMIT",
        "PASS", df_final$final_status[in_window])

    } else {
      val_column <- case_when(
        d_type == "linear" & corr_type == "multiplicative" ~ "linear_mult",
        d_type == "linear" & corr_type == "additive" ~ "linear_add",
        d_type == "exponential" & corr_type == "multiplicative" ~ "exp_mult",
        d_type == "exponential" & corr_type == "additive" ~ "exp_add",
        d_type == "uniform" & corr_type == "multiplicative" ~ "uniform_mult",
        d_type == "uniform" & corr_type == "additive" ~ "uniform_add",
        d_type == "fitted_linear" & corr_type == "multiplicative" ~ "fitted_mult",
        d_type == "fitted_linear" & corr_type == "additive" ~ "fitted_add",
        TRUE ~ "mean_analysis"
      )

      if (d_type %in% c("linear", "exponential", "uniform", "fitted_linear")) {
        df_final$mean_drift_trans[in_window] <- df_final[[val_column]][in_window]
        df_final$correction_type[in_window] <- d_type
      }
    }
  }

  # Clean up drift flags outside verified windows
  outside_unverified_drift <- df_final$drift & !in_any_verified_window
  if (any(outside_unverified_drift, na.rm = TRUE)) {
    df_final <- df_final %>%
      mutate(
        user_flag = if_else(outside_unverified_drift, map_chr(user_flag, remove_drift_token), user_flag),
        drift = if_else(outside_unverified_drift, FALSE, drift),
        verification_status = if_else(outside_unverified_drift & is.na(user_flag) & verification_status != "OMIT",
                                      "PASS", verification_status),
        final_status = if_else(outside_unverified_drift & is.na(user_flag) & final_status != "OMIT",
                               "PASS", final_status)
      )
  }

  # Remove temporary scenario columns
  df_final <- df_final %>%
    select(-any_of(c("linear_add", "linear_mult", "exp_add", "exp_mult",
                     "uniform_add", "uniform_mult", "fitted_add", "fitted_mult")))

  return(df_final)
}
