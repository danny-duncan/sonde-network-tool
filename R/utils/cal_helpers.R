##### Calibration Helper Functions #####
# Ported from calibration_correction_tool/R/

#' Generate calibration comparison plot
#' @param df Calibration data for a site-parameter
#' @param field_notes_data Field notes data frame
#' @return A plotly object
cal_plot <- function(df, field_notes_data = NULL) {
  parameter <- unique(df$parameter)
  site <- unique(df$site)
  year <- lubridate::year(df$DT_round)[1]

  combined_data <- bind_rows(
    df %>%
      select(DT_round, value = mean, correct_calibration) %>%
      mutate(data_type = ifelse(correct_calibration,
                                "Original Data (Good Calibration)",
                                "Original Data (Bad Calibration)")),
    df %>%
      select(DT_round, value = mean_cal) %>%
      mutate(data_type = "Back-calibrated Data")
  )

  if (parameter == "Turbidity") {
    combined_data <- combined_data %>%
      mutate(value = ifelse(value > 200, 200, value))
  }

  vline_df <- df %>%
    group_by(sensor_date) %>%
    slice_min(DT_round, n = 1) %>%
    arrange(DT_round)

  # Field note cleaning values
  cleaning_notes <- tibble(site = character(), DT_round = as.POSIXct(character()),
                           parameter_clean = character(), post_clean = numeric(), post_cal = numeric())
  if (!is.null(field_notes_data) && nrow(field_notes_data) > 0) {
    cleaning_notes <- tryCatch({
      field_notes_data %>%
        filter(parameter_clean == !!parameter & site == !!site &
               year(DT_round) == year & !is.na(value) & type != "pre_clean") %>%
        select(site, DT_round, parameter_clean, type, value) %>%
        mutate(value = if_else(parameter_clean == "ORP", value / 1000, value)) %>%
        pivot_wider(names_from = type, values_from = value) %>%
        filter(!is.na(post_clean) & !is.na(post_cal))
    }, error = function(e) {
      tibble(site = character(), DT_round = as.POSIXct(character()),
             parameter_clean = character(), post_clean = numeric(), post_cal = numeric())
    })
  }

  p <- ggplot(combined_data, aes(x = DT_round, y = value, color = data_type)) +
    geom_line(linewidth = 0.3, alpha = 0.8) +
    geom_point(data = cleaning_notes, aes(x = DT_round, y = post_clean),
               color = "black", shape = 17, size = 2, inherit.aes = FALSE) +
    scale_color_manual(
      values = c(
        "Original Data (Good Calibration)" = "springgreen4",
        "Original Data (Bad Calibration)" = "tomato",
        "Back-calibrated Data" = "steelblue"
      ),
      name = "Data Type"
    ) +
    geom_vline(xintercept = vline_df$DT_round) +
    labs(
      title = paste(site, parameter, year, "Calibration"),
      x = NULL,
      y = paste(parameter, "(units)")
    ) +
    theme_minimal()

  p <- ggplotly(p) %>%
    layout(legend = list(orientation = "h", x = 0.5, xanchor = 'center', y = -0.1))

  return(p)
}


#' Generate final calibration plot (post-decision)
#' @param df Final calibration data
#' @return A plotly object
final_calibration_plot <- function(df) {
  parameter <- unique(df$parameter)
  site <- unique(df$site)
  year <- lubridate::year(df$DT_round)[1]

  final_data <- df %>%
    select(DT_round, value = mean_cal) %>%
    mutate(data_type = "Final Data")

  vline_df <- df %>%
    group_by(sensor_date) %>%
    slice_min(DT_round, n = 1) %>%
    arrange(DT_round)

  p <- ggplot(final_data, aes(x = DT_round, y = value, color = data_type)) +
    geom_line(linewidth = 0.3, alpha = 0.8) +
    scale_color_manual(values = c("Final Data" = "black"), name = "Data Type") +
    geom_vline(xintercept = vline_df$DT_round) +
    labs(title = paste(site, parameter, year, "Final Data"), x = NULL, y = paste(parameter, "(units)")) +
    theme_minimal()

  p <- ggplotly(p) %>%
    layout(legend = list(orientation = "h", x = 0.5, xanchor = 'center', y = -0.1))

  return(p)
}


#' Generate final data frame with updated calibration decisions
#' @param cal_data The calibration plot data
#' @param site_cal_data Site calibration data frame
#' @param decisions Decision data frame with from/to columns
#' @return Updated calibration data frame
generate_final_df <- function(cal_data, site_cal_data, decisions) {
  parameter <- unique(cal_data$parameter)

  updated_calibrations <- site_cal_data %>%
    bind_cols(decisions) %>%
    select(sensor_date, DT_round, from, to)

  updated_cal_plot_df <- cal_data %>%
    dplyr::left_join(updated_calibrations, by = c("sensor_date", "DT_round")) %>%
    tidyr::fill(from, to, .direction = "downup") %>%
    mutate(
      updated_slope_from = case_when(
        from == "Original" ~ slope,
        from == "Lag" ~ slope_lag,
        from == "Lead" ~ slope_lead,
        .default = slope_final
      ),
      updated_offset_from = case_when(
        from == "Original" ~ offset,
        from == "Lag" ~ offset_lag,
        from == "Lead" ~ offset_lead,
        .default = offset_final
      ),
      updated_slope_to = case_when(
        to == "Original" ~ slope,
        to == "Lag" ~ slope_lag,
        to == "Lead" ~ slope_lead,
        .default = slope_lead
      ),
      updated_offset_to = case_when(
        to == "Original" ~ offset,
        to == "Lag" ~ offset_lag,
        to == "Lead" ~ offset_lead,
        .default = offset_lead
      )
    )

  # Apply parameter-specific back-calibration
  if (parameter %in% c("Chl-a Fluorescence", "FDOM Fluorescence", "ORP",
                        "Pressure", "Specific Conductivity", "DO", "Turbidity")) {
    updated_cal_plot_df <- updated_cal_plot_df %>%
      ross.wq.tools::cal_lin_trans_lm(
        df = .,
        raw_col = "mean_raw", slope_from_col = "updated_slope_from",
        offset_from_col = "updated_offset_from", slope_to_col = "updated_slope_to",
        offset_to_col = "updated_offset_to", wt_col = "wt"
      )
  }

  if (parameter == "pH") {
    updated_cal_plot_df <- updated_cal_plot_df %>%
      ross.wq.tools::cal_lin_trans_inv_lm_pH(
        df = .,
        mv_col = "mean_raw", slope_from_col = "updated_slope_from",
        offset_from_col = "updated_offset_from", slope_to_col = "updated_slope_to",
        offset_to_col = "updated_offset_to", wt_col = "wt"
      )
  }

  checked_df <- updated_cal_plot_df %>%
    ross.wq.tools::cal_check(df = ., obs_col = "mean", lm_trans_col = "mean_lm_trans") %>%
    mutate(cal_check = ifelse((from == "Original" & to == "Original"), FALSE, cal_check))

  final_df <- checked_df %>%
    dplyr::select(
      DT_round, site, sonde_serial, parameter,
      mean, mean_raw, mean_lm_trans, mean_cal, cal_check,
      sensor_serial, file_date, sonde_date, sensor_date_lag, sensor_date, sensor_date_lead,
      correct_calibration, slope_lag, offset_lag, slope, offset,
      slope_final, offset_final, slope_lead, offset_lead,
      updated_slope_from, updated_offset_from, updated_slope_to, updated_offset_to, wt
    )

  return(final_df)
}


#' Update calibration tracking backend (save finalized, remove from tracking)
#' @param df Finalized data frame
#' @param year Year string
#' @param site_param Site-parameter string
#' @param tracking_data Tracking data list (modified in place via <<-)
#' @param final_data Finalized data list (modified in place via <<-)
#' @param tracking_file Path to tracking RDS file
#' @param finalized_file Path to finalized RDS file
#' @param session Shiny session
update_cal_backend <- function(df, year, site_param, tracking_data, final_data,
                               tracking_file, finalized_file, session) {
  # Add to finalized data
  if (is.null(final_data[[year]])) {
    final_data[[year]] <- list()
  }
  final_data[[year]][[site_param]] <- df

  # Remove from tracking data
  tracking_data[[year]][[site_param]] <- NULL

  # Update UI choices
  remaining_choices <- names(tracking_data[[year]])

  if (length(remaining_choices) > 0) {
    updateSelectInput(session, "site_param_choice",
                      choices = remaining_choices,
                      selected = remaining_choices[1])
    showNotification(
      paste0("Calibration verified for: ", site_param, ". Remaining: ", length(remaining_choices)),
      type = "message", duration = 5
    )
  } else {
    showNotification(
      paste0("All calibrations verified for year ", year, "!"),
      type = "message", duration = 10
    )
  }

  # Save files
  readr::write_rds(tracking_data, tracking_file)
  readr::write_rds(final_data, finalized_file)

  return(list(tracking = tracking_data, finalized = final_data))
}
