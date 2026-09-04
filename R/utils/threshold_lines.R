##### Threshold Line Visualization #####
# Ported from manual_verification_tool/R/add_threshold_lines.R
# Adds seasonal threshold overlays to ggplots

#' Add seasonal threshold lines to a ggplot
#' @param plot A ggplot object
#' @param plot_data The data being plotted
#' @param site_arg Current site name
#' @param parameter_arg Current parameter name
#' @param meta_path Path to meta directory containing seasonal_thresholds.csv
#' @return Modified ggplot with threshold annotations
add_threshold_lines <- function(plot, plot_data, site_arg, parameter_arg, meta_path) {

  threshold_file <- file.path(meta_path, "seasonal_thresholds.csv")
  if (!file.exists(threshold_file)) return(plot)

  seasonal_thresholds <- read_csv(threshold_file, show_col_types = FALSE) %>%
    distinct(site, parameter, season, .keep_all = TRUE) %>%
    filter(parameter == parameter_arg, site == site_arg)

  unique_seasons <- unique(plot_data$season)
  seasonal_thresholds <- seasonal_thresholds %>%
    filter(season %in% unique_seasons)

  if (nrow(seasonal_thresholds) == 0) return(plot)

  if (nrow(seasonal_thresholds) > 1) {
    season_1 <- case_when(
      all(c("winter_baseflow", "snowmelt") %in% unique_seasons) ~ "winter_baseflow",
      all(c("snowmelt", "monsoon") %in% unique_seasons) ~ "snowmelt",
      all(c("monsoon", "fall_baseflow") %in% unique_seasons) ~ "monsoon",
      all(c("fall_baseflow", "winter_baseflow") %in% unique_seasons) ~ "fall_baseflow",
      TRUE ~ NA_character_
    )
    season_2 <- case_when(
      all(c("winter_baseflow", "snowmelt") %in% unique_seasons) ~ "snowmelt",
      all(c("snowmelt", "monsoon") %in% unique_seasons) ~ "monsoon",
      all(c("monsoon", "fall_baseflow") %in% unique_seasons) ~ "fall_baseflow",
      all(c("fall_baseflow", "winter_baseflow") %in% unique_seasons) ~ "winter_baseflow",
      TRUE ~ NA_character_
    )

    if (!is.na(season_1)) {
      s1_thresholds <- seasonal_thresholds %>% filter(season == season_1)
      s2_thresholds <- seasonal_thresholds %>% filter(season == season_2)

      if (nrow(s1_thresholds) > 0) {
        s1_low <- s1_thresholds$t_mean01
        s1_high <- s1_thresholds$t_mean99

        slice_data_s1 <- plot_data %>% filter(season == season_1)
        if (nrow(slice_data_s1) > 0) {
          plot <- plot +
            annotate("rect",
                     xmin = min(slice_data_s1$DT_round), xmax = max(slice_data_s1$DT_round),
                     ymin = s1_low, ymax = s1_high,
                     fill = "blue", alpha = 0.1)
        }
      }

      if (nrow(s2_thresholds) > 0) {
        s2_low <- s2_thresholds$t_mean01
        s2_high <- s2_thresholds$t_mean99

        slice_data_s2 <- plot_data %>% filter(season == season_2)
        if (nrow(slice_data_s2) > 0) {
          plot <- plot +
            annotate("rect",
                     xmin = min(slice_data_s2$DT_round), xmax = max(slice_data_s2$DT_round),
                     ymin = s2_low, ymax = s2_high,
                     fill = "green", alpha = 0.1)
        }
      }
    }
  } else {
    # Single season
    s_low <- seasonal_thresholds$t_mean01
    s_high <- seasonal_thresholds$t_mean99
    plot <- plot +
      geom_hline(yintercept = c(s_low, s_high), linetype = "dashed", color = "blue", alpha = 0.5)
  }

  return(plot)
}
