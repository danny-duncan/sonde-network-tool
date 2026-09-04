##### Step 4: Automated Flagging Module #####
# Orchestrates the full flagging pipeline using ross.wq.tools functions

step4_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
        h3("Automated QA/QC Flagging"),
        p("Apply automated flagging pipeline: field notes, single-sensor flags, intra-sensor flags, and network check.")
      )
    ),
    hr(),

    fluidRow(
      column(4,
        card(
          card_header("Flagging Controls"),
          card_body(
            actionButton(ns("run_flagging"), "Run Full Flagging Pipeline",
                         class = "btn-primary btn-lg w-100"),
            hr(),
            uiOutput(ns("flagging_progress")),
            hr(),
            actionButton(ns("continue_step4"), "Continue to Save →",
                         class = "btn-success w-100")
          )
        )
      ),
      column(8,
        card(
          card_header("Flagging Summary"),
          card_body(
            DT::dataTableOutput(ns("flag_summary_table"))
          )
        )
      )
    ),
    hr(),

    # Flag preview plots
    fluidRow(
      column(12,
        card(
          card_header("Flagged Data Preview"),
          card_body(
            selectInput(ns("preview_site"), "Select Site:", choices = NULL),
            plotlyOutput(ns("flag_preview_plot"), height = "500px")
          )
        )
      )
    )
  )
}

step4_server <- function(id, pipeline, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    flagging_status <- reactiveVal("Not started")

    # Run the full flagging pipeline
    observeEvent(input$run_flagging, {

      # Get calibrated data organized as site-parameter list
      cal_data <- pipeline$calibrated_data
      year_key <- as.character(pipeline$year)

      # Get the data to flag (may be nested by year or flat)
      if (!is.null(cal_data[[year_key]])) {
        data_to_flag <- cal_data[[year_key]]
      } else {
        # Assume flat list
        data_to_flag <- cal_data
      }

      # Ensure data is a flat list of site-parameter data frames
      if (!is.list(data_to_flag) || is.data.frame(data_to_flag)) {
        data_to_flag <- list(data_to_flag)
      }

      withProgress(message = "Running flagging pipeline...", value = 0, {

        #### Stage 1: Add field notes ####
        setProgress(0.1, detail = "Adding field notes...")
        flagging_status("Adding field notes...")

        if (!is.null(pipeline$field_notes)) {
          field_notes <- pipeline$field_notes %>% ross.wq.tools::fix_site_names()
          combined_data <- data_to_flag %>%
            map(~ tryCatch(ross.wq.tools::add_field_notes(df = ., notes = field_notes),
                           error = function(e) .))
        } else {
          combined_data <- data_to_flag
        }

        #### Stage 2: Summary statistics ####
        setProgress(0.2, detail = "Generating summary statistics...")
        flagging_status("Generating summary statistics...")

        summarized_data <- combined_data %>%
          map(~ tryCatch(ross.wq.tools::generate_summary_statistics(.), error = function(e) .))

        #### Stage 3: Single-sensor flags ####
        setProgress(0.35, detail = "Applying single-sensor flags...")
        flagging_status("Applying single-sensor flags...")

        single_sensor_flags <- summarized_data %>%
          map(function(data) {
            tryCatch({
              flagged <- data %>%
                data.table(.) %>%
                ross.wq.tools::add_field_flag(df = .) %>%
                ross.wq.tools::add_na_flag(df = .) %>%
                ross.wq.tools::find_do_noise(df = .) %>%
                ross.wq.tools::add_repeat_flag(df = .)

              # Depth shift flag (conditionally use field notes)
              if (!is.null(pipeline$field_notes)) {
                flagged <- flagged %>%
                  ross.wq.tools::add_depth_shift_flag(df = ., level_shift_table = pipeline$field_notes, post2024 = TRUE)
              }

              # Drift flag
              flagged <- flagged %>%
                ross.wq.tools::add_drift_flag(df = .)

              # Spec range flags (if thresholds provided)
              param <- unique(data$parameter)
              if (!is.null(pipeline$sensor_thresholds) && param %in% names(pipeline$sensor_thresholds)) {
                flagged <- flagged %>%
                  data.table(.) %>%
                  ross.wq.tools::add_spec_flag(df = ., spec_table = pipeline$sensor_thresholds)
              }

              # Seasonal range flags
              if (!is.null(pipeline$seasonal_thresholds) && param %in% unique(pipeline$seasonal_thresholds$parameter)) {
                flagged <- flagged %>%
                  data.table(.) %>%
                  ross.wq.tools::add_seasonal_flag(df = ., threshold_table = pipeline$seasonal_thresholds)
              }

              data.table(flagged)
            }, error = function(e) {
              warning("Flagging error for dataset: ", e$message)
              data.table(data)
            })
          })

        #### Stage 4: Intra-sensor flags ####
        setProgress(0.55, detail = "Applying intra-sensor flags...")
        flagging_status("Applying intra-sensor flags...")

        intrasensor_data <- single_sensor_flags %>%
          rbindlist(fill = TRUE) %>%
          split(by = "site")

        intrasensor_flags_list <- intrasensor_data %>%
          map(function(data) {
            tryCatch({
              flagged <- data %>%
                data.table() %>%
                ross.wq.tools::add_frozen_flag(.) %>%
                ross.wq.tools::intersensor_check(.) %>%
                ross.wq.tools::add_burial_flag(.) %>%
                ross.wq.tools::add_unsubmerged_flag(.)

              flagged
            }, error = function(e) {
              warning("Intra-sensor flagging error: ", e$message)
              data
            })
          }) %>%
          rbindlist(fill = TRUE) %>%
          dplyr::mutate(flag = ifelse(flag == "", NA, flag)) %>%
          distinct(site, parameter, DT_round, mean, .keep_all = TRUE) %>%
          split(f = list(.$site, .$parameter), sep = "-") %>%
          purrr::discard(~ nrow(.) == 0)

        # Add malfunction flags if available
        if (!is.null(pipeline$malfunction_notes)) {
          intrasensor_flags_list <- intrasensor_flags_list %>%
            map(~ tryCatch(
              ross.wq.tools::add_malfunction_flag(df = ., malfunction_records = pipeline$malfunction_notes),
              error = function(e) .
            ))
        }

        #### Stage 5: Network check ####
        setProgress(0.75, detail = "Running network check...")
        flagging_status("Running network check...")

        if (!is.null(pipeline$site_order)) {
          final_flags <- intrasensor_flags_list %>%
            map(~ tryCatch(
              ross.wq.tools::network_check(df = ., intrasensor_flags_arg = intrasensor_flags_list,
                                           site_order_arg = pipeline$site_order),
              error = function(e) .
            )) %>%
            rbindlist(fill = TRUE) %>%
            ross.wq.tools::tidy_flag_column() %>%
            split(f = list(.$site, .$parameter), sep = "-") %>%
            map(~ tryCatch(ross.wq.tools::add_suspect_flag(.), error = function(e) .)) %>%
            rbindlist(fill = TRUE)
        } else {
          final_flags <- intrasensor_flags_list %>%
            rbindlist(fill = TRUE)
        }

        #### Stage 6: Final cleanup ####
        setProgress(0.9, detail = "Final cleanup...")
        flagging_status("Final cleanup...")

        pipeline$flagged_data <- final_flags %>%
          mutate(auto_flag = ifelse(is.na(auto_flag), NA,
                                    ifelse(auto_flag == "suspect data" & is.na(lag(auto_flag, 1)) & is.na(lead(auto_flag, 1)),
                                           NA, auto_flag))) %>%
          select(any_of(c("DT_round", "DT_join", "site", "parameter", "mean_pre_cal", "mean", "units",
                          "n_obs", "spread", "auto_flag", "mal_flag", "sonde_moved", "sonde_employed",
                          "season", "last_site_visit", "depth_change", "back_cal_performed"))) %>%
          mutate(auto_flag = ifelse(is.na(auto_flag), NA, ifelse(auto_flag == "", NA, auto_flag))) %>%
          split(f = list(.$site, .$parameter), sep = "-") %>%
          keep(~ nrow(.) > 0)

        # Update site selector
        sites <- unique(map_chr(names(pipeline$flagged_data), ~ str_split(.x, "-")[[1]][1]))
        updateSelectInput(session, "preview_site", choices = sites)

        setProgress(1.0, detail = "Complete!")
        flagging_status("Complete!")
        showNotification(paste("Flagging complete!", length(pipeline$flagged_data), "datasets processed"),
                         type = "message")
      })
    })

    # Progress display
    output$flagging_progress <- renderUI({
      status <- flagging_status()
      if (status == "Not started") {
        div(class = "alert alert-secondary", icon("info-circle"), "Click 'Run Flagging' to start")
      } else if (status == "Complete!") {
        div(class = "alert alert-success", icon("check-circle"), status)
      } else {
        div(class = "alert alert-info", icon("spinner", class = "fa-spin"), status)
      }
    })

    # Flag summary table
    output$flag_summary_table <- DT::renderDataTable({
      req(pipeline$flagged_data)

      summary_df <- map_dfr(names(pipeline$flagged_data), function(name) {
        parts <- str_split(name, "-")[[1]]
        df <- pipeline$flagged_data[[name]]
        n_total <- nrow(df)
        n_flagged <- sum(!is.na(df$auto_flag))
        n_mal <- sum(!is.na(df$mal_flag))

        tibble(
          Site = parts[1],
          Parameter = paste(parts[-1], collapse = "-"),
          `Total Records` = n_total,
          `Auto Flagged` = n_flagged,
          `% Flagged` = round(n_flagged / n_total * 100, 1),
          `Malfunction` = n_mal
        )
      })

      DT::datatable(summary_df, options = list(pageLength = 25, scrollY = "400px"))
    })

    # Flag preview plot
    output$flag_preview_plot <- renderPlotly({
      req(pipeline$flagged_data, input$preview_site)

      matching_keys <- names(pipeline$flagged_data)[str_starts(names(pipeline$flagged_data), paste0(input$preview_site, "-"))]
      site_data <- map_dfr(matching_keys, ~ pipeline$flagged_data[[.x]]) %>%
        mutate(flagged = !is.na(auto_flag))

      if (nrow(site_data) == 0) return(plotly_empty())

      p <- ggplot(site_data, aes(x = DT_round, y = mean, color = flagged)) +
        geom_point(size = 0.3, alpha = 0.6) +
        scale_color_manual(values = c("FALSE" = "grey40", "TRUE" = "red"), name = "Flagged") +
        facet_wrap(~parameter, scales = "free_y") +
        labs(title = paste(input$preview_site, "- Flagged Data"), x = "Date", y = "Value") +
        theme_minimal()

      ggplotly(p)
    })

    # Continue
    observeEvent(input$continue_step4, {
      req(pipeline$flagged_data)
      pipeline$step_complete$step4 <- TRUE
      updateNavbarPage(parent_session, "main_tabs", selected = "step5")
    })
  })
}
