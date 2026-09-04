##### Step 3: Back-Calibration Review Module #####
# Ported from calibration_correction_tool. Manual review of back-calibration decisions.

step3_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
        h3("Manual Calibration Verification"),
        p("Review and adjust back-calibration decisions for each site-parameter combination.")
      )
    ),
    hr(),

    fluidRow(
      # Main content area
      column(9,
        # Plot output
        card(
          card_header("Calibration Plot"),
          card_body(
            plotlyOutput(ns("cal_plot"), height = "500px")
          )
        ),
        br(),
        # Information tables
        navset_card_tab(
          id = ns("main_tabs"),
          nav_panel("Site Calibration Data",
            DT::dataTableOutput(ns("site_calibration_df"))
          ),
          nav_panel("Sensor Calibration Data",
            DT::dataTableOutput(ns("sensor_calibration_df"))
          ),
          nav_panel("Field Note Information",
            DT::dataTableOutput(ns("field_notes_df"))
          )
        )
      ),
      # Sidebar
      column(3,
        # Options panel
        card(
          card_header("Calibration Options"),
          card_body(
            div(style = "max-height: 500px; overflow-y: auto; overflow-x: hidden; padding-right: 10px;",
              uiOutput(ns("dynamic_controls"))
            ),
            br(),
            actionButton(ns("previewPlot"), "Preview Plot", class = "btn-info w-100")
          )
        ),
        # Decision buttons
        card(
          card_header("Decisions"),
          card_body(
            actionButton(ns("acceptOriginal"), "Accept Original Data", class = "btn-secondary w-100 mb-2"),
            actionButton(ns("acceptRecalibrated"), "Accept Re-calibrated Data", class = "btn-primary w-100 mb-2"),
            actionButton(ns("acceptUpdates"), "Accept Updated Decisions", class = "btn-success w-100 mb-2")
          )
        ),
        # Navigation
        card(
          card_header("Navigation"),
          card_body(
            selectInput(ns("year_choice"), "Year:", choices = NULL),
            selectInput(ns("site_param_choice"), "Site-Parameter:", choices = NULL)
          )
        ),
        # Skip calibration
        card(
          card_body(
            actionButton(ns("skip_calibration"), "Skip Calibration Step →",
                         class = "btn-warning w-100"),
            helpText("Skip if no calibration data is available")
          )
        )
      )
    )
  )
}

step3_server <- function(id, pipeline, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Local reactive values for calibration state
    values <- reactiveValues(
      tracking_data = NULL,
      final_data = NULL,
      preview_final_df = NULL,
      selected_sensor_row = NULL,
      selected_sensor_data = NULL,
      selected_sensor_key = NULL
    )

    # Initialize calibration data when this step is reached
    observe({
      req(pipeline$calibration_data)

      # Use the tidy data organized into the calibration structure
      if (is.null(values$tracking_data) && !is.null(pipeline$tidy_data)) {

        # Organize sensor data for calibration
        sensor_data <- pipeline$tidy_data %>%
          bind_rows() %>%
          split(f = year(.$DT_round)) %>%
          map(~ {
            split_data <- split(.x, f = list(.x$site, .x$parameter), sep = "-")
            discard(split_data, ~ is.null(.) || nrow(.) == 0)
          })

        tryCatch({
          # Join calibration data
          sensor_calibration_data <- ross.wq.tools::cal_join_sensor_calibration_data(
            sensor_data_list = sensor_data,
            calibration_data_list = pipeline$calibration_data
          )

          # Prepare calibration windows
          prepped_snsr_cal_data <- ross.wq.tools::cal_prepare_calibration_windows(
            sensor_calibration_data_list = sensor_calibration_data
          )

          # Apply back-calibration
          calibrated_data <- prepped_snsr_cal_data %>%
            map(function(year_data) {
              year_data %>%
                map(function(site_param) {
                  site_param %>%
                    map_dfr(function(chunk) {
                      ross.wq.tools::cal_back_calibrate(chunk)
                    })
                })
            })

          values$tracking_data <- calibrated_data
          values$final_data <- lapply(calibrated_data, function(year_data) list())

          # Update year choices
          updateSelectInput(session, "year_choice",
                            choices = names(calibrated_data),
                            selected = as.character(pipeline$year))

        }, error = function(e) {
          showNotification(paste("Calibration setup failed:", e$message), type = "error")
        })
      }
    })

    # Update site-parameter choices when year changes
    observeEvent(input$year_choice, {
      req(values$tracking_data, input$year_choice)

      choices <- names(values$tracking_data[[input$year_choice]])
      updateSelectInput(session, "site_param_choice",
                        choices = choices,
                        selected = if (length(choices) > 0) choices[1] else NULL)
    })

    # Current calibration data reactive
    cal_plot_df <- reactive({
      req(values$tracking_data, input$year_choice, input$site_param_choice)
      values$tracking_data[[input$year_choice]][[input$site_param_choice]]
    })

    # Site calibration data reactive
    site_calibration_data_df <- reactive({
      req(cal_plot_df())
      cal_plot_df() %>%
        group_by(sensor_date) %>%
        slice_min(DT_round, n = 1) %>%
        ungroup()
    })

    # Calibration plot
    output$cal_plot <- renderPlotly({
      req(cal_plot_df())

      if (!is.null(values$preview_final_df)) {
        final_calibration_plot(values$preview_final_df)
      } else {
        cal_plot(cal_plot_df())
      }
    })

    # Site calibration data table
    output$site_calibration_df <- DT::renderDataTable({
      req(site_calibration_data_df())
      DT::datatable(site_calibration_data_df() %>%
        select(DT_round, site, parameter, sensor_serial,
               slope_lag, offset_lag, slope, offset, slope_lead, offset_lead,
               slope_final, offset_final, correct_calibration),
        options = list(scrollX = TRUE, pageLength = 10))
    })

    # Field notes table
    output$field_notes_df <- DT::renderDataTable({
      req(pipeline$field_notes)
      DT::datatable(pipeline$field_notes, options = list(scrollX = TRUE, pageLength = 10))
    })

    # Dynamic calibration decision controls
    output$dynamic_controls <- renderUI({
      site_cal_data <- site_calibration_data_df()
      if (is.null(site_cal_data) || nrow(site_cal_data) == 0) return(p("No data available"))

      ui_elements <- lapply(1:nrow(site_cal_data), function(i) {
        row_data <- site_cal_data[i, ]
        lag_present <- (!is.na(row_data$slope_lag) & !is.na(row_data$offset_lag))
        lead_present <- (!is.na(row_data$slope_lead) & !is.na(row_data$offset_lead))

        default_choices <- case_when(
          !lead_present & !lag_present ~ c(NA, "Original", NA),
          !lead_present ~ c("Lag", "Original", NA),
          !lag_present ~ c(NA, "Original", "Lead"),
          .default = c("Lag", "Original", "Lead")
        ) %>% discard(is.na)

        from_default <- case_when(
          row_data$slope_final == row_data$slope & row_data$offset_final == row_data$offset ~ "Original",
          row_data$slope_final == row_data$slope_lag & row_data$offset_final == row_data$offset_lag ~ "Lag",
          row_data$slope_final == row_data$slope_lead & row_data$offset_final == row_data$offset_lead ~ "Lead",
          .default = "Original"
        )
        to_default <- case_when(
          row_data$slope == row_data$slope_lead & row_data$offset == row_data$offset_lead ~ "Original",
          row_data$slope != row_data$slope_lead | row_data$offset != row_data$offset_lead ~ "Lead",
          .default = "Original"
        )

        div(
          style = "border: 1px solid #ddd; padding: 10px; margin: 5px 0; border-radius: 6px;",
          h6(paste0("Chunk ", i, ": ", as.character(row_data$DT_round))),
          fluidRow(
            column(6, selectInput(ns(paste0("from_decision_", i)), "From:", choices = default_choices, selected = from_default)),
            column(6, selectInput(ns(paste0("to_decision_", i)), "To:", choices = default_choices, selected = to_default))
          )
        )
      })
      do.call(tagList, ui_elements)
    })

    # Collect dynamic decisions
    dynamic_decisions <- reactive({
      req(site_calibration_data_df())
      site_cal_data <- site_calibration_data_df()
      if (is.null(site_cal_data) || nrow(site_cal_data) == 0) return(NULL)

      data.frame(
        row = 1:nrow(site_cal_data),
        from = sapply(1:nrow(site_cal_data), function(i) input[[paste0("from_decision_", i)]] %||% "Original"),
        to = sapply(1:nrow(site_cal_data), function(i) input[[paste0("to_decision_", i)]] %||% "Original"),
        stringsAsFactors = FALSE
      )
    })

    # Preview button
    observeEvent(input$previewPlot, {
      req(site_calibration_data_df(), cal_plot_df())
      decisions <- dynamic_decisions()
      values$preview_final_df <- generate_final_df(cal_plot_df(), site_calibration_data_df(), decisions)
    })

    # Accept Original
    observeEvent(input$acceptOriginal, {
      req(input$year_choice, input$site_param_choice, cal_plot_df())

      final_df <- cal_plot_df() %>%
        mutate(mean_cal = mean, cal_check = FALSE) %>%
        select(DT_round, site, sonde_serial, parameter,
               mean, mean_raw, mean_lm_trans, mean_cal, cal_check,
               sensor_serial, file_date, sonde_date, sensor_date_lag, sensor_date, sensor_date_lead,
               correct_calibration, slope_lag, offset_lag, slope, offset,
               slope_final, offset_final, slope_lead, offset_lead, wt)

      finalize_cal(final_df)
    })

    # Accept Recalibrated
    observeEvent(input$acceptRecalibrated, {
      req(input$year_choice, input$site_param_choice, cal_plot_df())

      final_df <- cal_plot_df() %>%
        mutate(cal_check = TRUE) %>%
        select(DT_round, site, sonde_serial, parameter,
               mean, mean_raw, mean_lm_trans, mean_cal, cal_check,
               sensor_serial, file_date, sonde_date, sensor_date_lag, sensor_date, sensor_date_lead,
               correct_calibration, slope_lag, offset_lag, slope, offset,
               slope_final, offset_final, slope_lead, offset_lead, wt)

      finalize_cal(final_df)
    })

    # Accept Updated Decisions
    observeEvent(input$acceptUpdates, {
      req(input$year_choice, input$site_param_choice)

      if (!is.null(values$preview_final_df)) {
        final_df <- values$preview_final_df
      } else {
        decisions <- dynamic_decisions()
        final_df <- generate_final_df(cal_plot_df(), site_calibration_data_df(), decisions)
      }

      finalize_cal(final_df)
    })

    # Helper to finalize a calibration decision
    finalize_cal <- function(final_df) {
      year <- input$year_choice
      site_param <- input$site_param_choice

      # Add to finalized data
      if (is.null(values$final_data[[year]])) values$final_data[[year]] <- list()
      values$final_data[[year]][[site_param]] <- final_df

      # Remove from tracking
      values$tracking_data[[year]][[site_param]] <- NULL

      # Update choices
      remaining <- names(values$tracking_data[[year]])
      if (length(remaining) > 0) {
        updateSelectInput(session, "site_param_choice", choices = remaining, selected = remaining[1])
        showNotification(paste0("Verified: ", site_param, ". Remaining: ", length(remaining)), type = "message")
      } else {
        showNotification(paste0("All calibrations verified for ", year, "!"), type = "message")
        # Store calibrated data in pipeline
        pipeline$calibrated_data <- values$final_data
        pipeline$step_complete$step3 <- TRUE
      }

      values$preview_final_df <- NULL
    }

    # Skip calibration
    observeEvent(input$skip_calibration, {
      # If skipping, pass tidy_data through as-is (no calibration applied)
      if (is.null(pipeline$calibrated_data)) {
        pipeline$calibrated_data <- list()
        pipeline$calibrated_data[[as.character(pipeline$year)]] <- pipeline$tidy_data
      }
      pipeline$step_complete$step3 <- TRUE
      updateNavbarPage(parent_session, "main_tabs", selected = "step4")
    })
  })
}
