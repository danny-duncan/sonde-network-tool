##### Step 7: Drift Correction Module #####
# Ported from drift_correction_tool
# Identifies drift windows for FDOM and Turbidity, applies mathematical corrections

step7_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
        h3("Drift Correction"),
        p("Review and correct drift for FDOM Fluorescence and Turbidity parameters.")
      )
    ),
    hr(),

    fluidRow(
      # Sidebar
      column(3,
        card(
          card_header("Data Selection"),
          card_body(
            selectInput(ns("site"), "Site:", choices = NULL),
            selectInput(ns("parameter"), "Parameter:",
                        choices = c("FDOM Fluorescence", "Turbidity"),
                        selected = "FDOM Fluorescence"),
            actionButton(ns("load_data"), "Load Data", class = "btn-primary w-100")
          )
        ),
        card(
          card_header("Drift Details"),
          card_body(
            h5(textOutput(ns("status_text"))),
            hr(),
            p("Click on the plot to set window start/end times."),
            div(style = "max-height: 400px; overflow-y: auto; overflow-x: hidden; padding-right: 10px;",
              uiOutput(ns("dynamic_drift_windows"))
            ),
            hr(),
            actionButton(ns("apply_updates"), "Apply Window Updates", class = "btn-warning w-100"),
            br(), br(),
            actionButton(ns("submit_final"), "Submit Final Corrections", class = "btn-success w-100")
          )
        ),
        # Additional sites
        card(
          card_header("Compare Sites"),
          card_body(
            checkboxGroupInput(ns("add_sites"), "Add Sites:", choices = NULL)
          )
        )
      ),

      # Main plot area
      column(9,
        card(
          card_header("Drift Visualization"),
          card_body(
            plotlyOutput(ns("drift_plot"), height = "600px")
          )
        ),
        navset_card_tab(
          id = ns("tables_tab"),
          nav_panel("Data View", DT::dataTableOutput(ns("data_table"))),
          nav_panel("Tracking Excel", DT::dataTableOutput(ns("tracking_table")))
        )
      )
    ),
    hr(),
    fluidRow(
      column(12,
        actionButton(ns("continue_step7"), "Continue to Export →", class = "btn-success btn-lg w-100")
      )
    )
  )
}

step7_server <- function(id, pipeline, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #### Reactive Values ####
    values <- reactiveValues(
      raw_data = NULL,
      current_filename = NULL,
      windows_df = NULL,
      scenarios_df = NULL,
      final_df = NULL,
      click_target = "start",
      click_window_idx = 1,
      tracking_excel = NULL,
      other_sites_data = list()
    )

    #### Setup and Load ####

    # Update site choices from verified directory
    observe({
      req(pipeline$verified_path)
      files <- list.files(pipeline$verified_path, pattern = "\\.parquet$")
      if (length(files) > 0) {
        sites <- unique(map_chr(files, ~ split_filename(.x)$site))
        updateSelectInput(session, "site", choices = sites)
      }
    })

    # Update comparison sites
    observeEvent(input$site, {
      compare_sites <- relevant_sonde_selector(input$site)
      updateCheckboxGroupInput(session, "add_sites", choices = compare_sites)
    })

    # Load data for selected site/parameter
    observeEvent(input$load_data, {
      req(input$site, input$parameter, pipeline$verified_path)

      param_file <- gsub(" ", "_", input$parameter)
      pattern <- paste0("^", input$site, "-", param_file, "_FINAL_")
      files <- list.files(pipeline$verified_path, pattern = pattern, full.names = TRUE)

      if (length(files) == 0) {
        showNotification("No verified data found for this site-parameter", type = "warning")
        return()
      }

      data_file <- files[length(files)]
      values$current_filename <- basename(data_file)
      df <- read_parquet(data_file)

      # Check if drift actually exists
      if (!"drift" %in% names(df) || !any(df$drift, na.rm = TRUE)) {
        showNotification("No drift flags found in this dataset. You can submit it as is.", type = "message")
        df <- df %>%
          mutate(mean_drift_trans = mean_analysis, correction_type = "raw")
        values$raw_data <- df
        values$final_df <- df
        values$windows_df <- tibble(start_dt = as.POSIXct(character()), end_dt = as.POSIXct(character()))
      } else {
        # Calculate initial fallback windows
        windows <- calculate_fallback_windows(df)
        values$windows_df <- windows
        values$raw_data <- df

        # Load other sites data for scenario grid
        site_order_template <- pipeline$site_order
        if (!is.null(site_order_template)) {
          # Attempt to load data for other sites for fitted/exponential math
          other_files <- list.files(pipeline$verified_path, pattern = paste0("-", param_file, "_FINAL_"), full.names = TRUE)
          other_files <- other_files[!other_files %in% data_file]

          prepped_data <- map(other_files, read_parquet)
          names(prepped_data) <- map_chr(other_files, ~ split_filename(basename(.x))$site)
          values$other_sites_data <- prepped_data
        }

        # Generate initial scenarios and stitch
        df_scenarios <- generate_scenario_grid(df, windows, values$other_sites_data, pipeline$site_order)
        values$scenarios_df <- df_scenarios
        values$final_df <- stitch_final_corrections(df_scenarios, windows)
      }
    })

    #### Status Text ####
    output$status_text <- renderText({
      if (is.null(values$windows_df)) return("Load data to begin")
      n_windows <- nrow(values$windows_df)
      if (n_windows == 0) return("No drift windows detected")
      paste("Found", n_windows, "drift windows")
    })

    #### Plotly Visualization ####
    output$drift_plot <- renderPlotly({
      req(values$final_df)

      df <- values$final_df

      # Basic plot: original vs corrected
      p <- plot_ly(df, x = ~DT_round) %>%
        add_lines(y = ~mean_analysis, name = "Original (Verified)", line = list(color = "rgba(0,0,0,0.3)")) %>%
        add_lines(y = ~mean_drift_trans, name = "Drift Corrected", line = list(color = "blue", width = 2))

      # Highlight active drift regions
      if (nrow(values$windows_df) > 0) {
        for (i in 1:nrow(values$windows_df)) {
          p <- p %>% add_segments(
            x = values$windows_df$start_dt[i], xend = values$windows_df$start_dt[i],
            y = min(df$mean_analysis, na.rm=T), yend = max(df$mean_analysis, na.rm=T),
            line = list(color = "red", dash = "dash"), name = paste("Start", i), showlegend = FALSE
          ) %>% add_segments(
            x = values$windows_df$end_dt[i], xend = values$windows_df$end_dt[i],
            y = min(df$mean_analysis, na.rm=T), yend = max(df$mean_analysis, na.rm=T),
            line = list(color = "green", dash = "dash"), name = paste("End", i), showlegend = FALSE
          )
        }
      }

      # Add comparison sites
      if (!is.null(input$add_sites) && length(input$add_sites) > 0) {
        for (s in input$add_sites) {
          if (s %in% names(values$other_sites_data)) {
            comp_df <- values$other_sites_data[[s]]
            p <- p %>% add_lines(data = comp_df, x = ~DT_round, y = ~mean_analysis,
                                 name = s, line = list(dash = "dot", width = 1))
          }
        }
      }

      p %>%
        layout(title = paste(input$site, "-", input$parameter),
               xaxis = list(title = "Date"),
               yaxis = list(title = input$parameter),
               dragmode = "pan") %>%
        event_register("plotly_click")
    })

    #### Click Event Handling ####
    observeEvent(event_data("plotly_click"), {
      req(values$windows_df)
      click <- event_data("plotly_click")
      click_time <- as.POSIXct(click$x, tz = "UTC", origin = "1970-01-01")
      idx <- values$click_window_idx

      if (idx <= nrow(values$windows_df)) {
        if (values$click_target == "start") {
          values$windows_df$start_dt[idx] <- click_time
          values$click_target <- "end"
          showNotification("Start set. Click for end time.", type = "message")
        } else {
          values$windows_df$end_dt[idx] <- click_time
          values$click_target <- "start"
          # Auto advance to next window
          if (idx < nrow(values$windows_df)) {
            values$click_window_idx <- idx + 1
            showNotification(paste("End set. Moving to Window", idx + 1), type = "message")
          } else {
            showNotification("End set for all windows.", type = "message")
          }
        }
      }
    })

    #### Dynamic UI Controls ####
    output$dynamic_drift_windows <- renderUI({
      req(values$windows_df)
      if (nrow(values$windows_df) == 0) return(NULL)

      lapply(1:nrow(values$windows_df), function(i) {
        w <- values$windows_df[i, ]
        div(
          style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
          h6(paste("Window", i)),
          fluidRow(
            column(12,
              HTML(paste("Start:", format(w$start_dt, "%Y-%m-%d %H:%M"), "<br>",
                         "End:", format(w$end_dt, "%Y-%m-%d %H:%M")))
            )
          ),
          fluidRow(
            column(6,
              selectInput(ns(paste0("drift_type_", i)), "Strategy",
                          choices = c("None", "linear", "exponential", "uniform",
                                      "fitted_linear", "non_resolved", "unflag"),
                          selected = w$arg_drift_type)
            ),
            column(6,
              selectInput(ns(paste0("formulation_", i)), "Formulation",
                          choices = c("additive", "multiplicative"),
                          selected = w$arg_correction_type)
            )
          ),
          actionButton(ns(paste0("set_click_", i)), "Set via Click", class = "btn-sm btn-outline-info")
        )
      })
    })

    # Handle Set via Click buttons
    observe({
      req(values$windows_df)
      for (i in 1:nrow(values$windows_df)) {
        local({
          idx <- i
          observeEvent(input[[paste0("set_click_", idx)]], {
            values$click_window_idx <- idx
            values$click_target <- "start"
            showNotification(paste("Click plot to set start time for Window", idx), type = "message")
          })
        })
      }
    })

    #### Apply Updates ####
    observeEvent(input$apply_updates, {
      req(values$windows_df, values$raw_data)

      # Read values from dynamic UI
      for (i in 1:nrow(values$windows_df)) {
        type_val <- input[[paste0("drift_type_", i)]]
        form_val <- input[[paste0("formulation_", i)]]
        if (!is.null(type_val)) values$windows_df$arg_drift_type[i] <- type_val
        if (!is.null(form_val)) values$windows_df$arg_correction_type[i] <- form_val
      }

      # Recalculate
      withProgress(message = "Recalculating drift corrections...", value = 0.5, {
        values$scenarios_df <- generate_scenario_grid(
          values$raw_data, values$windows_df, values$other_sites_data, pipeline$site_order
        )
        values$final_df <- stitch_final_corrections(values$scenarios_df, values$windows_df)
        setProgress(1.0)
      })

      showNotification("Updates applied", type = "message")
    })

    #### Submit Final Corrections ####
    observeEvent(input$submit_final, {
      req(values$final_df, values$current_filename)

      # Determine new filename
      parsed <- split_filename(values$current_filename)
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      hash <- digest::digest(values$final_df)
      param_file <- gsub(" ", "_", parsed$parameter)
      new_filename <- glue("{parsed$site}-{param_file}_DRIFT_{timestamp}_{hash}.parquet")

      out_dir <- pipeline$post_ver_path

      # Remove previous drift files for this site-param
      old_files <- list.files(out_dir, pattern = paste0("^", parsed$site, "-", param_file, "_DRIFT_"), full.names = TRUE)
      if (length(old_files) > 0) file.remove(old_files)

      # Save to post_verification directory
      write_parquet(values$final_df, file.path(out_dir, new_filename))

      showNotification(paste("Saved drift corrections for", parsed$site, parsed$parameter), type = "message")

      # Update choices list (remove finished)
      choices <- input$site
      updateSelectInput(session, "site", selected = character(0))
    })

    #### Continue ####
    observeEvent(input$continue_step7, {
      pipeline$step_complete$step7 <- TRUE
      updateNavbarPage(parent_session, "main_tabs", selected = "step8")
    })
  })
}
