##### Step 2: Load & Tidy Raw Data Module #####
# Upload raw WQ data, munge, tidy, and preview with data check plots

step2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
        h3("Load & Tidy Raw Data"),
        p("Upload raw water quality data files or use data pulled from HydroVu. Data will be tidied and summarized to 15-minute intervals.")
      )
    ),
    hr(),

    fluidRow(
      # Upload panel
      column(4,
        card(
          card_header("Data Upload"),
          card_body(
            uiOutput(ns("upload_ui")),
            hr(),
            actionButton(ns("load_data"), "Load & Process Data",
                         class = "btn-primary w-100"),
            br(), br(),
            uiOutput(ns("load_status"))
          )
        )
      ),
      # Data summary
      column(8,
        card(
          card_header("Data Summary"),
          card_body(
            DT::dataTableOutput(ns("data_summary_table"))
          )
        )
      )
    ),
    hr(),

    # Data check plots
    fluidRow(
      column(12,
        card(
          card_header("Data Check Plots"),
          card_body(
            selectInput(ns("check_site"), "Select Site:", choices = NULL),
            plotlyOutput(ns("check_plot"), height = "500px")
          )
        )
      )
    ),
    hr(),

    fluidRow(
      column(12,
        actionButton(ns("continue_step2"), "Continue to Calibration →",
                     class = "btn-success btn-lg w-100")
      )
    )
  )
}

step2_server <- function(id, pipeline, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Show upload UI based on data source
    output$upload_ui <- renderUI({
      if (pipeline$data_source == "upload") {
        tagList(
          fileInput(ns("data_files"), "Upload Raw Data Files:",
                    multiple = TRUE,
                    accept = c(".csv", ".parquet", ".rds", ".feather", ".xlsx")),
          selectInput(ns("timezone"), "Data Timezone:",
                      choices = c("UTC", "America/Denver", "MST"),
                      selected = "UTC")
        )
      } else {
        tagList(
          div(class = "alert alert-info",
              icon("info-circle"),
              "Using data pulled from HydroVu API in Step 1."),
          helpText("Data will be loaded from the staging directory.")
        )
      }
    })

    # Load and process data
    observeEvent(input$load_data, {

      withProgress(message = "Loading and tidying data...", value = 0.1, {
        tryCatch({

          if (pipeline$data_source == "hydrovu") {
            # Load from HydroVu staging directory
            staging_dir <- file.path(pipeline$verification_base_path, "hydro_vu_pull", "raw_data")

            setProgress(0.3, detail = "Munging API data...")
            hv_data <- ross.wq.tools::munge_api_data(api_dir = staging_dir, synapse_env = FALSE) %>%
              split(f = list(.$site, .$parameter), sep = "-") %>%
              keep(~ nrow(.) > 0)

            setProgress(0.6, detail = "Tidying data...")
            pipeline$tidy_data <- hv_data %>%
              future_map(~ ross.wq.tools::tidy_api_data(api_data = .), .progress = TRUE, .options = set_furr_options) %>%
              keep(~ !is.null(.)) %>%
              keep(~ unique(.$parameter) %in% pipeline$parameters)

          } else {
            # Load from uploaded files
            req(input$data_files)

            setProgress(0.3, detail = "Parsing uploaded files...")
            tz <- input$timezone

            all_data <- map(input$data_files$datapath, ~ parse_raw_file(.x, timezone = tz)) %>%
              keep(is.data.frame) %>%
              bind_rows()

            if (nrow(all_data) == 0) {
              showNotification("No valid data found in uploaded files", type = "error")
              return()
            }

            setProgress(0.6, detail = "Splitting by site-parameter...")
            pipeline$tidy_data <- all_data %>%
              split(f = list(.$site, .$parameter), sep = "-") %>%
              keep(~ nrow(.) > 0)
          }

          setProgress(0.9, detail = "Updating UI...")

          # Update site selector
          sites <- unique(map_chr(names(pipeline$tidy_data), ~ str_split(.x, "-")[[1]][1]))
          updateSelectInput(session, "check_site", choices = sites)

          setProgress(1.0, detail = "Complete!")
          showNotification(paste("Loaded", length(pipeline$tidy_data), "site-parameter datasets"),
                           type = "message")

        }, error = function(e) {
          showNotification(paste("Error loading data:", e$message), type = "error", duration = 10)
        })
      })
    })

    # Load status
    output$load_status <- renderUI({
      if (!is.null(pipeline$tidy_data)) {
        div(class = "alert alert-success",
            icon("check-circle"),
            paste("Loaded", length(pipeline$tidy_data), "site-parameter combinations"))
      }
    })

    # Data summary table
    output$data_summary_table <- DT::renderDataTable({
      req(pipeline$tidy_data)

      summary_df <- map_dfr(names(pipeline$tidy_data), function(name) {
        parts <- str_split(name, "-")[[1]]
        df <- pipeline$tidy_data[[name]]
        tibble(
          Site = parts[1],
          Parameter = paste(parts[-1], collapse = "-"),
          `N Records` = nrow(df),
          `Start Date` = format(min(df$DT_round, na.rm = TRUE), "%Y-%m-%d"),
          `End Date` = format(max(df$DT_round, na.rm = TRUE), "%Y-%m-%d"),
          `% Missing` = round(sum(is.na(df$mean)) / nrow(df) * 100, 1)
        )
      })

      DT::datatable(summary_df, options = list(pageLength = 25, scrollY = "400px"))
    })

    # Data check plots
    output$check_plot <- renderPlotly({
      req(pipeline$tidy_data, input$check_site)

      site_data <- pipeline$tidy_data %>%
        keep(~ str_starts(names(pipeline$tidy_data)[match(.x, pipeline$tidy_data)],
                          paste0(input$check_site, "-"))) %>%
        bind_rows() %>%
        mutate(DT_hourly = floor_date(DT_round, unit = "1 hour")) %>%
        group_by(site, parameter, DT_hourly) %>%
        summarize(hourly_med = median(mean, na.rm = TRUE), .groups = "drop")

      # Build site data from matching keys
      matching_keys <- names(pipeline$tidy_data)[str_starts(names(pipeline$tidy_data), paste0(input$check_site, "-"))]
      site_data <- map_dfr(matching_keys, ~ pipeline$tidy_data[[.x]]) %>%
        mutate(DT_hourly = floor_date(DT_round, unit = "1 hour")) %>%
        group_by(site, parameter, DT_hourly) %>%
        summarize(hourly_med = median(mean, na.rm = TRUE), .groups = "drop")

      if (nrow(site_data) == 0) return(plotly_empty())

      # Add field note markers if available
      p <- ggplot(site_data, aes(x = DT_hourly, y = hourly_med)) +
        geom_point(size = 0.5, alpha = 0.6) +
        facet_wrap(~parameter, scales = "free_y") +
        labs(title = input$check_site, x = "Date", y = "Hourly Median Value") +
        theme_minimal()

      if (!is.null(pipeline$field_notes)) {
        fn <- pipeline$field_notes %>%
          filter(site == input$check_site & year(DT_round) == pipeline$year)
        if (nrow(fn) > 0) {
          p <- p + geom_vline(data = fn, aes(xintercept = DT_round), alpha = 0.5, color = "red", linetype = "dashed")
        }
      }

      ggplotly(p)
    })

    # Continue to next step
    observeEvent(input$continue_step2, {
      req(pipeline$tidy_data)
      pipeline$step_complete$step2 <- TRUE
      updateNavbarPage(parent_session, "main_tabs", selected = "step3")
    })
  })
}
