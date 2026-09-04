##### Step 1: Data Pull Module #####
# Optional step: Pull field notes from mWater and/or sensor data from HydroVu API

step1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
        h3("API Data Pull"),
        p("Pull field notes from mWater and/or sensor data from HydroVu. Skip this step if you uploaded your own data.")
      )
    ),
    hr(),

    fluidRow(
      # mWater Pull
      column(6,
        card(
          card_header("mWater Field Notes Pull"),
          card_body(
            uiOutput(ns("mwater_status")),
            actionButton(ns("pull_mwater"), "Pull Field Notes from mWater",
                         class = "btn-primary w-100"),
            br(), br(),
            uiOutput(ns("mwater_preview"))
          )
        )
      ),
      # HydroVu Pull
      column(6,
        card(
          card_header("HydroVu Sensor Data Pull"),
          card_body(
            uiOutput(ns("hydrovu_status")),
            actionButton(ns("pull_hydrovu"), "Pull Sensor Data from HydroVu",
                         class = "btn-primary w-100"),
            helpText("⚠️ This may take 2-4 minutes per month of data per site."),
            br(),
            uiOutput(ns("hydrovu_preview"))
          )
        )
      )
    ),
    hr(),

    fluidRow(
      column(12,
        actionButton(ns("continue_step1"), "Continue to Load & Tidy →",
                     class = "btn-success btn-lg w-100")
      )
    )
  )
}

step1_server <- function(id, pipeline, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # mWater status indicator
    output$mwater_status <- renderUI({
      if (!is.null(pipeline$field_notes)) {
        div(class = "alert alert-success", icon("check"), "Field notes loaded")
      } else if (pipeline$field_note_source == "mwater") {
        div(class = "alert alert-info", icon("info-circle"), "Ready to pull from mWater")
      } else if (pipeline$field_note_source == "none") {
        div(class = "alert alert-secondary", icon("minus-circle"), "Field notes skipped")
      } else {
        div(class = "alert alert-secondary", icon("minus-circle"), "Field notes provided via upload")
      }
    })

    # HydroVu status indicator
    output$hydrovu_status <- renderUI({
      if (pipeline$data_source == "upload") {
        div(class = "alert alert-secondary", icon("minus-circle"), "Using uploaded data — HydroVu pull not needed")
      } else if (!is.null(pipeline$hv_creds)) {
        div(class = "alert alert-info", icon("info-circle"), "HydroVu credentials loaded, ready to pull")
      } else {
        div(class = "alert alert-warning", icon("exclamation-triangle"), "No HydroVu credentials provided")
      }
    })

    # Pull mWater field notes
    observeEvent(input$pull_mwater, {
      req(pipeline$mwater_creds)

      withProgress(message = "Pulling field notes from mWater...", value = 0.3, {
        tryCatch({
          mWater_data <- ross.wq.tools::load_mWater(creds = pipeline$mwater_creds)

          setProgress(0.6, detail = "Extracting sensor notes...")

          pipeline$field_notes <- ross.wq.tools::grab_mWater_sensor_notes(
            mWater_api_data = mWater_data
          ) %>%
            mutate(
              DT_round = with_tz(DT_round, tzone = "UTC"),
              last_site_visit = with_tz(last_site_visit, tzone = "UTC"),
              DT_join = as.character(DT_round)
            )

          setProgress(0.8, detail = "Extracting malfunction notes...")

          pipeline$malfunction_notes <- ross.wq.tools::grab_mWater_malfunction_notes(
            mWater_api_data = mWater_data
          ) %>%
            mutate(
              start_DT = with_tz(start_DT, tzone = "UTC"),
              end_DT = with_tz(end_DT, tzone = "UTC")
            )

          setProgress(1.0, detail = "Complete!")
          showNotification("Field notes pulled successfully!", type = "message")
        }, error = function(e) {
          showNotification(paste("mWater pull failed:", e$message), type = "error", duration = 10)
        })
      })
    })

    # mWater preview
    output$mwater_preview <- renderUI({
      if (!is.null(pipeline$field_notes)) {
        tagList(
          h6(paste("Field notes:", nrow(pipeline$field_notes), "records")),
          h6(paste("Sites:", paste(unique(pipeline$field_notes$site), collapse = ", "))),
          if (!is.null(pipeline$malfunction_notes)) {
            h6(paste("Malfunction records:", nrow(pipeline$malfunction_notes)))
          }
        )
      }
    })

    # Pull HydroVu data
    observeEvent(input$pull_hydrovu, {
      req(pipeline$hv_creds, pipeline$sites, pipeline$date_start, pipeline$date_end)

      # Create staging directory
      staging_dir <- file.path(pipeline$verification_base_path, "hydro_vu_pull", "raw_data")
      if (!dir.exists(staging_dir)) dir.create(staging_dir, recursive = TRUE)

      withProgress(message = "Pulling sensor data from HydroVu...", value = 0, {
        tryCatch({
          # Authenticate
          hv_token <- ross.wq.tools::hv_auth(
            client_id = as.character(pipeline$hv_creds[["client"]]),
            client_secret = as.character(pipeline$hv_creds[["secret"]])
          )
          pipeline$hv_token <- hv_token

          # Get site info
          hv_sites <- ross.wq.tools::hv_locations_all(hv_token) %>%
            filter(!grepl("vulink", name, ignore.case = TRUE)) %>%
            filter(!grepl("virridy", name, ignore.case = TRUE))

          n_sites <- length(pipeline$sites)

          # Pull data for each site
          for (i in seq_along(pipeline$sites)) {
            site <- pipeline$sites[i]
            setProgress(i / n_sites, detail = paste("Pulling", site, "..."))

            tryCatch({
              ross.wq.tools::api_puller(
                site = site,
                start_dt = with_tz(as.POSIXct(pipeline$date_start), tzone = "UTC"),
                end_dt = with_tz(as.POSIXct(pipeline$date_end), tzone = "UTC"),
                api_token = hv_token,
                hv_sites_arg = hv_sites,
                dump_dir = staging_dir
              )
            }, error = function(e) {
              showNotification(paste("Failed to pull", site, ":", e$message), type = "warning")
            })
          }

          showNotification("HydroVu data pull complete!", type = "message")
        }, error = function(e) {
          showNotification(paste("HydroVu pull failed:", e$message), type = "error", duration = 10)
        })
      })
    })

    # HydroVu preview
    output$hydrovu_preview <- renderUI({
      staging_dir <- file.path(pipeline$verification_base_path, "hydro_vu_pull", "raw_data")
      if (dir.exists(staging_dir)) {
        n_files <- length(list.files(staging_dir))
        if (n_files > 0) {
          h6(paste("Downloaded:", n_files, "data files"))
        }
      }
    })

    # Continue to next step
    observeEvent(input$continue_step1, {
      pipeline$step_complete$step1 <- TRUE
      updateNavbarPage(parent_session, "main_tabs", selected = "step2")
    })
  })
}
