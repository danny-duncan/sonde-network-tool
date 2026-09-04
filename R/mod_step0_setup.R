##### Step 0: Setup & Configuration Module #####
# Conditional upload UI based on user selections for data source, field note source

step0_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
        h3("Pipeline Configuration"),
        p("Configure your data sources, upload required files, and set processing parameters before proceeding.")
      )
    ),
    hr(),

    #### Source Selection Cards ####
    fluidRow(
      # Data Source Selection
      column(6,
        card(
          card_header("Data Source"),
          card_body(
            radioButtons(ns("data_source"), "How will you provide raw sonde data?",
              choices = c(
                "Upload my own data files" = "upload",
                "Pull from HydroVu API" = "hydrovu"
              ),
              selected = "upload"
            )
          )
        )
      ),
      # Field Note Source Selection
      column(6,
        card(
          card_header("Field Notes Source"),
          card_body(
            radioButtons(ns("field_note_source"), "How will you provide field notes?",
              choices = c(
                "Upload field notes CSV" = "upload",
                "Pull from mWater API" = "mwater",
                "No field notes" = "none"
              ),
              selected = "none"
            )
          )
        )
      )
    ),
    hr(),

    #### Dynamic Upload Section ####
    fluidRow(
      column(12, h4("Required Uploads"))
    ),

    # Credentials section (conditional)
    fluidRow(
      # HydroVu creds (shown when data_source == "hydrovu")
      column(4,
        uiOutput(ns("hydrovu_creds_ui"))
      ),
      # mWater creds (shown when field_note_source == "mwater")
      column(4,
        uiOutput(ns("mwater_creds_ui"))
      ),
      # Field notes upload (shown when field_note_source == "upload")
      column(4,
        uiOutput(ns("field_notes_upload_ui"))
      )
    ),

    fluidRow(
      # CDWR API key (always shown, for streamflow overlay)
      column(4,
        card(
          card_header("CDWR Streamflow (Optional)"),
          card_body(
            fileInput(ns("cdwr_creds_file"), "CDWR API Credentials YAML:",
                      accept = c(".yaml", ".yml")),
            fileInput(ns("site_gauge_file"), "Site-to-Gauge Mapping CSV:",
                      accept = c(".csv")),
            helpText("CSV with columns: site, station_abbrev")
          )
        )
      ),
      # Calibration data uploads
      column(4,
        card(
          card_header("Calibration Data"),
          card_body(
            fileInput(ns("cal_data_file"), "Munged Calibration Data (.RDS):",
                      accept = c(".rds", ".RDS")),
            fileInput(ns("sensor_cal_file"), "Sensor Calibration Data (.RDS):",
                      accept = c(".rds", ".RDS")),
            helpText("Required for back-calibration step")
          )
        )
      ),
      # Threshold uploads
      column(4,
        card(
          card_header("Thresholds & Site Order"),
          card_body(
            fileInput(ns("sensor_thresholds_file"), "Sensor Spec Thresholds (.yml):",
                      accept = c(".yaml", ".yml")),
            fileInput(ns("seasonal_thresholds_file"), "Seasonal Thresholds (.csv):",
                      accept = c(".csv")),
            fileInput(ns("site_order_file"), "Site Order (.yml):",
                      accept = c(".yaml", ".yml")),
            helpText("Required for flagging & network check steps")
          )
        )
      )
    ),
    hr(),

    #### Configuration Section ####
    fluidRow(
      column(12, h4("Processing Configuration"))
    ),
    fluidRow(
      column(3,
        card(
          card_header("Year & Date Range"),
          card_body(
            numericInput(ns("year"), "Processing Year:", value = year(Sys.Date()), min = 2019, max = 2030),
            dateInput(ns("date_start"), "Start Date:", value = paste0(year(Sys.Date()), "-03-01")),
            dateInput(ns("date_end"), "End Date:", value = paste0(year(Sys.Date()), "-11-30"))
          )
        )
      ),
      column(3,
        card(
          card_header("Timezone"),
          card_body(
            selectInput(ns("timezone"), "Data Timezone:",
                        choices = c("UTC", "America/Denver", "MST", "America/Chicago", "America/New_York"),
                        selected = "UTC")
          )
        )
      ),
      column(6,
        card(
          card_header("Parameters of Interest"),
          card_body(
            checkboxGroupInput(ns("parameters"), label = NULL,
              choices = c("Specific Conductivity", "Temperature", "DO", "pH", "ORP", "Depth",
                          "Chl-a Fluorescence", "Turbidity", "FDOM Fluorescence"),
              selected = c("Specific Conductivity", "Temperature", "DO", "pH", "ORP", "Depth",
                           "Chl-a Fluorescence", "Turbidity", "FDOM Fluorescence"),
              inline = TRUE
            )
          )
        )
      )
    ),

    #### Site List Upload ####
    fluidRow(
      column(6,
        card(
          card_header("Site List"),
          card_body(
            fileInput(ns("site_list_file"), "Upload Site List (.csv or .yml):",
                      accept = c(".csv", ".yaml", ".yml")),
            helpText("CSV with a 'site' column, or YAML list of site names"),
            uiOutput(ns("site_list_preview"))
          )
        )
      ),
      column(6,
        card(
          card_header("Output Directory"),
          card_body(
            shinyDirButton(ns("output_dir"), "Choose Output Directory", "Select output location"),
            verbatimTextOutput(ns("output_dir_text")),
            helpText("All pipeline outputs will be saved here")
          )
        )
      )
    ),
    hr(),

    #### Complete Setup Button ####
    fluidRow(
      column(12,
        actionButton(ns("complete_setup"), "Complete Setup & Continue →",
                     class = "btn-primary btn-lg w-100"),
        br(), br(),
        uiOutput(ns("setup_status"))
      )
    )
  )
}

step0_server <- function(id, pipeline, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #### Conditional UI Rendering ####

    # Show HydroVu creds upload when data source is "hydrovu"
    output$hydrovu_creds_ui <- renderUI({
      req(input$data_source)
      if (input$data_source == "hydrovu") {
        card(
          card_header("HydroVu API Credentials"),
          card_body(
            fileInput(ns("hv_creds_file"), "HydroVu Credentials YAML:",
                      accept = c(".yaml", ".yml")),
            helpText("YAML with 'client' and 'secret' keys")
          )
        )
      }
    })

    # Show mWater creds upload when field note source is "mwater"
    output$mwater_creds_ui <- renderUI({
      req(input$field_note_source)
      if (input$field_note_source == "mwater") {
        card(
          card_header("mWater API Credentials"),
          card_body(
            fileInput(ns("mwater_creds_file"), "mWater Credentials YAML:",
                      accept = c(".yaml", ".yml")),
            helpText("YAML with mWater API credentials")
          )
        )
      }
    })

    # Show field notes upload when field note source is "upload"
    output$field_notes_upload_ui <- renderUI({
      req(input$field_note_source)
      if (input$field_note_source == "upload") {
        card(
          card_header("Field Notes Upload"),
          card_body(
            fileInput(ns("field_notes_file"), "Field Notes CSV:",
                      accept = c(".csv")),
            helpText("CSV with columns: site, DT_round, sonde_employed, etc.")
          )
        )
      }
    })

    # Output directory selection
    volumes <- c(Home = fs::path_home(), getVolumes()())
    shinyDirChoose(input, "output_dir", roots = volumes, session = session)

    output$output_dir_text <- renderText({
      if (is.integer(input$output_dir)) {
        "No directory selected"
      } else {
        parseDirPath(volumes, input$output_dir)
      }
    })

    # Site list preview
    output$site_list_preview <- renderUI({
      req(input$site_list_file)
      file_ext <- tools::file_ext(input$site_list_file$name)

      sites <- tryCatch({
        if (file_ext %in% c("yaml", "yml")) {
          yaml::read_yaml(input$site_list_file$datapath)
        } else {
          read_csv(input$site_list_file$datapath, show_col_types = FALSE)$site
        }
      }, error = function(e) NULL)

      if (!is.null(sites)) {
        site_list <- if (is.list(sites)) unlist(sites) else sites
        tagList(
          h6(paste("Found", length(site_list), "sites:")),
          tags$code(paste(site_list, collapse = ", "))
        )
      }
    })

    #### Complete Setup Handler ####
    observeEvent(input$complete_setup, {
      # Validate required inputs
      errors <- character(0)

      # Check data source requirements
      if (input$data_source == "hydrovu" && is.null(input$hv_creds_file)) {
        errors <- c(errors, "HydroVu credentials are required when pulling from API")
      }
      if (input$field_note_source == "mwater" && is.null(input$mwater_creds_file)) {
        errors <- c(errors, "mWater credentials are required when pulling field notes from API")
      }
      if (input$field_note_source == "upload" && is.null(input$field_notes_file)) {
        errors <- c(errors, "Field notes file is required when uploading field notes")
      }
      if (is.null(input$site_list_file)) {
        errors <- c(errors, "Site list is required")
      }
      if (length(input$parameters) == 0) {
        errors <- c(errors, "At least one parameter must be selected")
      }

      if (length(errors) > 0) {
        output$setup_status <- renderUI({
          tagList(
            map(errors, ~ div(class = "alert alert-danger", icon("exclamation-triangle"), .x))
          )
        })
        return()
      }

      # Store configuration in pipeline reactive values
      pipeline$year <- input$year
      pipeline$year_cycle <- paste0(input$year, "_cycle")
      pipeline$date_start <- input$date_start
      pipeline$date_end <- input$date_end
      pipeline$parameters <- input$parameters
      pipeline$data_source <- input$data_source
      pipeline$field_note_source <- input$field_note_source

      # Parse site list
      file_ext <- tools::file_ext(input$site_list_file$name)
      pipeline$sites <- tryCatch({
        if (file_ext %in% c("yaml", "yml")) {
          unlist(yaml::read_yaml(input$site_list_file$datapath))
        } else {
          read_csv(input$site_list_file$datapath, show_col_types = FALSE)$site
        }
      }, error = function(e) NULL)

      # Read credentials
      if (!is.null(input$hv_creds_file)) {
        pipeline$hv_creds <- yaml::read_yaml(input$hv_creds_file$datapath)
      }
      if (!is.null(input$mwater_creds_file)) {
        pipeline$mwater_creds <- yaml::read_yaml(input$mwater_creds_file$datapath)
      }
      if (!is.null(input$cdwr_creds_file)) {
        pipeline$cdwr_creds <- yaml::read_yaml(input$cdwr_creds_file$datapath)
      }

      # Read site-gauge mapping
      if (!is.null(input$site_gauge_file)) {
        pipeline$site_gauge_mapping <- read_csv(input$site_gauge_file$datapath, show_col_types = FALSE)
      }

      # Read threshold files
      if (!is.null(input$sensor_thresholds_file)) {
        pipeline$sensor_thresholds <- yaml::read_yaml(input$sensor_thresholds_file$datapath)
      }
      if (!is.null(input$seasonal_thresholds_file)) {
        pipeline$seasonal_thresholds <- read_csv(input$seasonal_thresholds_file$datapath, show_col_types = FALSE) %>%
          ross.wq.tools::fix_site_names()
      }
      if (!is.null(input$site_order_file)) {
        pipeline$site_order <- ross.wq.tools::load_site_order(input$site_order_file$datapath)
      }

      # Read calibration data
      if (!is.null(input$cal_data_file)) {
        ross.wq.tools::load_calibration_data(cal_data_file_path = input$cal_data_file$datapath)
        pipeline$calibration_data <- calibration_data  # loaded into global by load_calibration_data
      }
      if (!is.null(input$sensor_cal_file)) {
        # sensor_calibration_data is used in the calibration tool
      }

      # Read field notes if uploaded
      if (input$field_note_source == "upload" && !is.null(input$field_notes_file)) {
        pipeline$field_notes <- read_csv(input$field_notes_file$datapath, show_col_types = FALSE) %>%
          mutate(DT_round = anytime::anytime(DT_round, tz = "UTC"))
      }

      # Set up output directory
      if (!is.integer(input$output_dir)) {
        output_path <- parseDirPath(volumes, input$output_dir)
        pipeline$verification_base_path <- file.path(output_path, pipeline$year_cycle)

        # Create pipeline directories
        dirs <- create_pipeline_directories(pipeline$verification_base_path)
        pipeline$in_progress_path <- dirs$in_progress
        pipeline$all_data_path <- dirs$all_data
        pipeline$pre_verification_path <- dirs$pre_verification
        pipeline$intermediary_path <- dirs$intermediary
        pipeline$verified_path <- dirs$verified
        pipeline$meta_path <- dirs$meta
        pipeline$post_ver_path <- dirs$post_verification
      }

      # Mark step as complete
      pipeline$step_complete$step0 <- TRUE

      output$setup_status <- renderUI({
        div(class = "alert alert-success",
            icon("check-circle"),
            "Setup complete! Navigate to the next step.")
      })

      showNotification("Setup complete! Proceed to Data Pull or Load & Tidy.",
                       type = "message", duration = 5)

      # Navigate to next step
      if (input$data_source == "hydrovu" || input$field_note_source == "mwater") {
        updateNavbarPage(parent_session, "main_tabs", selected = "step1")
      } else {
        updateNavbarPage(parent_session, "main_tabs", selected = "step2")
      }
    })
  })
}
