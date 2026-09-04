##### Step 5: Save Flagged Dataset Module #####
# Save flagged data to user-chosen location and set up verification directories

step5_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
        h3("Save Flagged Dataset"),
        p("Save the flagged dataset and prepare the directory structure for manual verification.")
      )
    ),
    hr(),

    fluidRow(
      column(6,
        card(
          card_header("Save Location"),
          card_body(
            shinyDirButton(ns("save_dir"), "Choose Save Directory", "Select where to save flagged data"),
            verbatimTextOutput(ns("save_dir_text")),
            hr(),
            radioButtons(ns("save_format"), "Save Format:",
              choices = c("Parquet (recommended)" = "parquet", "CSV" = "csv", "Both" = "both"),
              selected = "parquet"
            ),
            hr(),
            actionButton(ns("save_flagged"), "Save Flagged Data",
                         class = "btn-primary btn-lg w-100"),
            br(), br(),
            uiOutput(ns("save_status"))
          )
        )
      ),
      column(6,
        card(
          card_header("Flagged Data Overview"),
          card_body(
            h5(textOutput(ns("n_datasets"))),
            DT::dataTableOutput(ns("flagged_overview"))
          )
        )
      )
    ),
    hr(),

    fluidRow(
      column(12,
        card(
          card_header("Prepare for Verification"),
          card_body(
            p("This will create the directory structure and format the data for the manual verification tool."),
            actionButton(ns("prep_verification"), "Prepare Verification Directories",
                         class = "btn-warning w-100"),
            br(), br(),
            uiOutput(ns("prep_status")),
            hr(),
            actionButton(ns("continue_step5"), "Continue to Verification →",
                         class = "btn-success btn-lg w-100")
          )
        )
      )
    )
  )
}

step5_server <- function(id, pipeline, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    volumes <- c(Home = fs::path_home(), getVolumes()())
    shinyDirChoose(input, "save_dir", roots = volumes, session = session)

    output$save_dir_text <- renderText({
      if (is.integer(input$save_dir)) {
        "No directory selected"
      } else {
        parseDirPath(volumes, input$save_dir)
      }
    })

    output$n_datasets <- renderText({
      if (!is.null(pipeline$flagged_data)) {
        paste(length(pipeline$flagged_data), "site-parameter datasets ready to save")
      } else {
        "No flagged data available"
      }
    })

    # Overview table
    output$flagged_overview <- DT::renderDataTable({
      req(pipeline$flagged_data)
      map_dfr(names(pipeline$flagged_data), function(name) {
        parts <- str_split(name, "-")[[1]]
        df <- pipeline$flagged_data[[name]]
        tibble(
          Site = parts[1],
          Parameter = paste(parts[-1], collapse = "-"),
          Records = nrow(df),
          Flagged = sum(!is.na(df$auto_flag)),
          `Start` = format(min(df$DT_round, na.rm = T), "%Y-%m-%d"),
          `End` = format(max(df$DT_round, na.rm = T), "%Y-%m-%d")
        )
      }) %>%
        DT::datatable(options = list(pageLength = 20))
    })

    # Save flagged data
    observeEvent(input$save_flagged, {
      req(pipeline$flagged_data)

      save_path <- if (!is.integer(input$save_dir)) {
        parseDirPath(volumes, input$save_dir)
      } else {
        pipeline$verification_base_path
      }

      if (is.null(save_path) || save_path == "") {
        showNotification("Please select a save directory", type = "error")
        return()
      }

      flagged_dir <- file.path(save_path, "flagged_data")
      if (!dir.exists(flagged_dir)) dir.create(flagged_dir, recursive = TRUE)

      withProgress(message = "Saving flagged data...", value = 0, {
        n <- length(pipeline$flagged_data)
        iwalk(pipeline$flagged_data, function(df, name, i = NULL) {
          if (input$save_format %in% c("parquet", "both")) {
            write_parquet(df, file.path(flagged_dir, paste0(name, ".parquet")))
          }
          if (input$save_format %in% c("csv", "both")) {
            write_csv(df, file.path(flagged_dir, paste0(name, ".csv")))
          }
        })
        setProgress(1.0)
      })

      pipeline$flagged_save_path <- flagged_dir

      output$save_status <- renderUI({
        div(class = "alert alert-success", icon("check-circle"),
            paste("Saved", length(pipeline$flagged_data), "datasets to", flagged_dir))
      })

      showNotification("Flagged data saved successfully!", type = "message")
    })

    # Prepare verification directories
    observeEvent(input$prep_verification, {
      req(pipeline$flagged_data, pipeline$verification_base_path)

      withProgress(message = "Preparing verification directories...", value = 0.2, {
        # Ensure directories exist
        dirs <- create_pipeline_directories(pipeline$verification_base_path)

        setProgress(0.5, detail = "Formatting data for verification...")

        # Format flagged data into verification format and save
        iwalk(pipeline$flagged_data, function(df, name) {
          # Add verification columns
          prepped <- df %>%
            mutate(
              mean_verified = mean,
              is_verified = FALSE,
              verification_status = NA,
              year = year(DT_round),
              week = week(DT_round),
              weekday = lubridate::wday(DT_round, week_start = 7),
              y_w = paste(year, "-", week),
              day = yday(DT_round),
              y_d = paste(year, "-", day),
              season = case_when(
                month(DT_round) %in% c(12, 1, 2, 3, 4) ~ "winter_baseflow",
                month(DT_round) %in% c(5, 6) ~ "snowmelt",
                month(DT_round) %in% c(7, 8, 9) ~ "monsoon",
                month(DT_round) %in% c(10, 11) ~ "fall_baseflow"
              ),
              flag = case_when(
                !is.na(auto_flag) & !is.na(mal_flag) ~ paste(auto_flag, mal_flag, sep = ";"),
                !is.na(auto_flag) & is.na(mal_flag) ~ auto_flag,
                !is.na(mal_flag) & is.na(auto_flag) ~ mal_flag
              ),
              user_flag = flag,
              brush_omit = FALSE,
              user = NA,
              final_status = NA,
              week_decision = NA,
              is_finalized = FALSE
            )

          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          data_hash <- digest::digest(prepped)
          filename <- glue("{name}_{timestamp}_{data_hash}.parquet")

          write_parquet(prepped, file.path(dirs$all_data, filename))
        })

        # Copy to pre_verification
        R.utils::copyDirectory(dirs$all_data, dirs$pre_verification)

        # Save meta files
        if (!is.null(pipeline$seasonal_thresholds)) {
          write_csv(pipeline$seasonal_thresholds, file.path(dirs$meta, "seasonal_thresholds.csv"))
        }

        # Save available flags
        all_flags <- pipeline$flagged_data %>%
          map(~ unique(.$auto_flag)) %>%
          unlist() %>%
          unique() %>%
          na.omit()
        write_csv(tibble(flags = all_flags), file.path(dirs$meta, "available_flags.csv"))

        # Update pipeline paths
        pipeline$in_progress_path <- dirs$in_progress
        pipeline$all_data_path <- dirs$all_data
        pipeline$pre_verification_path <- dirs$pre_verification
        pipeline$intermediary_path <- dirs$intermediary
        pipeline$verified_path <- dirs$verified
        pipeline$meta_path <- dirs$meta
        pipeline$post_ver_path <- dirs$post_verification

        setProgress(1.0, detail = "Complete!")
      })

      output$prep_status <- renderUI({
        div(class = "alert alert-success", icon("check-circle"),
            "Verification directories prepared successfully!")
      })

      showNotification("Verification directories prepared!", type = "message")
    })

    # Continue
    observeEvent(input$continue_step5, {
      pipeline$step_complete$step5 <- TRUE
      updateNavbarPage(parent_session, "main_tabs", selected = "step6")
    })
  })
}
