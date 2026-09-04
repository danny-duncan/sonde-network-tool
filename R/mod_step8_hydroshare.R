##### Step 8: HydroShare Export Module #####
# Formats and saves finalized datasets for HydroShare

step8_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
        h3("HydroShare Export"),
        p("Combine all finalized data, standardize formats, and export for HydroShare publication.")
      )
    ),
    hr(),

    fluidRow(
      column(4,
        card(
          card_header("Export Options"),
          card_body(
            actionButton(ns("prep_export"), "Prepare Export Datasets", class = "btn-primary w-100"),
            br(), br(),
            uiOutput(ns("prep_status")),
            hr(),
            shinyDirButton(ns("export_dir"), "Choose Export Directory", "Select final export location"),
            verbatimTextOutput(ns("export_dir_text")),
            br(),
            actionButton(ns("save_export"), "Save Final Datasets", class = "btn-success w-100 mb-2")
          )
        )
      ),
      column(8,
        card(
          card_header("Preview Cleaned Data"),
          card_body(
            selectInput(ns("preview_site"), "Select Site:", choices = NULL),
            plotlyOutput(ns("export_plot"), height = "400px")
          )
        )
      )
    )
  )
}

step8_server <- function(id, pipeline, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    volumes <- c(Home = fs::path_home(), getVolumes()())
    shinyDirChoose(input, "export_dir", roots = volumes, session = session)

    output$export_dir_text <- renderText({
      if (is.integer(input$export_dir)) {
        "No directory selected"
      } else {
        parseDirPath(volumes, input$export_dir)
      }
    })

    # Prepare datasets
    observeEvent(input$prep_export, {
      req(pipeline$verified_path, pipeline$post_ver_path)

      withProgress(message = "Preparing datasets for export...", value = 0, {

        # Load drift-corrected data (from post_verification)
        drift_files <- list.files(pipeline$post_ver_path, pattern = "\\.parquet$", full.names = TRUE)
        drift_data <- map(drift_files, read_parquet)
        names(drift_data) <- map_chr(drift_files, ~ split_filename(basename(.x))$site)

        setProgress(0.3, detail = "Loading verified data...")

        # Load verified data (non-drift)
        ver_files <- list.files(pipeline$verified_path, pattern = "\\.parquet$", full.names = TRUE)
        # Exclude those that have drift corrections
        drift_keys <- map_chr(drift_files, ~ paste0(split_filename(basename(.x))$site, "-", split_filename(basename(.x))$parameter))

        ver_data <- map(ver_files, function(f) {
          key <- paste0(split_filename(basename(f))$site, "-", split_filename(basename(f))$parameter)
          if (!key %in% drift_keys) {
            read_parquet(f)
          } else {
            NULL
          }
        }) %>% compact()

        setProgress(0.6, detail = "Combining datasets...")

        # Combine all data
        all_data <- bind_rows(drift_data, ver_data)

        # Standardize for HydroShare
        all_data <- all_data %>%
          # Standardize column naming and structure
          mutate(
            DT_round = with_tz(DT_round, tzone = "UTC"),
            modifications = case_when(
              final_status == "OMIT" ~ "manual data exclusion",
              final_status == "PASS" & !is.na(correction_type) & correction_type != "raw" & correct_calibration == FALSE ~ "calibration correction;drift correction",
              final_status == "PASS" & !is.na(correction_type) & correction_type != "raw" & correct_calibration == TRUE ~ "drift correction",
              final_status == "PASS" & correct_calibration == FALSE ~ "calibration correction",
              TRUE ~ NA_character_
            ),
            # Set omitted data to NA
            mean_final = ifelse(final_status == "OMIT", NA_real_,
                                ifelse(!is.na(mean_drift_trans), mean_drift_trans, mean_verified)),
            mean_final = round(mean_final, 3)
          ) %>%
          arrange(site, parameter, DT_round)

        # Generate "raw" format (all QC columns included)
        hs_raw <- all_data

        # Generate "cleaned" format (stripped down for publication)
        hs_clean <- all_data %>%
          select(site, parameter, DT_round, mean = mean_final, modifications) %>%
          filter(!is.na(mean))

        pipeline$hydroshare_data <- list(raw = hs_raw, clean = hs_clean)

        setProgress(0.9, detail = "Updating UI...")

        sites <- unique(hs_clean$site)
        updateSelectInput(session, "preview_site", choices = sites)

        setProgress(1.0, detail = "Complete!")

        output$prep_status <- renderUI({
          div(class = "alert alert-success", icon("check-circle"), "Datasets prepared for export!")
        })
      })
    })

    # Preview plot
    output$export_plot <- renderPlotly({
      req(pipeline$hydroshare_data, input$preview_site)

      site_df <- pipeline$hydroshare_data$clean %>%
        filter(site == input$preview_site)

      if (nrow(site_df) == 0) return(plotly_empty())

      p <- ggplot(site_df, aes(x = DT_round, y = mean, color = parameter)) +
        geom_line(alpha = 0.8) +
        facet_wrap(~parameter, scales = "free_y", ncol = 1) +
        labs(title = paste("Cleaned Data:", input$preview_site), x = "Date (UTC)", y = "Value") +
        theme_minimal() +
        theme(legend.position = "none")

      ggplotly(p)
    })

    # Save exports
    observeEvent(input$save_export, {
      req(pipeline$hydroshare_data)

      export_path <- if (!is.integer(input$export_dir)) {
        parseDirPath(volumes, input$export_dir)
      } else {
        NULL
      }

      if (is.null(export_path) || export_path == "") {
        showNotification("Please select an export directory", type = "error")
        return()
      }

      withProgress(message = "Saving export files...", value = 0, {
        timestamp <- format(Sys.Date(), "%Y%m%d")

        # Save raw comprehensive dataset
        write_csv(pipeline$hydroshare_data$raw,
                  file.path(export_path, paste0("sonde_network_all_data_", timestamp, ".csv")))

        setProgress(0.4)

        # Save cleaned dataset
        write_csv(pipeline$hydroshare_data$clean,
                  file.path(export_path, paste0("sonde_network_clean_", timestamp, ".csv")))

        setProgress(0.8)

        # Save field notes if available
        if (!is.null(pipeline$field_notes)) {
          clean_notes <- pipeline$field_notes %>%
            select(site, DT_round, type, parameter_clean, user = initial) %>%
            arrange(DT_round, site)

          write_csv(clean_notes,
                    file.path(export_path, paste0("sonde_network_field_notes_", timestamp, ".csv")))
        }

        setProgress(1.0)
      })

      showNotification(paste("Successfully exported to", export_path), type = "message")
    })
  })
}
