##### Step 6: Manual Verification Module #####
# Ported from manual_verification_tool (1718 lines)
# Week-by-week data verification with brush selection and keyboard shortcuts

step6_ui <- function(id) {
  ns <- NS(id)

  # Keyboard shortcut bindings
  keys_js <- tagList(
    keys::useKeys(),
    keys::keysInput(ns("shortcut_keys"), c("a", "d", "w", "s", "left", "right"))
  )

  tagList(
    keys_js,
    navset_card_tab(
      id = ns("tabs"),
      #### Data Selection Tab ####
      nav_panel("Data Selection",
        fluidRow(
          column(4,
            card(
              card_header("Select Data"),
              card_body(
                selectInput(ns("site"), "Site:", choices = NULL),
                selectInput(ns("parameter"), "Parameter:", choices = NULL),
                textInput(ns("user"), "Reviewer Name:", placeholder = "Your initials"),
                hr(),
                actionButton(ns("load_verification"), "Load Site-Parameter Data",
                             class = "btn-primary w-100")
              )
            )
          ),
          column(8,
            card(
              card_header("File Status"),
              card_body(
                DT::dataTableOutput(ns("file_status_table"))
              )
            )
          )
        ),
        hr(),
        # Upload new data directly
        card(
          card_header("Upload Data for Verification (Alternative)"),
          card_body(
            fileInput(ns("upload_verification_data"), "Upload Flagged Data Files:",
                      multiple = TRUE,
                      accept = c(".parquet", ".csv", ".feather")),
            actionButton(ns("process_upload"), "Process Uploaded Data",
                         class = "btn-warning w-100")
          )
        )
      ),

      #### Data Verification Tab ####
      nav_panel("Data Verification",
        fluidRow(
          # Main plot area
          column(9,
            card(
              card_header(
                fluidRow(
                  column(6, textOutput(ns("week_header"))),
                  column(6,
                    div(class = "d-flex justify-content-end gap-2",
                      actionButton(ns("prev_week"), "← Prev", class = "btn-sm btn-outline-secondary"),
                      actionButton(ns("reset_week"), "Reset", class = "btn-sm btn-outline-secondary"),
                      actionButton(ns("next_week"), "Next →", class = "btn-sm btn-outline-secondary")
                    )
                  )
                )
              ),
              card_body(
                plotOutput(ns("main_plot"), height = "450px",
                           brush = brushOpts(
                             id = ns("plot_brush"),
                             direction = "x",
                             resetOnNew = FALSE
                           ))
              )
            ),
            # Sub-plots
            card(
              card_header("Additional Plots"),
              card_body(
                plotlyOutput(ns("sub_plots"), height = "300px")
              )
            )
          ),

          # Sidebar
          column(3,
            # Brush actions
            card(
              card_header("Brush Actions"),
              card_body(
                actionButton(ns("brush_accept"), "Accept Selected", class = "btn-success w-100 mb-1"),
                div(class = "d-flex gap-1 mb-1",
                  selectInput(ns("flag_type"), label = NULL,
                              choices = c("Select flag..." = "", "suspect data", "drift",
                                          "sensor malfunction", "interference"),
                              width = "60%"),
                  actionButton(ns("brush_flag"), "Flag", class = "btn-warning", style = "width: 38%")
                ),
                actionButton(ns("brush_omit"), "Omit Selected", class = "btn-danger w-100 mb-1"),
                actionButton(ns("brush_clear"), "Clear Selection", class = "btn-outline-secondary w-100")
              )
            ),
            # Weekly decision
            card(
              card_header("Weekly Decision"),
              card_body(
                radioButtons(ns("weekly_decision"), label = NULL,
                  choices = c(
                    "Select Decision" = "s",
                    "Accept All" = "aa",
                    "Accept Non-Omit" = "ano",
                    "Keep Flags" = "kf",
                    "Omit Flagged" = "of",
                    "Omit All" = "oa"
                  ),
                  selected = "s"
                ),
                actionButton(ns("submit_week"), "Submit Week Decision",
                             class = "btn-primary w-100")
              )
            ),
            # Plot options
            card(
              card_header("Plot Options"),
              card_body(
                checkboxGroupInput(ns("plot_options"), label = NULL,
                  choices = c(
                    "Remove Omitted" = "remove_omit",
                    "Remove Flagged" = "remove_flag",
                    "Plot Line" = "plot_line",
                    "Show Thresholds" = "thresholds",
                    "Log10 Scale" = "log10",
                    "Extra Days" = "extra_days",
                    "Show Legend" = "show_legend"
                  ),
                  selected = c("extra_days", "show_legend")
                )
              )
            ),
            # Additional sites
            card(
              card_header("Compare Sites"),
              card_body(
                checkboxGroupInput(ns("add_sites"), "Add Sites:", choices = NULL)
              )
            )
          )
        )
      ),

      #### Finalize Data Tab ####
      nav_panel("Finalize Data",
        fluidRow(
          column(9,
            card(
              card_header("Complete Dataset Overview"),
              card_body(
                plotlyOutput(ns("final_plot"), height = "500px")
              )
            )
          ),
          column(3,
            card(
              card_header("Final Options"),
              card_body(
                selectInput(ns("final_week_selection"), "Jump to Week:", choices = NULL),
                actionButton(ns("goto_final_week"), "Go to Week", class = "btn-info w-100"),
                hr(),
                checkboxInput(ns("remove_omit_finalplot"), "Remove Omitted Data", FALSE),
                checkboxInput(ns("log10_finalplot"), "Log10 Scale", FALSE),
                hr(),
                uiOutput(ns("submit_final_button"))
              )
            ),
            card(
              card_header("Unfinalize"),
              card_body(
                actionButton(ns("unfinalize"), "Unfinalize Current", class = "btn-outline-danger w-100"),
                helpText("Move finalized data back to intermediary")
              )
            )
          )
        ),
        hr(),
        actionButton(ns("continue_step6"), "Continue to Drift Correction →",
                     class = "btn-success btn-lg w-100")
      )
    )
  )
}

step6_server <- function(id, pipeline, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #### Reactive Values ####
    selected_data <- reactiveVal(NULL)
    current_week <- reactiveVal(NULL)
    selected_data_cur_filename <- reactiveVal(NULL)
    all_datasets <- reactiveVal(NULL)
    global_usgs_flow_data <- reactiveVal(NULL)

    #### Data Selection Tab ####

    # Update site/parameter choices from directory contents
    observe({
      req(pipeline$pre_verification_path)

      filenames <- get_filenames(
        all_path = pipeline$all_data_path,
        pre_path = pipeline$pre_verification_path,
        int_path = pipeline$intermediary_path,
        ver_path = pipeline$verified_path
      )

      if (nrow(filenames) > 0) {
        parsed <- split_filename(filenames$filename)
        sites <- unique(parsed$site)
        updateSelectInput(session, "site", choices = sites)
      }
    })

    # Update parameter choices when site changes
    observeEvent(input$site, {
      req(pipeline$pre_verification_path, input$site)

      filenames <- get_filenames(
        all_path = pipeline$all_data_path,
        pre_path = pipeline$pre_verification_path,
        int_path = pipeline$intermediary_path,
        ver_path = pipeline$verified_path
      )

      if (nrow(filenames) > 0) {
        parsed <- split_filename(filenames$filename)
        params <- parsed %>% filter(site == input$site) %>% pull(parameter) %>% unique()
        updateSelectInput(session, "parameter", choices = params)
      }

      # Update comparison sites
      compare_sites <- relevant_sonde_selector(input$site)
      updateCheckboxGroupInput(session, "add_sites", choices = compare_sites)
    })

    # File status table
    output$file_status_table <- DT::renderDataTable({
      req(pipeline$pre_verification_path)

      filenames <- get_filenames(
        all_path = pipeline$all_data_path,
        pre_path = pipeline$pre_verification_path,
        int_path = pipeline$intermediary_path,
        ver_path = pipeline$verified_path
      )

      if (nrow(filenames) > 0) {
        parsed <- split_filename(filenames$filename) %>%
          bind_cols(tibble(directory = filenames$directory))

        DT::datatable(parsed %>% select(site, parameter, directory, datetime),
                      options = list(pageLength = 20, scrollY = "300px")) %>%
          DT::formatStyle("directory",
                          backgroundColor = DT::styleEqual(
                            c("all_data", "pre_verification", "intermediary", "verified"),
                            c("#f8f9fa", "#fff3cd", "#cce5ff", "#d4edda")
                          ))
      }
    })

    # Load verification data
    observeEvent(input$load_verification, {
      req(input$site, input$parameter)

      # Determine which directory has the data
      site <- input$site
      param <- input$parameter
      param_file <- gsub(" ", "_", param)
      search_pattern <- paste0("^", site, "-", param_file)

      # Check directories in priority order: intermediary > pre_verification > verified
      data_file <- NULL
      source_dir <- NULL

      for (dir_info in list(
        list(path = pipeline$intermediary_path, name = "intermediary"),
        list(path = pipeline$pre_verification_path, name = "pre_verification"),
        list(path = pipeline$verified_path, name = "verified")
      )) {
        if (!is.null(dir_info$path) && dir.exists(dir_info$path)) {
          files <- list.files(dir_info$path, pattern = search_pattern, full.names = TRUE)
          if (length(files) > 0) {
            data_file <- files[length(files)]  # Most recent
            source_dir <- dir_info$name
            break
          }
        }
      }

      if (is.null(data_file)) {
        showNotification("No data found for this site-parameter", type = "error")
        return()
      }

      # Load the data
      df <- read_parquet(data_file)
      selected_data(df)
      selected_data_cur_filename(basename(data_file))

      # Set current week
      weeks <- sort(unique(df$week))
      first_unverified <- df %>% filter(is.na(is_verified) | !is_verified) %>% pull(week) %>% min(na.rm = TRUE)
      current_week(if (is.finite(first_unverified)) first_unverified else weeks[1])

      # Load all datasets for comparison
      all_data <- list(
        pre_verification_data = load_all_parquet(pipeline$pre_verification_path),
        intermediary_data = load_all_parquet(pipeline$intermediary_path),
        verified_data = load_all_parquet(pipeline$verified_path)
      )
      all_datasets(all_data)

      # Move to pre_verification → intermediary if needed
      if (source_dir == "pre_verification") {
        new_filename <- move_file_to_intermediary(
          basename(data_file), df,
          pipeline$pre_verification_path, pipeline$intermediary_path
        )
        selected_data_cur_filename(new_filename)
      }

      # Try to load USGS/CDWR flow data
      if (!is.null(pipeline$cdwr_creds) && !is.null(pipeline$site_gauge_mapping)) {
        tryCatch({
          flow <- cdssr::get_sw_ts(
            abbrev = unique(pipeline$site_gauge_mapping$station_abbrev),
            start_date = format(pipeline$date_start, "%m-%d-%Y"),
            end_date = format(pipeline$date_end, "%m-%d-%Y"),
            timescale = "day"
          )
          global_usgs_flow_data(flow)
        }, error = function(e) {
          showNotification(paste("Flow data not available:", e$message), type = "warning")
        })
      }

      showNotification(paste("Loaded:", site, param, "from", source_dir), type = "message")
      updateTabsetPanel(session, "tabs", selected = "Data Verification")
    })

    # Process uploaded data
    observeEvent(input$process_upload, {
      req(input$upload_verification_data, pipeline$all_data_path)

      result <- setup_directories_from_upload(
        uploaded_file_paths = input$upload_verification_data$datapath,
        timezone = "UTC",
        all_path = pipeline$all_data_path,
        pre_path = pipeline$pre_verification_path,
        raw_path = file.path(pipeline$in_progress_path, "raw_data"),
        meta_path = pipeline$meta_path,
        year = pipeline$year
      )

      showNotification(result, type = "message")
      session$reload()
    })

    #### Week Navigation ####
    observeEvent(input$prev_week, {
      req(selected_data(), current_week())
      weeks <- sort(unique(selected_data()$week))
      idx <- which(weeks == current_week())
      if (idx > 1) {
        current_week(weeks[idx - 1])
        updateRadioButtons(session, "weekly_decision", selected = "s")
      }
    })

    observeEvent(input$next_week, {
      req(selected_data(), current_week())
      weeks <- sort(unique(selected_data()$week))
      idx <- which(weeks == current_week())
      if (idx < length(weeks)) {
        current_week(weeks[idx + 1])
        updateRadioButtons(session, "weekly_decision", selected = "s")
      }
    })

    observeEvent(input$reset_week, {
      req(selected_data())
      weeks <- sort(unique(selected_data()$week))
      current_week(weeks[1])
      updateRadioButtons(session, "weekly_decision", selected = "s")
    })

    # Keyboard shortcuts
    observeEvent(input$shortcut_keys, {
      key <- input$shortcut_keys
      if (key == "left") {
        click("prev_week")
      } else if (key == "right") {
        click("next_week")
      } else if (key == "a") {
        click("brush_accept")
      } else if (key == "d") {
        click("brush_omit")
      } else if (key == "w") {
        click("submit_week")
      }
    })

    #### Week header ####
    output$week_header <- renderText({
      req(current_week(), selected_data())
      week_data <- selected_data() %>% filter(week == current_week())
      paste0("Week ", current_week(), " (", format(min(week_data$DT_round), "%b %d"), " - ",
             format(max(week_data$DT_round), "%b %d"), ")")
    })

    #### Main Plot ####
    output$main_plot <- renderPlot({
      req(selected_data(), current_week())

      week_data <- selected_data() %>% filter(week == current_week())
      if (nrow(week_data) == 0) return()

      week_min <- min(week_data$DT_round, na.rm = TRUE)
      week_max <- max(week_data$DT_round, na.rm = TRUE)

      # Extra days padding
      if ("extra_days" %in% input$plot_options) {
        plot_data <- selected_data() %>%
          filter(DT_round >= week_min - days(2) & DT_round <= week_max + days(2))
      } else {
        plot_data <- week_data
      }

      # Filter based on options
      if ("remove_omit" %in% input$plot_options) {
        plot_data <- plot_data %>% filter(!brush_omit)
      }
      if ("remove_flag" %in% input$plot_options) {
        plot_data <- plot_data %>% filter(is.na(user_flag) | user_flag == "")
      }

      # Handle weekly decision preview
      if (input$weekly_decision != "s") {
        week_data <- week_data %>%
          mutate(
            final_decision = case_when(
              input$weekly_decision == "aa" ~ "PASS",
              input$weekly_decision == "ano" & !brush_omit ~ "PASS",
              input$weekly_decision == "kf" & is.na(user_flag) & !brush_omit ~ "PASS",
              input$weekly_decision == "kf" & !is.na(user_flag) & !brush_omit ~ "FLAGGED",
              input$weekly_decision == "of" & is.na(user_flag) & !brush_omit ~ "PASS",
              input$weekly_decision == "of" & !is.na(user_flag) & !brush_omit ~ "OMIT",
              input$weekly_decision == "oa" ~ "OMIT",
              input$weekly_decision != "aa" & brush_omit ~ "OMIT"
            )
          )

        p <- ggplot(week_data, aes(x = DT_round, y = mean, color = final_decision)) +
          geom_point(size = 1) +
          scale_color_manual(values = c("PASS" = "#008a18", "OMIT" = "#ff1100", "FLAGGED" = "#ff8200"),
                             na.value = "grey50")
      } else {
        # Standard plot
        plot_data <- plot_data %>%
          mutate(status = case_when(
            brush_omit ~ "User Omit",
            !is.na(user_flag) ~ "Flagged",
            TRUE ~ "Normal"
          ))

        p <- ggplot(plot_data, aes(x = DT_round, y = mean, color = status)) +
          geom_point(size = 1) +
          scale_color_manual(values = c("Normal" = "grey40", "Flagged" = "#ff8200", "User Omit" = "#ff1100"),
                             na.value = "grey80")
      }

      if ("plot_line" %in% input$plot_options) {
        p <- p + geom_line(alpha = 0.5)
      }

      if ("log10" %in% input$plot_options) {
        p <- p + scale_y_log10()
      }

      # Add threshold lines
      if ("thresholds" %in% input$plot_options && !is.null(pipeline$meta_path)) {
        p <- add_threshold_lines(p, plot_data, input$site, input$parameter, pipeline$meta_path)
      }

      # Week boundary shading
      p <- p +
        annotate("rect", xmin = week_min, xmax = week_max,
                 ymin = -Inf, ymax = Inf, fill = "lightyellow", alpha = 0.3) +
        labs(title = paste(input$site, input$parameter, "— Week", current_week()),
             x = "Date", y = input$parameter, color = "Status") +
        theme_minimal() +
        theme(legend.position = if ("show_legend" %in% input$plot_options) "bottom" else "none")

      p
    })

    #### Brush Actions ####
    observeEvent(input$brush_accept, {
      req(selected_data(), input$plot_brush)
      brush <- input$plot_brush
      df <- selected_data()
      brushed <- brushedPoints(df, brush, xvar = "DT_round", yvar = "mean")

      if (nrow(brushed) > 0) {
        df <- df %>%
          mutate(
            user_flag = ifelse(DT_round %in% brushed$DT_round & week == current_week(), NA, user_flag),
            brush_omit = ifelse(DT_round %in% brushed$DT_round & week == current_week(), FALSE, brush_omit)
          )
        selected_data(df)
      }
    })

    observeEvent(input$brush_flag, {
      req(selected_data(), input$plot_brush, input$flag_type)
      if (input$flag_type == "") return()

      brush <- input$plot_brush
      df <- selected_data()
      brushed <- brushedPoints(df, brush, xvar = "DT_round", yvar = "mean")

      if (nrow(brushed) > 0) {
        df <- df %>%
          mutate(
            user_flag = ifelse(DT_round %in% brushed$DT_round & week == current_week(),
                               ifelse(is.na(user_flag), input$flag_type,
                                      paste(user_flag, input$flag_type, sep = "; ")),
                               user_flag)
          )
        selected_data(df)
      }
    })

    observeEvent(input$brush_omit, {
      req(selected_data(), input$plot_brush)
      brush <- input$plot_brush
      df <- selected_data()
      brushed <- brushedPoints(df, brush, xvar = "DT_round", yvar = "mean")

      if (nrow(brushed) > 0) {
        df <- df %>%
          mutate(brush_omit = ifelse(DT_round %in% brushed$DT_round & week == current_week(), TRUE, brush_omit))
        selected_data(df)
      }
    })

    observeEvent(input$brush_clear, {
      req(selected_data())
      df <- selected_data() %>%
        mutate(brush_omit = ifelse(week == current_week(), FALSE, brush_omit))
      selected_data(df)
    })

    #### Submit Week Decision ####
    observeEvent(input$submit_week, {
      req(selected_data(), current_week(), input$weekly_decision)
      if (input$weekly_decision == "s") {
        showNotification("Please select a weekly decision first", type = "warning")
        return()
      }

      df <- selected_data()
      week_data <- df %>% filter(week == current_week())

      # Apply decision
      updated_week <- week_data %>%
        mutate(
          is_verified = TRUE,
          week_decision = input$weekly_decision,
          user = input$user,
          final_status = case_when(
            input$weekly_decision == "aa" ~ "PASS",
            input$weekly_decision == "ano" & !brush_omit ~ "PASS",
            input$weekly_decision == "kf" & is.na(user_flag) & !brush_omit ~ "PASS",
            input$weekly_decision == "kf" & !is.na(user_flag) & !brush_omit ~ "FLAGGED",
            input$weekly_decision == "of" & is.na(user_flag) & !brush_omit ~ "PASS",
            input$weekly_decision == "of" & !is.na(user_flag) & !brush_omit ~ "OMIT",
            input$weekly_decision == "oa" ~ "OMIT",
            input$weekly_decision != "aa" & brush_omit ~ "OMIT"
          ),
          verification_status = final_status,
          mean_verified = case_when(
            final_status %in% c("PASS", "FLAGGED") ~ mean,
            TRUE ~ NA_real_
          )
        )

      other_data <- df %>% filter(week != current_week())
      selected_data(bind_rows(other_data, updated_week) %>% arrange(DT_round))

      # Save to intermediary
      selected_data_cur_filename(
        update_intermediary_data(selected_data_cur_filename(), selected_data(), pipeline$intermediary_path)
      )

      # Advance to next week
      weeks <- sort(unique(selected_data()$week))
      idx <- which(weeks == current_week())

      if (all(!is.na(selected_data()$final_status))) {
        showNotification("All weeks reviewed! Go to Finalize tab.", type = "message")
        updateTabsetPanel(session, "tabs", selected = "Finalize Data")
      } else if (idx == length(weeks)) {
        # Find earliest unverified week
        min_unverified <- selected_data() %>% filter(is.na(is_verified) | !is_verified) %>% pull(week) %>% min(na.rm = TRUE)
        current_week(min_unverified)
        showNotification("Reached end. Moving to earliest unverified week.", type = "warning")
      } else {
        current_week(weeks[idx + 1])
      }

      updateRadioButtons(session, "weekly_decision", selected = "s")
      showNotification(paste("Decision", toupper(input$weekly_decision), "submitted"), type = "message")
    })

    #### Sub Plots ####
    output$sub_plots <- renderPlotly({
      req(selected_data(), current_week(), input$add_sites)

      week_data <- selected_data() %>% filter(week == current_week())
      week_min <- min(week_data$DT_round) - days(2)
      week_max <- max(week_data$DT_round) + days(2)

      # Get flow data if available
      flow_reactive <- reactive({
        if (!is.null(global_usgs_flow_data()) && !is.null(pipeline$site_gauge_mapping)) {
          gauge <- pipeline$site_gauge_mapping %>%
            filter(site == input$site) %>%
            pull(station_abbrev)

          if (length(gauge) > 0) {
            col_name <- if ("station_abbrev" %in% names(global_usgs_flow_data())) "station_abbrev" else "abbrev"
            global_usgs_flow_data() %>%
              filter(!!sym(col_name) %in% gauge,
                     datetime >= week_min, datetime <= week_max)
          }
        }
      })

      flow_data <- flow_reactive()

      if (!is.null(flow_data) && nrow(flow_data) > 0) {
        p <- plot_ly(flow_data, x = ~datetime, y = ~value, type = 'scatter', mode = 'lines',
                     name = "Streamflow (cfs)") %>%
          layout(title = "Streamflow", xaxis = list(title = "Date"), yaxis = list(title = "Discharge (cfs)"))
        return(p)
      }

      plotly_empty() %>% layout(title = "No comparison data available")
    })

    #### Final Plot ####
    output$final_plot <- renderPlotly({
      req(selected_data())

      final_plot_data <- selected_data()

      if (input$remove_omit_finalplot) {
        final_plot_data <- final_plot_data %>%
          filter(final_status != "OMIT" | is.na(final_status))
      }

      final_plot_data$final_status <- ifelse(is.na(final_plot_data$final_status), "NA", final_plot_data$final_status)
      final_plot_data$final_status <- as.factor(final_plot_data$final_status)

      year <- min(final_plot_data$year, na.rm = TRUE)
      start_date <- min(final_plot_data$DT_round, na.rm = TRUE)
      end_date <- max(final_plot_data$DT_round, na.rm = TRUE)

      vline_dates <- seq(as.POSIXct(paste0(year, "-01-01")),
                         as.POSIXct(paste0(year, "-12-31")), by = "week") %>%
        keep(~ .x >= start_date & .x <= end_date)

      p <- plot_ly(final_plot_data, x = ~DT_round, y = ~mean, type = 'scatter', mode = 'markers',
                   color = ~final_status,
                   colors = c("PASS" = "#008a18", "OMIT" = "#ff1100", "FLAGGED" = "#ff8200", "NA" = "grey"),
                   text = ~paste0("Week ", week, "\nStatus: ", final_status),
                   hoverinfo = "text") %>%
        layout(
          title = paste0("Complete Dataset: ", input$site, "-", input$parameter),
          xaxis = list(title = "Date"),
          yaxis = list(title = input$parameter,
                       type = if (input$log10_finalplot) "log" else "linear"),
          shapes = lapply(vline_dates, function(d) {
            list(type = "line", x0 = d, x1 = d, y0 = 0, y1 = 1,
                 xref = "x", yref = "paper", line = list(color = "black", width = 0.5))
          })
        )
      p
    })

    # Week selection for final tab
    observe({
      req(selected_data())
      weeks <- sort(unique(selected_data()$week))
      updateSelectInput(session, "final_week_selection", choices = weeks)
    })

    observeEvent(input$goto_final_week, {
      req(input$final_week_selection)
      current_week(as.numeric(input$final_week_selection))
      updateTabsetPanel(session, "tabs", selected = "Data Verification")
    })

    # Submit final button
    output$submit_final_button <- renderUI({
      all_verified <- FALSE
      if (!is.null(selected_data())) {
        all_verified <- all(!is.na(selected_data()$final_status))
      }

      if (all_verified) {
        actionButton(ns("submit_final"), "Submit Finalized Dataset", class = "btn-success w-100")
      } else {
        actionButton(ns("submit_final"), "Submit Finalized Dataset",
                     class = "btn-success w-100 disabled", disabled = TRUE)
      }
    })

    # Handle final submission
    observeEvent(input$submit_final, {
      update_finalized <- selected_data() %>% mutate(is_finalized = TRUE)

      final_name <- move_file_to_verified(
        selected_data_cur_filename(), update_finalized,
        pipeline$intermediary_path, pipeline$verified_path
      )

      showNotification(paste0(input$site, "-", input$parameter, " finalized: ", final_name), type = "message")
      updateTabsetPanel(session, "tabs", selected = "Data Selection")

      # Reset
      selected_data(NULL)
      selected_data_cur_filename(NULL)
    })

    # Unfinalize
    observeEvent(input$unfinalize, {
      req(input$site, input$parameter)
      tryCatch({
        unfinalize_site_parameter(input$site, input$parameter,
                                  pipeline$verified_path, pipeline$intermediary_path)
        showNotification("Data unfinalized and moved to intermediary", type = "message")
      }, error = function(e) {
        showNotification(paste("Unfinalize failed:", e$message), type = "error")
      })
    })

    # Continue
    observeEvent(input$continue_step6, {
      pipeline$step_complete$step6 <- TRUE
      updateNavbarPage(parent_session, "main_tabs", selected = "step7")
    })
  })
}


#### Helper: Load all parquet files from a directory into a named list ####
load_all_parquet <- function(dir_path) {
  if (is.null(dir_path) || !dir.exists(dir_path)) return(list())

  files <- list.files(dir_path, pattern = "\\.parquet$", full.names = TRUE)
  if (length(files) == 0) return(list())

  result <- map(files, read_parquet)
  # Name by site-parameter
  names(result) <- map_chr(files, function(f) {
    parsed <- split_filename(basename(f))
    paste0(parsed$site, "-", gsub(" ", "_", parsed$parameter))
  })
  result
}
