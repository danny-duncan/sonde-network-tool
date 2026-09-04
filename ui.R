##### UI Script #####
# Unified Sonde Network Tool — Master UI
# Step-wizard navigation with 9 panels (Setup + 8 pipeline steps)

# Source module UI functions
walk(list.files("R", pattern = "^mod_step.*\\.R$", full.names = TRUE), source)

ui <- page_navbar(
  title = "Sonde Network Tool",
  id = "main_tabs",
  theme = bs_theme(
    preset = "bootstrap",
    primary = "#0d6efd",
    success = "#008a18",
    warning = "#ff8200",
    danger = "#ff1100",
    "navbar-bg" = "#1a1a2e",
    "body-bg" = "#f8f9fa"
  ),

  # Dark mode toggle in navbar

nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  ),

  #### Step 0: Setup & Configuration ####
  nav_panel(
    title = "Setup",
    icon = icon("gear"),
    value = "step0",
    step0_ui("step0")
  ),

  #### Step 1: Data Pull ####
  nav_panel(
    title = "Data Pull",
    icon = icon("cloud-download-alt"),
    value = "step1",
    step1_ui("step1")
  ),

  #### Step 2: Load & Tidy ####
  nav_panel(
    title = "Load & Tidy",
    icon = icon("broom"),
    value = "step2",
    step2_ui("step2")
  ),

  #### Step 3: Calibration ####
  nav_panel(
    title = "Calibration",
    icon = icon("sliders-h"),
    value = "step3",
    step3_ui("step3")
  ),

  #### Step 4: Flagging ####
  nav_panel(
    title = "Flagging",
    icon = icon("flag"),
    value = "step4",
    step4_ui("step4")
  ),

  #### Step 5: Save Flagged ####
  nav_panel(
    title = "Save Flagged",
    icon = icon("save"),
    value = "step5",
    step5_ui("step5")
  ),

  #### Step 6: Verification ####
  nav_panel(
    title = "Verification",
    icon = icon("check-double"),
    value = "step6",
    step6_ui("step6")
  ),

  #### Step 7: Drift Correction ####
  nav_panel(
    title = "Drift Correction",
    icon = icon("chart-line"),
    value = "step7",
    step7_ui("step7")
  ),

  #### Step 8: HydroShare Export ####
  nav_panel(
    title = "Export",
    icon = icon("upload"),
    value = "step8",
    step8_ui("step8")
  )
)
