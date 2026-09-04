##### GLOBAL OPTIONS #####
# Unified Sonde Network Tool
# Consolidates all packages and shared configuration from:
# - poudre_sonde_network/global.R
# - manual_verification_tool/global.R
# - calibration_correction_tool/global.R
# - drift_correction_tool/global.R

# Package loader function
package_loader <- function(x) {
  if (x %in% installed.packages()) {
    suppressMessages({
      library(x, character.only = TRUE)
    })
  } else {
    suppressMessages({
      install.packages(x)
      library(x, character.only = TRUE)
    })
  }
}

# Load all required packages
invisible(
  lapply(c(
    # Shiny & UI
    "shiny",
    "bslib",
    "DT",
    "plotly",
    "shinyFiles",
    "shinyWidgets",
    "keys",
    # Data manipulation
    "tidyverse",
    "lubridate",
    "data.table",
    "arrow",
    "zoo",
    "padr",
    "RcppRoll",
    # Plotting
    "ggplot2",
    "ggpubr",
    "patchwork",
    # File I/O
    "yaml",
    "here",
    "readxl",
    "writexl",
    "digest",
    "fs",
    "glue",
    "anytime",
    # Parallel processing
    "furrr",
    "future",
    # In-house tools
    "ross.wq.tools",
    # CDWR streamflow
    "cdssr"
  ),
  package_loader)
)

# Suppress scientific notation for consistent formatting
options(scipen = 999)

# Allow large file uploads (10GB)
options(shiny.maxRequestSize = 10000 * 1024^2)

# Auto-source all R files in R/utils/
options(shiny.autoload.r = FALSE) # We handle sourcing ourselves

# Negation operator
`%nin%` <- Negate(`%in%`)

# Set up parallel processing
num_workers <- min(availableCores() - 1, 4)
plan(multisession, workers = num_workers)
set_furr_options <- furrr_options(
  globals = TRUE,
  packages = c("arrow", "data.table", "httr2", "tidyverse", "dplyr", "lubridate", "zoo",
               "padr", "stats", "RcppRoll", "yaml", "here", "ross.wq.tools")
)

##### Color Palettes #####

site_color_combo <- tibble(
  site = c("joei", "cbri", "chd", "pfal", "sfm", "pbr", "pman", "pbd",
           "bellvue", "salyer", "udall", "riverbend_virridy", "riverbend",
           "cottonwood_virridy", "cottonwood", "elc", "archery_virridy",
           "archery", "boxcreek", "springcreek", "riverbluffs"),
  color = c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD",
            "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA",
            "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77",
            "#771122", "#AA4455", "#DD7788")
)

final_status_colors <- c(
  "PASS" = "#008a18",
  "OMIT" = "#ff1100",
  "FLAGGED" = "#ff8200",
  "NA" = "grey"
)

##### Default Parameters #####
default_parameters <- c(
  "Specific Conductivity", "Temperature", "DO", "pH", "ORP", "Depth",
  "Chl-a Fluorescence", "Turbidity", "FDOM Fluorescence"
)

##### Source utility helpers #####
utils_dir <- file.path("R", "utils")
if (dir.exists(utils_dir)) {
  walk(list.files(utils_dir, pattern = "\\.R$", full.names = TRUE), source)
}
