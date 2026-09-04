# Poudre Sonde Network Tool

This is a unified Shiny application for the Poudre Sonde Network data processing pipeline. It consolidates the workflow previously spread across four separate tools and scripts:
1. `Process_2025.Rmd` (Data pull and QA/QC flagging)
2. `calibration_correction_tool` (Manual calibration review)
3. `manual_verification_tool` (Manual data review and flagging)
4. `drift_correction_tool` (Mathematical drift correction)

## Overview

The application is structured as a step-by-step wizard containing the following modules:

- **Step 0: Setup**: Configure inputs, choose data sources, and provide credentials/metadata files.
- **Step 1: Data Pull**: Automatically retrieve sensor data from the HydroVu API and field notes/malfunction records from the mWater API.
- **Step 2: Load & Tidy**: Ingest raw data (from API or direct upload), parse it, and preview.
- **Step 3: Calibration**: Interactively review and apply lag/lead back-calibration to the data using calibration reports.
- **Step 4: Automated Flagging**: Run the comprehensive automated QA/QC flagging pipeline (field notes, single-sensor, intra-sensor, and network checks).
- **Step 5: Save Flagged Dataset**: Export the flagged dataset and prepare directory scaffolding for manual verification.
- **Step 6: Manual Verification**: Review data week-by-week using a brush tool to omit, flag, or accept data points with streamflow overlay support.
- **Step 7: Drift Correction**: Interactively identify FDOM/Turbidity drift windows and apply mathematical corrections (linear, exponential, etc.).
- **Step 8: Export**: Combine all verified data, format it to UTC, and export a clean version (for publication) and raw version (for archiving).

## Installation

Ensure you have R and the necessary packages installed. This package relies heavily on the `ross.wq.tools` package.

```R
# Install dependencies
install.packages(c("shiny", "bslib", "tidyverse", "plotly", "DT", "shinyFiles", "shinyWidgets", "keys", "yaml", "anytime", "future", "furrr"))

# Run the app
shiny::runApp()
```

## Setup Requirements

Before running, you may need the following configuration files depending on your chosen data sources:
- **HydroVu Credentials** (`.yaml`)
- **mWater Credentials** (`.yaml`)
- **CDWR API Key** (`.yaml` - required for streamflow overlay in Step 6)
- **Site-to-Gauge Mapping** (`.csv` - maps sonde sites to USGS/CDWR stream gauges)
- **Thresholds** (`.yml` for sensor thresholds, `.csv` for seasonal)
- **Site List** (`.csv` or `.yml`)
- **Site Order** (`.yml` - for network checks)
- **Calibration Data** (`.RDS` - required if applying back-calibration in Step 3)
