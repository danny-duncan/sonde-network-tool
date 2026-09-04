##### Server Script #####
# Unified Sonde Network Tool — Master Server
# Manages inter-step data flow and step gating via shared reactiveValues

server <- function(input, output, session) {

  ##### Shared Pipeline State #####
  # Central reactive values store that all modules read/write
  pipeline <- reactiveValues(
    # Step 0: Configuration
    year = NULL,
    year_cycle = NULL,
    date_start = NULL,
    date_end = NULL,
    parameters = NULL,
    sites = NULL,
    data_source = NULL,       # "upload" or "hydrovu"
    field_note_source = NULL,  # "upload", "mwater", or "none"

    # Credentials
    mwater_creds = NULL,
    hv_creds = NULL,
    cdwr_creds = NULL,

    # Threshold/config files
    sensor_thresholds = NULL,
    seasonal_thresholds = NULL,
    site_order = NULL,
    site_gauge_mapping = NULL,  # maps WQ sites to streamflow gauges

    # Step 1: Data Pull outputs
    field_notes = NULL,
    malfunction_notes = NULL,
    hv_token = NULL,

    # Step 2: Loaded & tidied data
    tidy_data = NULL,  # list of site-parameter data frames

    # Step 3: Calibration outputs
    calibration_data = NULL,
    calibrated_data = NULL,

    # Step 4: Flagging outputs
    flagged_data = NULL,

    # Step 5: Save location
    flagged_save_path = NULL,

    # Step 6: Verification working directory
    verification_base_path = NULL,
    in_progress_path = NULL,
    all_data_path = NULL,
    pre_verification_path = NULL,
    intermediary_path = NULL,
    verified_path = NULL,
    meta_path = NULL,

    # Step 7: Drift correction outputs
    drift_corrected_data = NULL,
    post_ver_path = NULL,

    # Step 8: Export outputs
    hydroshare_data = NULL,
    export_path = NULL,

    # Step completion tracking
    step_complete = list(
      step0 = FALSE,
      step1 = FALSE,
      step2 = FALSE,
      step3 = FALSE,
      step4 = FALSE,
      step5 = FALSE,
      step6 = FALSE,
      step7 = FALSE,
      step8 = FALSE
    )
  )

  ##### Call Step Module Servers #####
  step0_server("step0", pipeline, session)
  step1_server("step1", pipeline, session)
  step2_server("step2", pipeline, session)
  step3_server("step3", pipeline, session)
  step4_server("step4", pipeline, session)
  step5_server("step5", pipeline, session)
  step6_server("step6", pipeline, session)
  step7_server("step7", pipeline, session)
  step8_server("step8", pipeline, session)
}
