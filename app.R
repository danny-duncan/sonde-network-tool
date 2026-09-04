# Sonde Network Tool — Entry Point
source("global.R")

shinyApp(
  ui = source("ui.R", local = TRUE)$value,
  server = source("server.R", local = TRUE)$value
)
