
# =========================================================
# WILDLIFE POPULATION DEMOGRAPHIC DASHBOARD
# MAIN APPLICATION
# =========================================================

packages <- c(
  "shiny",
  "shinydashboard",
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "DT",
  "stringr",
  "lubridate",
  "scales"
)

installed <- rownames(installed.packages())

for (package in packages) {
  if (!package %in% installed) {
    install.packages(package)
  }
}

library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(stringr)
library(lubridate)
library(scales)

# Load UI and Server
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

# Run application
shinyApp(
  ui = ui,
  server = server
)

