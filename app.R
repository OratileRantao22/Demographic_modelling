#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# ============================================================
# WILDLIFE POPULATION OVERVIEW
# MAIN APPLICATION FILE
# ============================================================

# Load required packages

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

source("ui.R")
source("server.R")

# Launch application

shinyApp(
  ui = ui,
  server = server
)
