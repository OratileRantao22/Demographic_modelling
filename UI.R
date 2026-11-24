
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(scales)

ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(
    title = "Wildlife Population Dashboard",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    
    sidebarMenu(
      menuItem("Population Overview", tabName = "overview",
               icon = icon("chart-pie")),
      menuItem("Demographics", tabName = "demographics",
               icon = icon("users")),
      menuItem("Age Analysis", tabName = "age",
               icon = icon("chart-line")),
      menuItem("Animal Register", tabName = "register",
               icon = icon("paw"))
    ),
    
    tags$hr(),
    
    h4("Register Filters", style = "padding-left: 15px;"),
    
    selectInput(
      "sex_filter",
      "Sex",
      choices = c("All", "Male", "Female", "Unclassified", "Unknown"),
      selected = "All"
    ),
    
    selectInput(
      "age_class_filter",
      "Age Class",
      choices = "All",
      selected = "All"
    ),
    
    selectInput(
      "age_category_filter",
      "Age Category",
      choices = "All",
      selected = "All"
    ),
    
    tags$hr(),
    
    helpText(
      "Add animal records through the Animal Register page.",
      style = "padding: 10px; color: white;"
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f8f3;
        }

        .skin-green .main-sidebar {
          background-color: #174d32;
        }

        .skin-green .main-header .logo {
          background-color: #174d32;
          color: white;
          font-weight: bold;
        }

        .skin-green .main-header .navbar {
          background-color: #2e8b57;
        }

        .skin-green .sidebar-menu > li.active > a {
          background-color: #2e8b57;
          border-left-color: #a5d6a7;
        }

        .skin-green .sidebar-menu > li > a:hover {
          background-color: #236b45;
        }

        .box {
          border-top: 3px solid #4CAF50;
        }

        .box-header {
          color: #174d32;
          font-weight: bold;
        }

        .small-box {
          border-radius: 5px;
        }

        .register-instructions {
          background-color: #e8f5e9;
          padding: 12px;
          border-left: 4px solid #4CAF50;
          margin-bottom: 15px;
        }
      "))
    ),
    
    tabItems(
      
      # =====================================================
      # PAGE 1: POPULATION OVERVIEW
      # =====================================================
      
      tabItem(
        tabName = "overview",
        
        h2("Wildlife Population Overview"),
        
        fluidRow(
          valueBoxOutput("total_animals", width = 3),
          valueBoxOutput("male_animals", width = 3),
          valueBoxOutput("female_animals", width = 3),
          valueBoxOutput("unknown_animals", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Population by Sex",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            plotOutput("sex_plot", height = 300)
          ),
          
          box(
            title = "Population by Age Class",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            plotOutput("age_class_plot", height = 300)
          )
        ),
        
        fluidRow(
          box(
            title = "Population by Age Category",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            plotOutput("age_category_plot", height = 300)
          ),
          
          box(
            title = "Notch Status",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            plotOutput("notch_plot", height = 300)
          )
        )
      ),
      
      # =====================================================
      # PAGE 2: DEMOGRAPHICS
      # =====================================================
      
      tabItem(
        tabName = "demographics",
        
        h2("Population Demographic Analysis"),
        
        fluidRow(
          box(
            title = "Sex Distribution",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            plotOutput("demographic_sex_plot", height = 350)
          ),
          
          box(
            title = "Sex Distribution by Age Class",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            plotOutput("sex_age_class_plot", height = 350)
          )
        ),
        
        fluidRow(
          box(
            title = "Age Category by Sex",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            plotOutput("age_category_sex_plot", height = 350)
          )
        )
      ),
      
      # =====================================================
      # PAGE 3: AGE ANALYSIS
      # =====================================================
      
      tabItem(
        tabName = "age",
        
        h2("Animal Age Analysis"),
        
        fluidRow(
          valueBoxOutput("average_age", width = 4),
          valueBoxOutput("youngest_age", width = 4),
          valueBoxOutput("oldest_age", width = 4)
        ),
        
        fluidRow(
          box(
            title = "Age Distribution",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            plotOutput("age_histogram", height = 350)
          ),
          
          box(
            title = "Births by Year",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            plotOutput("births_by_year", height = 350)
          )
        )
      ),
      
      # =====================================================
      # PAGE 4: ANIMAL REGISTER
      # =====================================================
      
      tabItem(
        tabName = "register",
        
        h2("Individual Animal Register"),
        
        
        # =====================================================
        # EXCEL WORKBOOK UPLOAD
        # =====================================================
        
        fluidRow(
          box(
            title = "Upload Excel Workbook",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            
            fileInput(
              "excel_file",
              "Choose Excel Workbook",
              accept = c(".xlsx", ".xls"),
              width = "100%"
            ),
            
            helpText(
              "Upload an Excel workbook containing wildlife population information."
            ),
            
            textOutput("workbook_status"),
            
            selectInput(
              "workbook_sheet",
              "Select Worksheet to Preview",
              choices = c("No workbook uploaded" = "")
            ),
            
            DTOutput("workbook_preview")
          )
        ),
        
        
        
        div(
          class = "register-instructions",
          strong("Important: "),
          "This workbook does not contain an individual animal register. ",
          "Use the form below to create and maintain individual animal records. ",
          "Records added here are held in the current app session. Download your ",
          "register regularly to keep a copy."
        ),
        
        fluidRow(
          box(
            title = "Add a New Animal",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            
            fluidRow(
              column(
                width = 4,
                textInput(
                  "entry_id",
                  "Animal ID / Name *",
                  placeholder = "Enter animal ID or name"
                )
              ),
              
              column(
                width = 4,
                selectInput(
                  "entry_sex",
                  "Sex *",
                  choices = c(
                    "Male",
                    "Female",
                    "Unclassified",
                    "Unknown"
                  ),
                  selected = "Unknown"
                )
              ),
              
              column(
                width = 4,
                dateInput(
                  "entry_dob",
                  "Date of Birth",
                  value = NULL,
                  format = "yyyy-mm-dd",
                  startview = "decade"
                )
              )
            ),
            
            fluidRow(
              column(
                width = 4,
                numericInput(
                  "entry_age",
                  "Age in Years (if DOB is unknown)",
                  value = NA,
                  min = 0,
                  max = 100,
                  step = 0.1
                )
              ),
              
              column(
                width = 4,
                selectInput(
                  "entry_age_class",
                  "Age Class",
                  choices = c(
                    "Unknown",
                    "A", "B", "C", "D", "E", "F"
                  ),
                  selected = "Unknown"
                )
              ),
              
              column(
                width = 4,
                textInput(
                  "entry_age_category",
                  "Age Category",
                  placeholder = "Enter age category"
                )
              )
            ),
            
            fluidRow(
              column(
                width = 4,
                textInput(
                  "entry_notch",
                  "Notch Number",
                  placeholder = "Enter notch number"
                )
              ),
              
              column(
                width = 4,
                selectInput(
                  "entry_notch_status",
                  "Notch Status",
                  choices = c(
                    "Notched",
                    "Not Notched",
                    "Unknown"
                  ),
                  selected = "Unknown"
                )
              ),
              
              column(
                width = 4,
                textInput(
                  "entry_area",
                  "Area Last Sighted",
                  placeholder = "Enter area or location"
                )
              )
            ),
            
            fluidRow(
              column(
                width = 4,
                dateInput(
                  "entry_sighting_date",
                  "Date of Last Sighting",
                  value = NULL,
                  format = "yyyy-mm-dd"
                )
              ),
              
              column(
                width = 4,
                numericInput(
                  "entry_sightings",
                  "Number of Sightings in Last 6 Months",
                  value = 0,
                  min = 0,
                  step = 1
                )
              )
            ),
            
            tags$hr(),
            
            actionButton(
              "add_animal",
              "Add Animal to Register",
              icon = icon("plus"),
              class = "btn-success"
            ),
            
            actionButton(
              "clear_register_form",
              "Clear Form",
              icon = icon("eraser"),
              class = "btn-default"
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Animal Register Records",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            
            DTOutput("animal_table"),
            
            br(),
            
            downloadButton(
              "download_data",
              "Download Animal Register (CSV)",
              class = "btn-success"
            )
          )
        )
      )
    )
  )
)



