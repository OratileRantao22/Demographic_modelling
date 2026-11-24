# ============================================================
# WILDLIFE POPULATION DEMOGRAPHIC DASHBOARD
# UI.R - USER INTERFACE
# ============================================================

library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  
  skin = "green",
  
  # ----------------------------------------------------------
  # HEADER
  # ----------------------------------------------------------
  
  dashboardHeader(
    title = "Wildlife Population Dashboard",
    titleWidth = 300
  ),
  
  # ----------------------------------------------------------
  # SIDEBAR
  # ----------------------------------------------------------
  
  dashboardSidebar(
    
    width = 300,
    
    sidebarMenu(
      
      menuItem(
        "Population Overview",
        tabName = "overview",
        icon = icon("chart-pie")
      ),
      
      menuItem(
        "Demographics",
        tabName = "demographics",
        icon = icon("users")
      ),
      
      menuItem(
        "Age Analysis",
        tabName = "age",
        icon = icon("chart-line")
      ),
      
      menuItem(
        "Animal Register",
        tabName = "register",
        icon = icon("paw")
      )
      
    )
  ),
  
  # ----------------------------------------------------------
  # DASHBOARD BODY
  # ----------------------------------------------------------
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f8f2;
        }

        .skin-green .main-header .logo {
          background-color: #1b5e20;
          font-weight: bold;
        }

        .skin-green .main-header .navbar {
          background-color: #2e7d32;
        }

        .skin-green .main-sidebar {
          background-color: #1b5e20;
        }

        .box {
          border-top: 3px solid #4caf50;
        }

        .small-box {
          border-radius: 8px;
        }

        h3 {
          color: #1b5e20;
        }
      "))
    ),
    
    # --------------------------------------------------------
    # WORKBOOK UPLOAD - VISIBLE ON ALL TABS
    # --------------------------------------------------------
    
    fluidRow(
      
      box(
        title = "Upload Wildlife Population Workbook",
        width = 12,
        status = "success",
        solidHeader = TRUE,
        
        fileInput(
          "excel_file",
          "Select Excel Workbook",
          accept = c(".xlsx", ".xls")
        ),
        
        textOutput("workbook_status")
        
      )
      
    ),
    
    # --------------------------------------------------------
    # TAB CONTENT
    # --------------------------------------------------------
    
    tabItems(
      
      # ======================================================
      # TAB 1: POPULATION OVERVIEW
      # ======================================================
      
      tabItem(
        
        tabName = "overview",
        
        h3("Wildlife Population Overview"),
        
        fluidRow(
          
          valueBoxOutput("total_animals", width = 3),
          
          valueBoxOutput("male_animals", width = 3),
          
          valueBoxOutput("female_animals", width = 3),
          
          valueBoxOutput("unknown_animals", width = 3)
          
        ),
        
        fluidRow(
          
          box(
            title = "Population Filters",
            width = 12,
            status = "success",
            
            selectInput(
              "filter_sex",
              "Filter by Sex:",
              choices = c("All", "Male", "Female", "Unknown"),
              selected = "All"
            ),
            
            selectInput(
              "filter_age_class",
              "Filter by Age Class:",
              choices = "All",
              selected = "All"
            )
            
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Population by Sex",
            width = 6,
            status = "success",
            plotOutput("sex_plot", height = 350)
          ),
          
          box(
            title = "Population by Age Class",
            width = 6,
            status = "success",
            plotOutput("age_class_plot", height = 350)
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Population by Age Category",
            width = 6,
            status = "success",
            plotOutput("age_category_plot", height = 350)
          ),
          
          box(
            title = "Population by Notch Number",
            width = 6,
            status = "success",
            plotOutput("notch_plot", height = 350)
          )
          
        )
        
      ),
      
      # ======================================================
      # TAB 2: DEMOGRAPHICS
      # ======================================================
      
      tabItem(
        
        tabName = "demographics",
        
        h3("Wildlife Demographic Analysis"),
        
        fluidRow(
          
          box(
            title = "Sex Distribution",
            width = 6,
            status = "success",
            plotOutput("demographic_sex_plot", height = 350)
          ),
          
          box(
            title = "Sex by Age Class",
            width = 6,
            status = "success",
            plotOutput("sex_age_class_plot", height = 350)
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Age Category by Sex",
            width = 12,
            status = "success",
            plotOutput("category_sex_plot", height = 400)
          )
          
        )
        
      ),
      
      # ======================================================
      # TAB 3: AGE ANALYSIS
      # ======================================================
      
      tabItem(
        
        tabName = "age",
        
        h3("Wildlife Age Analysis"),
        
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
            plotOutput("age_histogram", height = 350)
          ),
          
          box(
            title = "Births by Year",
            width = 6,
            status = "success",
            plotOutput("birth_year_plot", height = 350)
          )
          
        )
        
      ),
      
      # ======================================================
      # TAB 4: ANIMAL REGISTER
      # ======================================================
      
      tabItem(
        
        tabName = "register",
        
        h3("Wildlife Animal Register"),
        
        fluidRow(
          
          box(
            title = "Add Animal Record Manually",
            width = 12,
            status = "success",
            
            textInput(
              "animal_id",
              "Animal ID / Name"
            ),
            
            selectInput(
              "animal_sex",
              "Sex",
              choices = c("Female", "Male", "Unknown")
            ),
            
            textInput(
              "animal_notch",
              "Notch Number"
            ),
            
            dateInput(
              "animal_dob",
              "Date of Birth",
              value = NA
            ),
            
            numericInput(
              "animal_age",
              "Age (Years)",
              value = NA,
              min = 0
            ),
            
            textInput(
              "animal_age_class",
              "Age Class"
            ),
            
            textInput(
              "animal_age_category",
              "Age Category"
            ),
            
            actionButton(
              "add_animal",
              "Add Animal",
              icon = icon("plus"),
              class = "btn-success"
            )
            
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Animal Register",
            width = 12,
            status = "success",
            
            DTOutput("register_table"),
            
            br(),
            
            downloadButton(
              "download_register",
              "Download Animal Register"
            )
            
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Uploaded Workbook Worksheet",
            width = 12,
            status = "success",
            
            selectInput(
              "workbook_sheet",
              "Select Worksheet:",
              choices = NULL
            ),
            
            DTOutput("workbook_preview")
            
          )
          
        )
        
      )
      
    )
    
  )
  
)
