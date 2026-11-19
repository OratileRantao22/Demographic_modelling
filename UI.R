
# =========================================================
# WILDLIFE POPULATION DEMOGRAPHIC DASHBOARD
# USER INTERFACE
# =========================================================

ui <- dashboardPage(
  
  skin = "green",
  
  dashboardHeader(
    title = "Wildlife Population Dashboard",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    
    width = 300,
    
    sidebarMenu(
      
      menuItem(
        "Population Overview",
        tabName = "overview",
        icon = icon("chart-pie")
      ),
      
      menuItem(
        "Population Demographics",
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
        icon = icon("table")
      )
      
    ),
    
    br(),
    
    fileInput(
      "file",
      "Upload Excel Workbook",
      accept = c(".xlsx", ".xls")
    ),
    
    helpText(
      "Upload your wildlife population workbook. The dashboard will identify worksheets containing individual animal records."
    ),
    
    hr(),
    
    selectInput(
      "sheet_filter",
      "Select Animal Data Worksheet:",
      choices = "All Worksheets",
      selected = "All Worksheets"
    ),
    
    selectInput(
      "sex_filter",
      "Filter by Sex:",
      choices = c("All", "Male", "Female", "Unclassified", "Unknown"),
      selected = "All"
    ),
    
    selectInput(
      "age_class_filter",
      "Filter by Age Class:",
      choices = "All",
      selected = "All"
    ),
    
    selectInput(
      "age_category_filter",
      "Filter by Age Category:",
      choices = "All",
      selected = "All"
    )
    
  ),
  
  dashboardBody(
    
    tags$head(
      
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f8f3;
        }

        .main-header .logo {
          font-weight: bold;
        }

        .box {
          border-top: 3px solid #4CAF50;
        }

        .small-box {
          border-radius: 8px;
        }

        .skin-green .main-sidebar {
          background-color: #174d32;
        }

        .skin-green .sidebar-menu > li.active > a {
          background-color: #2e8b57;
        }

        .skin-green .main-header .logo {
          background-color: #174d32;
        }

        .skin-green .main-header .navbar {
          background-color: #2e8b57;
        }
      "))
      
    ),
    
    tabItems(
      
      # ===================================================
      # PAGE 1: POPULATION OVERVIEW
      # ===================================================
      
      tabItem(
        
        tabName = "overview",
        
        fluidRow(
          
          valueBoxOutput("total_population", width = 3),
          valueBoxOutput("male_population", width = 3),
          valueBoxOutput("female_population", width = 3),
          valueBoxOutput("unknown_population", width = 3)
          
        ),
        
        fluidRow(
          
          box(
            title = "Population by Sex",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotOutput("sex_plot")
          ),
          
          box(
            title = "Population by Age Class",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotOutput("age_class_plot")
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Population by Age Category",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotOutput("age_category_plot")
          ),
          
          box(
            title = "Notch Status",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotOutput("notch_plot")
          )
          
        )
        
      ),
      
      # ===================================================
      # PAGE 2: POPULATION DEMOGRAPHICS
      # ===================================================
      
      tabItem(
        
        tabName = "demographics",
        
        fluidRow(
          
          box(
            title = "Population by Sex",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotOutput("demographic_sex_plot")
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Sex Distribution Across Age Classes",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotOutput("sex_age_plot")
          ),
          
          box(
            title = "Age Category by Sex",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotOutput("category_sex_plot")
          )
          
        )
        
      ),
      
      # ===================================================
      # PAGE 3: AGE ANALYSIS
      # ===================================================
      
      tabItem(
        
        tabName = "age",
        
        fluidRow(
          
          valueBoxOutput("average_age", width = 4),
          valueBoxOutput("youngest_age", width = 4),
          valueBoxOutput("oldest_age", width = 4)
          
        ),
        
        fluidRow(
          
          box(
            title = "Age Distribution",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotOutput("age_histogram")
          ),
          
          box(
            title = "Births by Year",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotOutput("birth_year_plot")
          )
          
        )
        
      ),
      
      # ===================================================
      # PAGE 4: ANIMAL REGISTER
      # ===================================================
      
      tabItem(
        
        tabName = "register",
        
        fluidRow(
          
          box(
            title = "Wildlife Animal Register",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            
            downloadButton(
              "download_data",
              "Download CSV"
            ),
            
            br(),
            br(),
            
            DTOutput("animal_table")
            
          )
          
        )
        
      )
      
    )
    
  )
  
)
