
# ============================================================
# WILDLIFE POPULATION OVERVIEW
# USER INTERFACE
# ============================================================


ui <- dashboardPage(
  
  skin = "green",
  
  # HEADER
  dashboardHeader(
    title = "Population Overview",
    titleWidth = 280
  ),
  
  # SIDEBAR
  dashboardSidebar(
    
    width = 280,
    
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
        icon = icon("chart-bar")
      ),
      
      menuItem(
        "Animal Register",
        tabName = "register",
        icon = icon("table")
      )
      
    ),
    
    br(),
    
    # EXCEL FILE UPLOAD
    
    fileInput(
      "file",
      "Upload Excel Population File",
      accept = c(".xlsx", ".xls")
    ),
    
    hr(),
    
    h4("Population Filters"),
    
    selectInput(
      "sheet_filter",
      "Source Worksheet",
      choices = "All"
    ),
    
    selectInput(
      "sex_filter",
      "Sex",
      choices = "All"
    ),
    
    selectInput(
      "age_class_filter",
      "Age Class",
      choices = "All"
    ),
    
    selectInput(
      "age_category_filter",
      "Age Category",
      choices = "All"
    )
    
  ),
  
  # MAIN DASHBOARD BODY
  
  dashboardBody(
    
    # CUSTOM GREEN THEME
    
    tags$head(
      
      tags$style(HTML("

        .content-wrapper, .right-side {
          background-color: #f4f7f2;
        }

        .skin-green .main-header .logo {
          background-color: #285b3b;
          color: white;
          font-weight: bold;
        }

        .skin-green .main-header .navbar {
          background-color: #36784b;
        }

        .skin-green .main-sidebar {
          background-color: #203d2b;
        }

        .box {
          border-top: 3px solid #438653;
        }

        .small-box {
          border-radius: 6px;
        }

        .content {
          padding: 20px;
        }

      "))
      
    ),
    
    # ========================================================
    # DASHBOARD PAGES
    # ========================================================
    
    tabItems(
      
      # ------------------------------------------------------
      # PAGE 1: POPULATION OVERVIEW
      # ------------------------------------------------------
      
      tabItem(
        
        tabName = "overview",
        
        fluidRow(
          
          valueBoxOutput(
            "total_population",
            width = 3
          ),
          
          valueBoxOutput(
            "male_population",
            width = 3
          ),
          
          valueBoxOutput(
            "female_population",
            width = 3
          ),
          
          valueBoxOutput(
            "unknown_population",
            width = 3
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Sex Distribution",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            
            plotOutput(
              "sex_plot",
              height = 300
            )
          ),
          
          box(
            title = "Age Class Distribution",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            
            plotOutput(
              "age_class_plot",
              height = 300
            )
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Age Category Distribution",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            
            plotOutput(
              "age_category_plot",
              height = 300
            )
          ),
          
          box(
            title = "Notch Identification",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            
            plotOutput(
              "notch_plot",
              height = 300
            )
          )
          
        )
        
      ),
      
      # ------------------------------------------------------
      # PAGE 2: POPULATION DEMOGRAPHICS
      # ------------------------------------------------------
      
      tabItem(
        
        tabName = "demographics",
        
        fluidRow(
          
          box(
            title = "Population by Sex",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            
            plotOutput(
              "sex_detail_plot",
              height = 350
            )
          ),
          
          box(
            title = "Sex Distribution Across Age Classes",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            
            plotOutput(
              "sex_age_plot",
              height = 350
            )
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Population by Age Category",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            
            plotOutput(
              "category_detail_plot",
              height = 350
            )
          )
          
        )
        
      ),
      
      # ------------------------------------------------------
      # PAGE 3: AGE ANALYSIS
      # ------------------------------------------------------
      
      tabItem(
        
        tabName = "age",
        
        fluidRow(
          
          valueBoxOutput(
            "average_age",
            width = 4
          ),
          
          valueBoxOutput(
            "youngest_age",
            width = 4
          ),
          
          valueBoxOutput(
            "oldest_age",
            width = 4
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Age Distribution",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            
            plotOutput(
              "age_distribution",
              height = 350
            )
          ),
          
          box(
            title = "Births by Year",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            
            plotOutput(
              "birth_year_plot",
              height = 350
            )
          )
          
        )
        
      ),
      
      # ------------------------------------------------------
      # PAGE 4: ANIMAL REGISTER
      # ------------------------------------------------------
      
      tabItem(
        
        tabName = "register",
        
        fluidRow(
          
          box(
            title = "Wildlife Population Register",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            
            downloadButton(
              "download_data",
              "Download Population Data"
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
