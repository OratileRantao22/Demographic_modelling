# =========================================================
# WILDLIFE POPULATION DEMOGRAPHIC DASHBOARD
# SERVER
# =========================================================


# =========================================================
# HELPER FUNCTION: FIND COLUMN
# =========================================================

find_column <- function(df, possible_names) {
  
  clean_names <- function(x) {
    tolower(gsub("[^a-z0-9]", "", x))
  }
  
  actual_names <- names(df)
  
  cleaned_actual <- clean_names(actual_names)
  
  cleaned_possible <- clean_names(possible_names)
  
  match_index <- match(
    cleaned_possible,
    cleaned_actual
  )
  
  match_index <- match_index[!is.na(match_index)]
  
  if (length(match_index) == 0) {
    return(NA_character_)
  }
  
  actual_names[match_index[1]]
}


# =========================================================
# HELPER FUNCTION: CONVERT DATES
# =========================================================

safe_date <- function(x) {
  
  if (inherits(x, "Date")) {
    return(x)
  }
  
  if (inherits(x, "POSIXct") ||
      inherits(x, "POSIXlt")) {
    return(as.Date(x))
  }
  
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }
  
  parsed_dates <- suppressWarnings(
    lubridate::parse_date_time(
      as.character(x),
      orders = c(
        "ymd",
        "dmy",
        "mdy",
        "ymd HMS",
        "dmy HMS",
        "mdy HMS"
      )
    )
  )
  
  as.Date(parsed_dates)
}


# =========================================================
# HELPER FUNCTION: CLEAN AGE VALUES
# =========================================================

safe_numeric <- function(x) {
  
  suppressWarnings(
    as.numeric(as.character(x))
  )
}


# =========================================================
# PROCESS EXCEL WORKBOOK
# =========================================================

process_workbook <- function(file) {
  
  # Check file path
  if (!is.character(file) ||
      length(file) != 1 ||
      is.na(file) ||
      !file.exists(file)) {
    
    stop("Please provide a valid Excel file path.")
    
  }
  
  # Get worksheet names
  sheets <- readxl::excel_sheets(file)
  
  if (length(sheets) != 5) {
    
    warning(
      paste(
        "Expected 5 worksheets but found",
        length(sheets)
      )
    )
    
  }
  
  # Read all worksheets
  all_data <- lapply(sheets, function(sheet) {
    
    df <- readxl::read_excel(
      file,
      sheet = sheet
    )
    
    df <- as.data.frame(df)
    
    # Remove completely empty rows
    if (nrow(df) > 0) {
      
      df <- df[
        rowSums(!is.na(df)) > 0,
        ,
        drop = FALSE
      ]
      
    }
    
    # Identify columns
    id_col <- find_column(
      df,
      c("ID/Name", "ID", "Name", "Animal ID")
    )
    
    sex_col <- find_column(
      df,
      c("Sex", "Gender")
    )
    
    notch_col <- find_column(
      df,
      c("Notch", "Notch Status")
    )
    
    dob_col <- find_column(
      df,
      c(
        "Date_of_Birth",
        "Date of Birth",
        "DOB",
        "Birth Date"
      )
    )
    
    age_col <- find_column(
      df,
      c(
        "Age(years)",
        "Age (years)",
        "Age",
        "Current Age",
        "Age_Years"
      )
    )
    
    age_class_col <- find_column(
      df,
      c("Age_Class", "Age Class")
    )
    
    age_category_col <- find_column(
      df,
      c("Age_Category", "Age Category")
    )
    
    # Required columns
    # Age and Date of Birth are optional.
    required <- c(
      ID_Name = id_col,
      Sex = sex_col,
      Notch = notch_col,
      Age_Class = age_class_col,
      Age_Category = age_category_col
    )
    
    missing <- names(required)[is.na(required)]
    
    if (length(missing) > 0) {
      
      stop(
        paste(
          "Worksheet",
          sheet,
          "is missing:",
          paste(missing, collapse = ", ")
        )
      )
      
    }
    
    # Standardise column names
    data.frame(
      
      Animal_ID = as.character(df[[id_col]]),
      
      Sex = as.character(df[[sex_col]]),
      
      Notch = as.character(df[[notch_col]]),
      
      DOB = if (!is.na(dob_col)) {
        safe_date(df[[dob_col]])
      } else {
        as.Date(rep(NA, nrow(df)))
      },
      
      Age = if (!is.na(age_col)) {
        safe_numeric(df[[age_col]])
      } else {
        rep(NA_real_, nrow(df))
      },
      
      Age_Class = as.character(df[[age_class_col]]),
      
      Age_Category = as.character(df[[age_category_col]]),
      
      Source_Sheet = rep(sheet, nrow(df)),
      
      stringsAsFactors = FALSE
      
    )
    
  })  # Closes lapply() and its function
  
  
  # =======================================================
  # COMBINE ALL WORKSHEETS
  # =======================================================
  
  combined <- dplyr::bind_rows(all_data)
  
  
  # =======================================================
  # CLEAN AND STANDARDISE DATA
  # =======================================================
  
  combined <- combined %>%
    
    dplyr::mutate(
      
      dplyr::across(
        c(
          Animal_ID,
          Sex,
          Notch,
          Age_Class,
          Age_Category,
          Source_Sheet
        ),
        ~ trimws(as.character(.x))
      ),
      
      # Standardise sex
      Sex = dplyr::case_when(
        
        tolower(Sex) %in% c("m", "male") ~ "Male",
        
        tolower(Sex) %in% c("f", "female") ~ "Female",
        
        is.na(Sex) | Sex == "" ~ "Unknown",
        
        TRUE ~ Sex
        
      ),
      
      # Standardise age class
      Age_Class = dplyr::if_else(
        is.na(Age_Class) | Age_Class == "",
        "Unknown",
        Age_Class
      ),
      
      # Standardise age category
      Age_Category = dplyr::if_else(
        is.na(Age_Category) | Age_Category == "",
        "Unknown",
        Age_Category
      ),
      
      # Determine notch status
      Notch_Status = dplyr::case_when(
        
        is.na(Notch) | Notch == "" ~ "Not Recorded",
        
        tolower(Notch) %in%
          c("no", "none", "not notched", "n/a") ~ "No Notch",
        
        TRUE ~ "Notched"
        
      )
      
    )
  
  
  # =======================================================
  # CALCULATE AGE WHERE AGE IS MISSING
  # =======================================================
  
  calculated_age <- floor(
    as.numeric(
      difftime(
        Sys.Date(),
        combined$DOB,
        units = "days"
      )
    ) / 365.2425
  )
  
  combined$Age <- ifelse(
    is.na(combined$Age) & !is.na(combined$DOB),
    calculated_age,
    combined$Age
  )
  
  # Remove records without an animal ID
  combined <- combined %>%
    dplyr::filter(
      !is.na(Animal_ID),
      Animal_ID != ""
    )
  
  # IMPORTANT: return the completed data frame
  return(combined)
  
}


# =========================================================
# MAIN SERVER FUNCTION
# =========================================================

server <- function(input, output, session) {
  
  
  # =======================================================
  # READ UPLOADED EXCEL WORKBOOK
  # =======================================================
  
  population_data <- shiny::reactive({
    
    shiny::req(input$file)
    
    file_path <- input$file$datapath
    
    shiny::req(
      is.character(file_path),
      length(file_path) == 1,
      !is.na(file_path),
      nzchar(file_path),
      file.exists(file_path)
    )
    
    tryCatch(
      
      process_workbook(file_path),
      
      error = function(e) {
        
        shiny::showNotification(
          paste(
            "Workbook error:",
            conditionMessage(e)
          ),
          type = "error",
          duration = NULL
        )
        
        NULL
        
      }
      
    )
    
  })
  
  
  # =======================================================
  # FILTER POPULATION DATA
  # =======================================================
  
  filtered_data <- shiny::reactive({
    
    data <- population_data()
    
    shiny::req(!is.null(data))
    
    if (!is.null(input$sheet_filter) &&
        input$sheet_filter != "All Worksheets") {
      
      data <- data %>%
        dplyr::filter(
          Source_Sheet == input$sheet_filter
        )
      
    }
    
    if (!is.null(input$sex_filter) &&
        input$sex_filter != "All") {
      
      data <- data %>%
        dplyr::filter(
          Sex == input$sex_filter
        )
      
    }
    
    if (!is.null(input$age_class_filter) &&
        input$age_class_filter != "All") {
      
      data <- data %>%
        dplyr::filter(
          Age_Class == input$age_class_filter
        )
      
    }
    
    if (!is.null(input$age_category_filter) &&
        input$age_category_filter != "All") {
      
      data <- data %>%
        dplyr::filter(
          Age_Category == input$age_category_filter
        )
      
    }
    
    data
    
  })
  
  
  # =======================================================
  # UPDATE FILTER OPTIONS AFTER UPLOAD
  # =======================================================
  
  observe({
    
    data <- population_data()
    
    shiny::req(!is.null(data))
    
    updateSelectInput(
      session,
      "sheet_filter",
      choices = c(
        "All Worksheets",
        sort(unique(data$Source_Sheet))
      ),
      selected = "All Worksheets"
    )
    
    updateSelectInput(
      session,
      "sex_filter",
      choices = c(
        "All",
        "Male",
        "Female",
        "Unknown"
      ),
      selected = "All"
    )
    
    updateSelectInput(
      session,
      "age_class_filter",
      choices = c(
        "All",
        sort(unique(data$Age_Class))
      ),
      selected = "All"
    )
    
    updateSelectInput(
      session,
      "age_category_filter",
      choices = c(
        "All",
        sort(unique(data$Age_Category))
      ),
      selected = "All"
    )
    
  })
  
  
  # =======================================================
  # PAGE 1: POPULATION OVERVIEW
  # =======================================================
  
  output$total_population <- renderValueBox({
    
    data <- filtered_data()
    
    valueBox(
      nrow(data),
      "Total Population",
      icon = icon("paw"),
      color = "green"
    )
    
  })
  
  
  output$male_population <- renderValueBox({
    
    data <- filtered_data()
    
    valueBox(
      sum(data$Sex == "Male", na.rm = TRUE),
      "Male Population",
      icon = icon("mars"),
      color = "blue"
    )
    
  })
  
  
  output$female_population <- renderValueBox({
    
    data <- filtered_data()
    
    valueBox(
      sum(data$Sex == "Female", na.rm = TRUE),
      "Female Population",
      icon = icon("venus"),
      color = "purple"
    )
    
  })
  
  
  output$unknown_population <- renderValueBox({
    
    data <- filtered_data()
    
    valueBox(
      sum(data$Sex == "Unknown", na.rm = TRUE),
      "Unknown Sex",
      icon = icon("question-circle"),
      color = "yellow"
    )
    
  })
  
  
  # =======================================================
  # POPULATION BY SEX
  # =======================================================
  
  output$sex_plot <- renderPlot({
    
    data <- filtered_data()
    
    plot_data <- data %>%
      dplyr::count(Sex)
    
    ggplot(
      plot_data,
      aes(x = Sex, y = n, fill = Sex)
    ) +
      
      geom_col() +
      
      geom_text(
        aes(label = n),
        vjust = -0.5
      ) +
      
      labs(
        x = "Sex",
        y = "Number of Animals"
      ) +
      
      theme_minimal()
    
  })
  
  
  # =======================================================
  # POPULATION BY AGE CLASS
  # =======================================================
  
  output$age_class_plot <- renderPlot({
    
    data <- filtered_data()
    
    ggplot(
      data,
      aes(x = Age_Class, fill = Age_Class)
    ) +
      
      geom_bar() +
      
      labs(
        x = "Age Class",
        y = "Number of Animals"
      ) +
      
      theme_minimal()
    
  })
  
  
  # =======================================================
  # POPULATION BY AGE CATEGORY
  # =======================================================
  
  output$age_category_plot <- renderPlot({
    
    data <- filtered_data()
    
    ggplot(
      data,
      aes(x = Age_Category, fill = Age_Category)
    ) +
      
      geom_bar() +
      
      labs(
        x = "Age Category",
        y = "Number of Animals"
      ) +
      
      theme_minimal()
    
  })
  
  
  # =======================================================
  # NOTCH STATUS
  # =======================================================
  
  output$notch_plot <- renderPlot({
    
    data <- filtered_data()
    
    ggplot(
      data,
      aes(x = Notch_Status, fill = Notch_Status)
    ) +
      
      geom_bar() +
      
      labs(
        x = "Notch Status",
        y = "Number of Animals"
      ) +
      
      theme_minimal()
    
  })
  
  
  # =======================================================
  # PAGE 2: POPULATION DEMOGRAPHICS
  # =======================================================
  
  output$demographic_sex_plot <- renderPlot({
    
    data <- filtered_data()
    
    ggplot(
      data,
      aes(x = Sex, fill = Sex)
    ) +
      
      geom_bar() +
      
      labs(
        x = "Sex",
        y = "Population"
      ) +
      
      theme_minimal()
    
  })
  
  
  output$sex_age_plot <- renderPlot({
    
    data <- filtered_data()
    
    ggplot(
      data,
      aes(
        x = Age_Class,
        fill = Sex
      )
    ) +
      
      geom_bar(
        position = "dodge"
      ) +
      
      labs(
        x = "Age Class",
        y = "Population"
      ) +
      
      theme_minimal()
    
  })
  
  
  output$category_sex_plot <- renderPlot({
    
    data <- filtered_data()
    
    ggplot(
      data,
      aes(
        x = Age_Category,
        fill = Sex
      )
    ) +
      
      geom_bar(
        position = "dodge"
      ) +
      
      labs(
        x = "Age Category",
        y = "Population"
      ) +
      
      theme_minimal()
    
  })
  
  
  # =======================================================
  # PAGE 3: AGE ANALYSIS
  # =======================================================
  
  output$average_age <- renderValueBox({
    
    data <- filtered_data()
    
    age <- mean(
      data$Age,
      na.rm = TRUE
    )
    
    if (is.nan(age)) {
      age <- NA_real_
    }
    
    valueBox(
      ifelse(
        is.na(age),
        "N/A",
        round(age, 1)
      ),
      "Average Age (Years)",
      icon = icon("calculator"),
      color = "green"
    )
    
  })
  
  
  output$youngest_age <- renderValueBox({
    
    data <- filtered_data()
    
    age <- suppressWarnings(
      min(data$Age, na.rm = TRUE)
    )
    
    if (is.infinite(age)) {
      age <- NA_real_
    }
    
    valueBox(
      ifelse(
        is.na(age),
        "N/A",
        age
      ),
      "Youngest Age (Years)",
      icon = icon("baby"),
      color = "blue"
    )
    
  })
  
  
  output$oldest_age <- renderValueBox({
    
    data <- filtered_data()
    
    age <- suppressWarnings(
      max(data$Age, na.rm = TRUE)
    )
    
    if (is.infinite(age)) {
      age <- NA_real_
    }
    
    valueBox(
      ifelse(
        is.na(age),
        "N/A",
        age
      ),
      "Oldest Age (Years)",
      icon = icon("paw"),
      color = "purple"
    )
    
  })
  
  
  # =======================================================
  # AGE DISTRIBUTION
  # =======================================================
  
  output$age_histogram <- renderPlot({
    
    data <- filtered_data() %>%
      dplyr::filter(
        !is.na(Age)
      )
    
    ggplot(
      data,
      aes(x = Age)
    ) +
      
      geom_histogram(
        binwidth = 1,
        fill = "forestgreen",
        colour = "white"
      ) +
      
      labs(
        x = "Age (Years)",
        y = "Number of Animals"
      ) +
      
      theme_minimal()
    
  })
  
  
  # =======================================================
  # BIRTHS BY YEAR
  # =======================================================
  
  output$birth_year_plot <- renderPlot({
    
    data <- filtered_data() %>%
      dplyr::filter(
        !is.na(DOB)
      ) %>%
      dplyr::mutate(
        Birth_Year = lubridate::year(DOB)
      ) %>%
      dplyr::count(Birth_Year)
    
    ggplot(
      data,
      aes(
        x = Birth_Year,
        y = n
      )
    ) +
      
      geom_col(
        fill = "forestgreen"
      ) +
      
      labs(
        x = "Birth Year",
        y = "Number of Animals"
      ) +
      
      theme_minimal()
    
  })
  
  
  # =======================================================
  # PAGE 4: ANIMAL REGISTER
  # =======================================================
  
  output$animal_table <- DT::renderDT({
    
    data <- filtered_data()
    
    DT::datatable(
      
      data,
      
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      
      rownames = FALSE
      
    )
    
  })
  
  
  # =======================================================
  # DOWNLOAD FILTERED DATA
  # =======================================================
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "Wildlife_Population_",
        Sys.Date(),
        ".csv"
      )
      
    },
    
    content = function(file) {
      
      write.csv(
        filtered_data(),
        file,
        row.names = FALSE
      )
      
    }
    
  )
  
}
