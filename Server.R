
# ============================================================
# WILDLIFE POPULATION OVERVIEW
# SERVER FILE
# ============================================================

# ============================================================
# 1. HELPER FUNCTIONS
# ============================================================

# Standardise column names

clean_names <- function(x) {
  
  x <- tolower(trimws(x))
  
  x <- gsub("[^a-z0-9]", "", x)
  
  return(x)
  
}


# Identify matching columns

find_column <- function(data, possible_names) {
  
  existing <- clean_names(names(data))
  
  possible <- clean_names(possible_names)
  
  match_index <- match(possible, existing)
  
  match_index <- match_index[!is.na(match_index)]
  
  if (length(match_index) == 0) {
    return(NULL)
  }
  
  names(data)[match_index[1]]
  
}


# Convert dates safely

safe_date <- function(x) {
  
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }
  
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  
  if (is.numeric(x)) {
    
    return(
      as.Date(x, origin = "1899-12-30")
    )
    
  }
  
  parsed <- suppressWarnings(
    
    lubridate::parse_date_time(
      as.character(x),
      
      orders = c(
        "ymd",
        "dmy",
        "mdy",
        "Ymd",
        "dmY",
        "mdY"
      )
      
    )
    
  )
  
  as.Date(parsed)
  
}


# ============================================================
# 2. PROCESS EXCEL WORKBOOK
# ============================================================

process_workbook <- function(file) {
  
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
  
  
  # Read each worksheet
  
  all_data <- lapply(sheets, function(sheet) {
    
    df <- readxl::read_excel(
      file,
      sheet = sheet
    )
    
    
    # Identify columns
    
    id_col <- find_column(
      df,
      c(
        "ID/Name",
        "ID",
        "Name",
        "Animal ID",
        "Animal Name",
        "Individual ID",
        "Individual Name"
      )
    )
    
    
    sex_col <- find_column(
      df,
      c("Sex", "Gender")
    )
    
    
    notch_col <- find_column(
      df,
      c(
        "Notch",
        "Notch Number",
        "Notch ID",
        "Ear Notch"
      )
    )
    
    
    dob_col <- find_column(
      df,
      c(
        "Date of Birth",
        "DOB",
        "Birth Date",
        "Date Born"
      )
    )
    
    
    age_col <- find_column(
      df,
      c(
        "Age",
        "Age Years",
        "Age in Years"
      )
    )
    
    
    age_class_col <- find_column(
      df,
      c(
        "Age Class",
        "AgeClass"
      )
    )
    
    
    age_category_col <- find_column(
      df,
      c(
        "Age Category",
        "AgeCategory"
      )
    )
    
    
    # Extract columns
    
    get_column <- function(column) {
      
      if (is.null(column)) {
        return(rep(NA, nrow(df)))
      }
      
      df[[column]]
      
    }
    
    
    # Create standardised dataset
    
    data.frame(
      
      Animal_ID = as.character(
        get_column(id_col)
      ),
      
      Sex = as.character(
        get_column(sex_col)
      ),
      
      Notch = as.character(
        get_column(notch_col)
      ),
      
      DOB = safe_date(
        get_column(dob_col)
      ),
      
      Age = suppressWarnings(
        as.numeric(get_column(age_col))
      ),
      
      Age_Class = as.character(
        get_column(age_class_col)
      ),
      
      Age_Category = as.character(
        get_column(age_category_col)
      ),
      
      Source_Sheet = sheet,
      
      stringsAsFactors = FALSE
      
    )
    
  })
  
  
  # Combine all five worksheets
  
  combined <- dplyr::bind_rows(all_data)
  
  
  # Clean data
  
  combined <- combined %>%
    
    mutate(
      
      across(
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
      
      Sex = case_when(
        
        tolower(Sex) %in% c(
          "m",
          "male"
        ) ~ "Male",
        
        tolower(Sex) %in% c(
          "f",
          "female"
        ) ~ "Female",
        
        is.na(Sex) | Sex == "" ~ "Unknown",
        
        TRUE ~ Sex
        
      ),
      
      
      # Missing age class
      
      Age_Class = ifelse(
        is.na(Age_Class) | Age_Class == "",
        "Unknown",
        Age_Class
      ),
      
      
      # Missing age category
      
      Age_Category = ifelse(
        is.na(Age_Category) | Age_Category == "",
        "Unknown",
        Age_Category
      ),
      
      
      # Notch status
      
      Notch_Status = case_when(
        
        is.na(Notch) | Notch == "" ~ "Not Recorded",
        
        tolower(Notch) %in% c(
          "no",
          "none",
          "not notched",
          "n/a"
        ) ~ "No Notch",
        
        TRUE ~ "Notched"
        
      )
      
    )
  
  
  # Calculate age where missing
  
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
    
    is.na(combined$Age) &
      !is.na(combined$DOB),
    
    calculated_age,
    
    combined$Age
    
  )
  
  
  # Remove completely empty records
  
  combined <- combined %>%
    
    filter(
      
      !(
        is.na(Animal_ID) &
          is.na(Sex) &
          is.na(DOB) &
          is.na(Age)
      )
      
    )
  
  
  return(combined)
  
}


# ============================================================
# 3. SERVER FUNCTION
# ============================================================

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # READ EXCEL FILE
  # ----------------------------------------------------------
  
  population_data <- reactive({
    
    req(input$file)
    
    process_workbook(
      input$file$datapath
    )
    
  })
  
  
  # ----------------------------------------------------------
  # UPDATE FILTERS
  # ----------------------------------------------------------
  
  observeEvent(population_data(), {
    
    data <- population_data()
    
    
    updateSelectInput(
      session,
      "sheet_filter",
      
      choices = c(
        "All",
        sort(unique(data$Source_Sheet))
      ),
      
      selected = "All"
    )
    
    
    updateSelectInput(
      session,
      "sex_filter",
      
      choices = c(
        "All",
        sort(unique(data$Sex))
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
  
  
  # ----------------------------------------------------------
  # FILTER POPULATION DATA
  # ----------------------------------------------------------
  
  filtered_data <- reactive({
    
    data <- population_data()
    
    
    if (!is.null(input$sheet_filter) &&
        input$sheet_filter != "All") {
      
      data <- data %>%
        
        filter(
          Source_Sheet == input$sheet_filter
        )
      
    }
    
    
    if (!is.null(input$sex_filter) &&
        input$sex_filter != "All") {
      
      data <- data %>%
        
        filter(
          Sex == input$sex_filter
        )
      
    }
    
    
    if (!is.null(input$age_class_filter) &&
        input$age_class_filter != "All") {
      
      data <- data %>%
        
        filter(
          Age_Class == input$age_class_filter
        )
      
    }
    
    
    if (!is.null(input$age_category_filter) &&
        input$age_category_filter != "All") {
      
      data <- data %>%
        
        filter(
          Age_Category == input$age_category_filter
        )
      
    }
    
    
    return(data)
    
  })
  
  
  # ==========================================================
  # 4. POPULATION SUMMARY CARDS
  # ==========================================================
  
  output$total_population <- renderValueBox({
    
    data <- filtered_data()
    
    valueBox(
      
      value = nrow(data),
      
      subtitle = "Total Population",
      
      icon = icon("paw"),
      
      color = "green"
      
    )
    
  })
  
  
  output$male_population <- renderValueBox({
    
    data <- filtered_data()
    
    valueBox(
      
      value = sum(
        data$Sex == "Male",
        na.rm = TRUE
      ),
      
      subtitle = "Male Population",
      
      icon = icon("mars"),
      
      color = "olive"
      
    )
    
  })
  
  
  output$female_population <- renderValueBox({
    
    data <- filtered_data()
    
    valueBox(
      
      value = sum(
        data$Sex == "Female",
        na.rm = TRUE
      ),
      
      subtitle = "Female Population",
      
      icon = icon("venus"),
      
      color = "teal"
      
    )
    
  })
  
  
  output$unknown_population <- renderValueBox({
    
    data <- filtered_data()
    
    valueBox(
      
      value = sum(
        data$Sex == "Unknown",
        na.rm = TRUE
      ),
      
      subtitle = "Unknown Sex",
      
      icon = icon("question-circle"),
      
      color = "yellow"
      
    )
    
  })
  
  
  # ==========================================================
  # 5. POPULATION OVERVIEW CHARTS
  # ==========================================================
  
  
  # SEX DISTRIBUTION
  
  output$sex_plot <- renderPlot({
    
    data <- filtered_data()
    
    plot_data <- data %>%
      count(Sex)
    
    ggplot(
      plot_data,
      aes(
        x = Sex,
        y = n,
        fill = Sex
      )
    ) +
      
      geom_col(width = 0.65) +
      
      geom_text(
        aes(label = n),
        vjust = -0.5,
        size = 5
      ) +
      
      scale_fill_manual(
        values = c(
          "Male" = "#397D54",
          "Female" = "#91B96E",
          "Unknown" = "#B9B9B9"
        ),
        na.value = "#B9B9B9"
      ) +
      
      labs(
        x = "Sex",
        y = "Number of Animals"
      ) +
      
      theme_minimal() +
      
      theme(
        legend.position = "none"
      )
    
  })
  
  
  # AGE CLASS DISTRIBUTION
  
  output$age_class_plot <- renderPlot({
    
    data <- filtered_data()
    
    plot_data <- data %>%
      count(Age_Class) %>%
      arrange(n)
    
    ggplot(
      plot_data,
      aes(
        x = reorder(Age_Class, n),
        y = n
      )
    ) +
      
      geom_col(
        fill = "#438653",
        width = 0.7
      ) +
      
      coord_flip() +
      
      geom_text(
        aes(label = n),
        hjust = -0.3
      ) +
      
      labs(
        x = "Age Class",
        y = "Number of Animals"
      ) +
      
      theme_minimal()
    
  })
  
  
  # AGE CATEGORY DISTRIBUTION
  
  output$age_category_plot <- renderPlot({
    
    data <- filtered_data()
    
    plot_data <- data %>%
      count(Age_Category)
    
    ggplot(
      plot_data,
      aes(
        x = reorder(Age_Category, n),
        y = n
      )
    ) +
      
      geom_col(
        fill = "#78A65A"
      ) +
      
      coord_flip() +
      
      labs(
        x = "Age Category",
        y = "Number of Animals"
      ) +
      
      theme_minimal()
    
  })
  
  
  # NOTCH IDENTIFICATION
  
  output$notch_plot <- renderPlot({
    
    data <- filtered_data()
    
    plot_data <- data %>%
      count(Notch_Status)
    
    ggplot(
      plot_data,
      aes(
        x = Notch_Status,
        y = n,
        fill = Notch_Status
      )
    ) +
      
      geom_col() +
      
      geom_text(
        aes(label = n),
        vjust = -0.4
      ) +
      
      scale_fill_manual(
        values = c(
          "Notched" = "#397D54",
          "No Notch" = "#91B96E",
          "Not Recorded" = "#B9B9B9"
        )
      ) +
      
      labs(
        x = "Notch Status",
        y = "Number of Animals"
      ) +
      
      theme_minimal() +
      
      theme(
        legend.position = "none"
      )
    
  })
  
  
  # ==========================================================
  # 6. POPULATION DEMOGRAPHICS
  # ==========================================================
  
  
  # SEX DISTRIBUTION PIE CHART
  
  output$sex_detail_plot <- renderPlot({
    
    data <- filtered_data()
    
    plot_data <- data %>%
      count(Sex)
    
    ggplot(
      plot_data,
      aes(
        x = "",
        y = n,
        fill = Sex
      )
    ) +
      
      geom_col(
        width = 1,
        color = "white"
      ) +
      
      coord_polar("y") +
      
      labs(
        x = NULL,
        y = NULL,
        fill = "Sex"
      ) +
      
      theme_void()
    
  })
  
  
  # SEX BY AGE CLASS
  
  output$sex_age_plot <- renderPlot({
    
    data <- filtered_data()
    
    plot_data <- data %>%
      count(Age_Class, Sex)
    
    ggplot(
      plot_data,
      aes(
        x = Age_Class,
        y = n,
        fill = Sex
      )
    ) +
      
      geom_col(
        position = "dodge"
      ) +
      
      scale_fill_manual(
        values = c(
          "Male" = "#397D54",
          "Female" = "#91B96E",
          "Unknown" = "#B9B9B9"
        ),
        na.value = "#B9B9B9"
      ) +
      
      labs(
        x = "Age Class",
        y = "Number of Animals"
      ) +
      
      theme_minimal()
    
  })
  
  
  # AGE CATEGORY BY SEX
  
  output$category_detail_plot <- renderPlot({
    
    data <- filtered_data()
    
    plot_data <- data %>%
      count(Age_Category, Sex)
    
    ggplot(
      plot_data,
      aes(
        x = Age_Category,
        y = n,
        fill = Sex
      )
    ) +
      
      geom_col(
        position = "dodge"
      ) +
      
      labs(
        x = "Age Category",
        y = "Number of Animals"
      ) +
      
      theme_minimal()
    
  })
  
  
  # ==========================================================
  # 7. AGE ANALYSIS SUMMARY CARDS
  # ==========================================================
  
  
  output$average_age <- renderValueBox({
    
    data <- filtered_data()
    
    age <- data$Age[
      !is.na(data$Age)
    ]
    
    valueBox(
      
      value = if (length(age) == 0) {
        "N/A"
      } else {
        round(mean(age), 1)
      },
      
      subtitle = "Average Age (Years)",
      
      icon = icon("calculator"),
      
      color = "green"
      
    )
    
  })
  
  
  output$youngest_age <- renderValueBox({
    
    data <- filtered_data()
    
    age <- data$Age[
      !is.na(data$Age)
    ]
    
    valueBox(
      
      value = if (length(age) == 0) {
        "N/A"
      } else {
        min(age)
      },
      
      subtitle = "Youngest Age (Years)",
      
      icon = icon("child"),
      
      color = "olive"
      
    )
    
  })
  
  
  output$oldest_age <- renderValueBox({
    
    data <- filtered_data()
    
    age <- data$Age[
      !is.na(data$Age)
    ]
    
    valueBox(
      
      value = if (length(age) == 0) {
        "N/A"
      } else {
        max(age)
      },
      
      subtitle = "Oldest Age (Years)",
      
      icon = icon("paw"),
      
      color = "teal"
      
    )
    
  })
  
  
  # ==========================================================
  # 8. AGE DISTRIBUTION
  # ==========================================================
  
  output$age_distribution <- renderPlot({
    
    data <- filtered_data() %>%
      filter(!is.na(Age))
    
    validate(
      need(
        nrow(data) > 0,
        "No age information available."
      )
    )
    
    ggplot(
      data,
      aes(x = Age)
    ) +
      
      geom_histogram(
        binwidth = 1,
        fill = "#438653",
        color = "white"
      ) +
      
      labs(
        x = "Age (Years)",
        y = "Number of Animals"
      ) +
      
      theme_minimal()
    
  })
  
  
  # ==========================================================
  # 9. BIRTH YEAR ANALYSIS
  # ==========================================================
  
  output$birth_year_plot <- renderPlot({
    
    data <- filtered_data() %>%
      
      filter(!is.na(DOB)) %>%
      
      mutate(
        Birth_Year = lubridate::year(DOB)
      ) %>%
      
      count(Birth_Year)
    
    validate(
      need(
        nrow(data) > 0,
        "No birth date information available."
      )
    )
    
    ggplot(
      data,
      aes(
        x = Birth_Year,
        y = n
      )
    ) +
      
      geom_col(
        fill = "#438653"
      ) +
      
      geom_line(
        color = "#285B3B",
        linewidth = 1
      ) +
      
      labs(
        x = "Birth Year",
        y = "Number of Animals Born"
      ) +
      
      theme_minimal()
    
  })
  
  
  # ==========================================================
  # 10. ANIMAL REGISTER
  # ==========================================================
  
  output$animal_table <- renderDT({
    
    data <- filtered_data() %>%
      
      select(
        Animal_ID,
        Sex,
        Notch,
        DOB,
        Age,
        Age_Class,
        Age_Category,
        Source_Sheet
      )
    
    datatable(
      
      data,
      
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c(
          "copy",
          "csv",
          "excel"
        )
      ),
      
      rownames = FALSE,
      
      filter = "top"
      
    )
    
  })
  
  
  # ==========================================================
  # 11. DOWNLOAD POPULATION DATA
  # ==========================================================
  
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
