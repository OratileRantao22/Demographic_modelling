
# =========================================================
# WILDLIFE POPULATION DEMOGRAPHIC DASHBOARD
# SERVER
# =========================================================

# =========================================================
# HELPER FUNCTION: CLEAN COLUMN NAMES FOR MATCHING
# =========================================================

clean_column_names <- function(x) {
  
  tolower(
    gsub("[^a-z0-9]", "", as.character(x))
  )
  
}

# =========================================================
# HELPER FUNCTION: FIND A COLUMN
# =========================================================

find_column <- function(df, possible_names) {
  
  actual_names <- names(df)
  
  cleaned_actual <- clean_column_names(actual_names)
  
  cleaned_possible <- clean_column_names(possible_names)
  
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
    
    return(
      as.Date(x, origin = "1899-12-30")
    )
    
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
# HELPER FUNCTION: CONVERT NUMERIC VALUES
# =========================================================

safe_numeric <- function(x) {
  
  suppressWarnings(
    as.numeric(
      gsub(",", "", as.character(x))
    )
  )
  
}

# =========================================================
# HELPER FUNCTION: STANDARDISE SEX
# =========================================================

standardise_sex <- function(x) {
  
  x <- trimws(as.character(x))
  
  dplyr::case_when(
    
    tolower(x) %in% c("m", "male") ~ "Male",
    
    tolower(x) %in% c("f", "female") ~ "Female",
    
    tolower(x) %in% c(
      "unclassified",
      "unclass",
      "unknown sex"
    ) ~ "Unclassified",
    
    is.na(x) | x == "" ~ "Unknown",
    
    TRUE ~ x
    
  )
  
}

# =========================================================
# HELPER FUNCTION: DERIVE AGE CLASS
# =========================================================

derive_age_class <- function(age) {
  
  dplyr::case_when(
    
    is.na(age) ~ "Unknown",
    
    age < 0.25 ~ "A",
    
    age >= 0.25 & age < 1 ~ "B",
    
    age >= 1 & age < 2 ~ "C",
    
    age >= 2 & age < 3.5 ~ "D",
    
    age >= 3.5 & age <= 6.9 ~ "E",
    
    age > 7 ~ "F",
    
    TRUE ~ "Unknown"
    
  )
  
}

# =========================================================
# FIND ANIMAL REGISTER HEADER ROW
# =========================================================


# =========================================================
# FIND ANIMAL REGISTER HEADER ROW
# =========================================================

find_animal_header <- function(file, sheet, max_rows = 150) {
  
  preview <- readxl::read_excel(
    file,
    sheet = sheet,
    col_names = FALSE,
    n_max = max_rows
  )
  
  if (nrow(preview) == 0) {
    return(NA_integer_)
  }
  
  for (i in seq_len(nrow(preview))) {
    
    row_values <- as.character(
      unlist(preview[i, ], use.names = FALSE)
    )
    
    row_values[is.na(row_values)] <- ""
    
    cleaned_values <- clean_column_names(row_values)
    
    # Look for animal identification headings
    has_id <- any(
      cleaned_values %in% c(
        "idname",
        "animalid",
        "animalname",
        "id",
        "name"
      )
    )
    
    # Look for sex heading
    has_sex <- any(
      cleaned_values %in% c(
        "sex",
        "gender"
      )
    )
    
    # Only ID and Sex are needed to identify the register.
    if (has_id && has_sex) {
      
      return(i)
      
    }
    
  }
  
  return(NA_integer_)
  
}

# =========================================================
# READ AND STANDARDISE ONE ANIMAL REGISTER WORKSHEET
# =========================================================

read_animal_sheet <- function(file, sheet) {
  
  # Find the actual header row.
  header_row <- find_animal_header(
    file,
    sheet
  )
  
  # Skip worksheets that do not contain an animal register.
  if (is.na(header_row)) {
    return(NULL)
  }
  
  # Read from the animal-register header row.
  df <- readxl::read_excel(
    file,
    sheet = sheet,
    skip = header_row - 1,
    .name_repair = "unique"
  )
  
  df <- as.data.frame(df)
  
  if (nrow(df) == 0) {
    return(NULL)
  }
  
  # Remove completely empty rows.
  df <- df[
    rowSums(!is.na(df)) > 0,
    ,
    drop = FALSE
  ]
  
  if (nrow(df) == 0) {
    return(NULL)
  }
  
  # -------------------------------------------------------
  # IDENTIFY WORKSHEET COLUMNS
  # -------------------------------------------------------
  
  
  id_col <- find_column(
    df,
    c(
      "ID/Name",
      "ID / Name",
      "Animal ID",
      "Animal Name",
      "ID",
      "Name"
    )
  )
  
  sex_col <- find_column(
    df,
    c(
      "Sex",
      "Gender"
    )
  )
  
  dob_col <- find_column(
    df,
    c(
      "Date of Birth",
      "Date_of_Birth",
      "DOB",
      "Birth Date"
    )
  )
  
  age_col <- find_column(
    df,
    c(
      "Age(years)",
      "Age (years)",
      "Age Years",
      "Age",
      "Current Age"
    )
  )
  
  age_class_col <- find_column(
    df,
    c(
      "Age Class",
      "Age_Class",
      "Class"
    )
  )
  
  age_category_col <- find_column(
    df,
    c(
      "Age Category",
      "Age_Category"
    )
  )
  
  notch_col <- find_column(
    df,
    c(
      "Notch",
      "Notch Number",
      "Notch Status",
      "Notching"
    )
  )
  
  # -------------------------------------------------------
  # STANDARDISE THE ANIMAL DATA
  # -------------------------------------------------------
  
  animal_data <- data.frame(
    
    Animal_ID = trimws(
      as.character(df[[id_col]])
    ),
    
    Sex = if (!is.na(sex_col)) {
      standardise_sex(df[[sex_col]])
    } else {
      rep("Unknown", nrow(df))
    },
    
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
    
    Age_Class = if (!is.na(age_class_col)) {
      as.character(df[[age_class_col]])
    } else {
      rep(NA_character_, nrow(df))
    },
    
    Age_Category = if (!is.na(age_category_col)) {
      as.character(df[[age_category_col]])
    } else {
      rep(NA_character_, nrow(df))
    },
    
    Notch = if (!is.na(notch_col)) {
      as.character(df[[notch_col]])
    } else {
      rep(NA_character_, nrow(df))
    },
    
    Area_Last_Sighted = if (!is.na(area_col)) {
      as.character(df[[area_col]])
    } else {
      rep(NA_character_, nrow(df))
    },
    
    Date_Last_Sighting = if (!is.na(last_sighting_col)) {
      safe_date(df[[last_sighting_col]])
    } else {
      as.Date(rep(NA, nrow(df)))
    },
    
    Sightings_Last_6_Months = if (!is.na(sightings_col)) {
      safe_numeric(df[[sightings_col]])
    } else {
      rep(NA_real_, nrow(df))
    },
    
    Source_Sheet = rep(sheet, nrow(df)),
    
    stringsAsFactors = FALSE
    
  )
  
  # Remove blank IDs and summary rows.
  animal_data <- animal_data %>%
    dplyr::filter(
      !is.na(Animal_ID),
      Animal_ID != ""
    )
  
  if (nrow(animal_data) == 0) {
    return(NULL)
  }
  
  animal_data
  
}

# =========================================================
# PROCESS THE COMPLETE EXCEL WORKBOOK
# =========================================================

process_workbook <- function(file) {
  
  if (!is.character(file) ||
      length(file) != 1 ||
      is.na(file) ||
      !file.exists(file)) {
    
    stop("Please provide a valid Excel file path.")
    
  }
  
  # Get all worksheet names.
  sheets <- readxl::excel_sheets(file)
  
  if (length(sheets) == 0) {
    stop("The uploaded workbook does not contain worksheets.")
  }
  
  # Read worksheets and retain only animal registers.
  all_data <- lapply(
    sheets,
    function(sheet) {
      
      read_animal_sheet(
        file,
        sheet
      )
      
    }
  )
  
  # Remove worksheets that did not contain animal records.
  all_data <- Filter(
    Negate(is.null),
    all_data
  )
  
  if (length(all_data) == 0) {
    
    stop(
      paste(
        "No animal register was found.",
        "Check that at least one worksheet has columns for",
        "ID/Name, Sex, and Date of Birth or Age."
      )
    )
    
  }
  
  # Combine animal records.
  combined <- dplyr::bind_rows(all_data)
  
  # -------------------------------------------------------
  # CLEAN TEXT FIELDS
  # -------------------------------------------------------
  
  combined <- combined %>%
    dplyr::mutate(
      
      dplyr::across(
        c(
          Animal_ID,
          Age_Class,
          Age_Category,
          Notch,
          Area_Last_Sighted,
          Source_Sheet
        ),
        ~ trimws(as.character(.x))
      ),
      
      Sex = standardise_sex(Sex),
      
      # Clean missing age class values.
      Age_Class = dplyr::if_else(
        is.na(Age_Class) | Age_Class == "",
        "Unknown",
        Age_Class
      ),
      
      # Clean missing age category values.
      Age_Category = dplyr::if_else(
        is.na(Age_Category) | Age_Category == "",
        "Unknown",
        Age_Category
      ),
      
      # Standardise notch information.
      Notch_Status = dplyr::case_when(
        
        is.na(Notch) | Notch == "" ~ "Not Recorded",
        
        tolower(Notch) %in% c(
          "no",
          "none",
          "not notched",
          "n/a",
          "na"
        ) ~ "No Notch",
        
        TRUE ~ "Notched"
        
      )
      
    )
  
  # -------------------------------------------------------
  # CALCULATE AGE FROM DATE OF BIRTH
  # -------------------------------------------------------
  
  calculated_age <- as.numeric(
    difftime(
      Sys.Date(),
      combined$DOB,
      units = "days"
    )
  ) / 365.2425
  
  combined$Age <- ifelse(
    is.na(combined$Age) & !is.na(combined$DOB),
    calculated_age,
    combined$Age
  )
  
  # -------------------------------------------------------
  # DERIVE AGE CLASS WHEN NOT PROVIDED
  # -------------------------------------------------------
  
  derived_class <- derive_age_class(
    combined$Age
  )
  
  combined$Age_Class <- ifelse(
    is.na(combined$Age_Class) |
      combined$Age_Class == "Unknown" |
      combined$Age_Class == "",
    derived_class,
    combined$Age_Class
  )
  
  # -------------------------------------------------------
  # REMOVE DUPLICATE ANIMAL RECORDS
  # -------------------------------------------------------
  
  # Prefer the record with the most complete information.
  combined <- combined %>%
    dplyr::mutate(
      .completeness = rowSums(
        !is.na(
          dplyr::across(
            c(
              Sex,
              DOB,
              Age,
              Age_Class,
              Age_Category,
              Notch,
              Area_Last_Sighted,
              Date_Last_Sighting,
              Sightings_Last_6_Months
            )
          )
        )
      )
    ) %>%
    dplyr::arrange(
      dplyr::desc(.completeness)
    ) %>%
    dplyr::distinct(
      Animal_ID,
      .keep_all = TRUE
    ) %>%
    dplyr::select(
      -.completeness
    )
  
  # Return the final data frame.
  return(combined)
  
}

# =========================================================
# MAIN SERVER FUNCTION
# =========================================================

server <- function(input, output, session) {
  
  # =======================================================
  # READ UPLOADED WORKBOOK
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
        "Unclassified",
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
      sum(
        data$Sex %in% c("Unknown", "Unclassified"),
        na.rm = TRUE
      ),
      "Unknown / Unclassified Sex",
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
      aes(
        x = Sex,
        y = n,
        fill = Sex
      )
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
    
    plot_data <- data %>%
      dplyr::count(Age_Class)
    
    ggplot(
      plot_data,
      aes(
        x = Age_Class,
        y = n,
        fill = Age_Class
      )
    ) +
      
      geom_col() +
      
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
    
    plot_data <- data %>%
      dplyr::count(Age_Category)
    
    ggplot(
      plot_data,
      aes(
        x = Age_Category,
        y = n,
        fill = Age_Category
      )
    ) +
      
      geom_col() +
      
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
    
    plot_data <- data %>%
      dplyr::count(Notch_Status)
    
    ggplot(
      plot_data,
      aes(
        x = Notch_Status,
        y = n,
        fill = Notch_Status
      )
    ) +
      
      geom_col() +
      
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
      aes(
        x = Sex,
        fill = Sex
      )
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
        round(age, 1)
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
        round(age, 1)
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
    
    display_data <- data %>%
      dplyr::select(
        Animal_ID,
        Sex,
        DOB,
        Age,
        Age_Class,
        Age_Category,
        Notch,
        Notch_Status,
        Area_Last_Sighted,
        Date_Last_Sighting,
        Sightings_Last_6_Months,
        Source_Sheet
      )
    
    DT::datatable(
      
      display_data,
      
      colnames = c(
        "ID / Name",
        "Sex",
        "Date of Birth",
        "Age (Years)",
        "Age Class",
        "Age Category",
        "Notch Number",
        "Notch Status",
        "Area Last Sighted",
        "Date of Last Sighting",
        "Sightings in Last 6 Months",
        "Source Worksheet"
      ),
      
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
        row.names = FALSE,
        na = ""
      )
      
    }
    
  )
  
}