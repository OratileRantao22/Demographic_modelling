
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(scales)
library(readxl)


# =========================================================
# HELPER FUNCTIONS
# =========================================================

# Safely convert values to numeric
safe_numeric <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1]) ||
      identical(trimws(as.character(x[1])), "")) {
    return(NA_real_)
  }
  
  suppressWarnings(
    as.numeric(gsub(",", "", as.character(x[1])))
  )
}


# Safely convert values to Date
safe_date <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) {
    return(as.Date(NA))
  }
  
  if (inherits(x, "Date")) {
    return(as.Date(x[1]))
  }
  
  if (inherits(x, "POSIXt")) {
    return(as.Date(x[1]))
  }
  
  suppressWarnings(as.Date(x[1]))
}


# Standardise sex values
standardise_sex <- function(x) {
  x <- trimws(tolower(as.character(x)))
  
  if (is.na(x) || x == "") {
    return("Unknown")
  }
  
  if (x %in% c("m", "male")) {
    return("Male")
  }
  
  if (x %in% c("f", "female")) {
    return("Female")
  }
  
  if (x %in% c("unclassified", "unclass", "u")) {
    return("Unclassified")
  }
  
  "Unknown"
}


# Calculate age from date of birth
calculate_age <- function(dob) {
  if (is.na(dob)) {
    return(NA_real_)
  }
  
  as.numeric(
    difftime(Sys.Date(), dob, units = "days")
  ) / 365.25
}


# Derive age class using the age ranges previously specified
derive_age_class <- function(age) {
  if (is.na(age)) {
    return("Unknown")
  }
  
  if (age < 0.25) {
    return("A")
  } else if (age >= 0.25 && age < 1) {
    return("B")
  } else if (age >= 1 && age < 2) {
    return("C")
  } else if (age >= 2 && age < 3.5) {
    return("D")
  } else if (age >= 3.5 && age <= 6.9) {
    return("E")
  } else if (age > 7) {
    return("F")
  }
  
  "Unknown"
}


# Return an empty plot when no records are available
empty_plot <- function(message = "No animal records available") {
  ggplot() +
    annotate(
      "text",
      x = 1,
      y = 1,
      label = message,
      size = 5,
      colour = "grey40"
    ) +
    xlim(0, 2) +
    ylim(0, 2) +
    theme_void()
}


# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session) {
  
  # =======================================================
  # 1. CREATE EMPTY ANIMAL REGISTER
  # =======================================================
  
  empty_register <- data.frame(
    Animal_ID = character(),
    Sex = character(),
    DOB = as.Date(character()),
    Age = numeric(),
    Age_Class = character(),
    Age_Category = character(),
    Notch = character(),
    Notch_Status = character(),
    Area_Last_Sighted = character(),
    Date_Last_Sighting = as.Date(character()),
    Sightings_Last_6_Months = numeric(),
    Source_Sheet = character(),
    stringsAsFactors = FALSE
  )
  
  animal_records <- reactiveVal(empty_register)
  
  # =======================================================
  # EXCEL WORKBOOK UPLOAD AND PREVIEW
  # =======================================================
  
  # Store worksheet names
  workbook_sheets <- reactiveVal(character())
  
  
  # Detect uploaded workbook and retrieve worksheet names
  observeEvent(input$excel_file, {
    
    req(input$excel_file)
    
    uploaded_file <- input$excel_file$datapath
    
    tryCatch({
      
      sheets <- readxl::excel_sheets(uploaded_file)
      
      if (length(sheets) == 0) {
        stop("The workbook does not contain any worksheets.")
      }
      
      workbook_sheets(sheets)
      
      updateSelectInput(
        session,
        "workbook_sheet",
        choices = sheets,
        selected = sheets[1]
      )
      
      showNotification(
        paste(
          "Workbook uploaded successfully.",
          length(sheets),
          "worksheet(s) found."
        ),
        type = "message"
      )
      
    }, error = function(e) {
      
      workbook_sheets(character())
      
      updateSelectInput(
        session,
        "workbook_sheet",
        choices = c("No workbook uploaded" = ""),
        selected = ""
      )
      
      showNotification(
        paste("Workbook upload error:", e$message),
        type = "error",
        duration = NULL
      )
      
    })
    
  })
  
  
  # Display workbook upload information
  output$workbook_status <- renderText({
    
    req(input$excel_file)
    
    sheets <- workbook_sheets()
    
    if (length(sheets) == 0) {
      return("No worksheets could be read from this workbook.")
    }
    
    paste0(
      "Uploaded workbook: ",
      input$excel_file$name,
      "\nNumber of worksheets: ",
      length(sheets),
      "\nWorksheets: ",
      paste(sheets, collapse = ", ")
    )
    
  })
  
  
  # Read and preview selected worksheet
  workbook_preview_data <- reactive({
    
    req(input$excel_file)
    req(input$workbook_sheet)
    
    validate(
      need(
        input$workbook_sheet %in% workbook_sheets(),
        "Please select a valid worksheet."
      )
    )
    
    preview <- readxl::read_excel(
      input$excel_file$datapath,
      sheet = input$workbook_sheet,
      col_names = FALSE,
      n_max = 40
    )
    
    preview <- as.data.frame(preview)
    
    # Limit preview to first 30 columns
    if (ncol(preview) > 30) {
      preview <- preview[, 1:30, drop = FALSE]
    }
    
    # Give columns readable temporary names
    names(preview) <- paste("Column", seq_len(ncol(preview)))
    
    preview
    
  })
  
  
  # Display worksheet preview
  output$workbook_preview <- DT::renderDT({
    
    preview <- workbook_preview_data()
    
    DT::datatable(
      preview,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollY = "400px",
        autoWidth = TRUE
      )
    )
    
  })
  
  
  
  # =======================================================
  # 2. ADD ANIMAL TO REGISTER
  # =======================================================
  
  observeEvent(input$add_animal, {
    
    animal_id <- trimws(input$entry_id)
    
    # Validate animal ID
    if (is.null(animal_id) || animal_id == "") {
      showNotification(
        "Please enter an Animal ID or Name.",
        type = "error"
      )
      return()
    }
    
    current_data <- animal_records()
    
    # Check for duplicate animal ID
    if (any(
      tolower(current_data$Animal_ID) == tolower(animal_id)
    )) {
      showNotification(
        "This Animal ID or Name already exists in the register.",
        type = "error"
      )
      return()
    }
    
    # Standardise sex
    animal_sex <- standardise_sex(input$entry_sex)
    
    # Get date of birth
    animal_dob <- safe_date(input$entry_dob)
    
    # Get manually entered age
    entered_age <- safe_numeric(input$entry_age)
    
    # Calculate age from DOB if available
    if (!is.na(animal_dob)) {
      
      if (animal_dob > Sys.Date()) {
        showNotification(
          "Date of birth cannot be in the future.",
          type = "error"
        )
        return()
      }
      
      animal_age <- calculate_age(animal_dob)
      
    } else {
      animal_age <- entered_age
    }
    
    # Validate age
    if (!is.na(animal_age) && animal_age < 0) {
      showNotification(
        "Age cannot be negative.",
        type = "error"
      )
      return()
    }
    
    # Determine age class
    selected_age_class <- input$entry_age_class
    
    if (is.null(selected_age_class) ||
        selected_age_class == "Unknown") {
      
      animal_age_class <- derive_age_class(animal_age)
      
    } else {
      animal_age_class <- selected_age_class
    }
    
    # Determine age category
    animal_age_category <- trimws(input$entry_age_category)
    
    if (is.null(animal_age_category) ||
        animal_age_category == "") {
      animal_age_category <- "Unknown"
    }
    
    # Notch details
    animal_notch <- trimws(input$entry_notch)
    
    if (is.null(animal_notch) || animal_notch == "") {
      animal_notch <- "Not Recorded"
    }
    
    animal_notch_status <- input$entry_notch_status
    
    if (is.null(animal_notch_status) ||
        animal_notch_status == "") {
      animal_notch_status <- "Unknown"
    }
    
    # Last sighting details
    animal_area <- trimws(input$entry_area)
    
    if (is.null(animal_area) || animal_area == "") {
      animal_area <- "Unknown"
    }
    
    animal_sighting_date <- safe_date(input$entry_sighting_date)
    
    if (!is.na(animal_sighting_date) &&
        animal_sighting_date > Sys.Date()) {
      showNotification(
        "Date of last sighting cannot be in the future.",
        type = "error"
      )
      return()
    }
    
    animal_sightings <- safe_numeric(input$entry_sightings)
    
    if (is.na(animal_sightings)) {
      animal_sightings <- 0
    }
    
    if (animal_sightings < 0 ||
        animal_sightings != floor(animal_sightings)) {
      showNotification(
        "Number of sightings must be a non-negative whole number.",
        type = "error"
      )
      return()
    }
    
    # Create new animal record
    new_animal <- data.frame(
      Animal_ID = animal_id,
      Sex = animal_sex,
      DOB = animal_dob,
      Age = animal_age,
      Age_Class = animal_age_class,
      Age_Category = animal_age_category,
      Notch = animal_notch,
      Notch_Status = animal_notch_status,
      Area_Last_Sighted = animal_area,
      Date_Last_Sighting = animal_sighting_date,
      Sightings_Last_6_Months = animal_sightings,
      Source_Sheet = "Manual Entry",
      stringsAsFactors = FALSE
    )
    
    # Add new animal to existing register
    updated_data <- bind_rows(
      current_data,
      new_animal
    )
    
    animal_records(updated_data)
    
    showNotification(
      paste(animal_id, "was successfully added to the register."),
      type = "message"
    )
    
    # Clear form after successful addition
    updateTextInput(session, "entry_id", value = "")
    updateSelectInput(session, "entry_sex", selected = "Unknown")
    updateDateInput(session, "entry_dob", value = NA)
    updateNumericInput(session, "entry_age", value = NA)
    updateSelectInput(session, "entry_age_class", selected = "Unknown")
    updateTextInput(session, "entry_age_category", value = "")
    updateTextInput(session, "entry_notch", value = "")
    updateSelectInput(
      session,
      "entry_notch_status",
      selected = "Unknown"
    )
    updateTextInput(session, "entry_area", value = "")
    updateDateInput(session, "entry_sighting_date", value = NA)
    updateNumericInput(session, "entry_sightings", value = 0)
    
  })
  
  
  # =======================================================
  # 3. CLEAR REGISTER FORM
  # =======================================================
  
  observeEvent(input$clear_register_form, {
    
    updateTextInput(session, "entry_id", value = "")
    
    updateSelectInput(
      session,
      "entry_sex",
      selected = "Unknown"
    )
    
    updateDateInput(
      session,
      "entry_dob",
      value = NA
    )
    
    updateNumericInput(
      session,
      "entry_age",
      value = NA
    )
    
    updateSelectInput(
      session,
      "entry_age_class",
      selected = "Unknown"
    )
    
    updateTextInput(
      session,
      "entry_age_category",
      value = ""
    )
    
    updateTextInput(
      session,
      "entry_notch",
      value = ""
    )
    
    updateSelectInput(
      session,
      "entry_notch_status",
      selected = "Unknown"
    )
    
    updateTextInput(
      session,
      "entry_area",
      value = ""
    )
    
    updateDateInput(
      session,
      "entry_sighting_date",
      value = NA
    )
    
    updateNumericInput(
      session,
      "entry_sightings",
      value = 0
    )
    
  })
  
  
  # =======================================================
  # 4. FILTER ANIMAL RECORDS
  # =======================================================
  
  filtered_data <- reactive({
    
    data <- animal_records()
    
    if (input$sex_filter != "All") {
      data <- data %>%
        filter(Sex == input$sex_filter)
    }
    
    if (input$age_class_filter != "All") {
      data <- data %>%
        filter(Age_Class == input$age_class_filter)
    }
    
    if (input$age_category_filter != "All") {
      data <- data %>%
        filter(Age_Category == input$age_category_filter)
    }
    
    data
  })
  
  
  # =======================================================
  # 5. UPDATE FILTER OPTIONS
  # =======================================================
  
  observe({
    
    data <- animal_records()
    
    age_classes <- sort(unique(data$Age_Class))
    age_classes <- age_classes[!is.na(age_classes)]
    
    age_categories <- sort(unique(data$Age_Category))
    age_categories <- age_categories[!is.na(age_categories)]
    
    current_class <- isolate(input$age_class_filter)
    current_category <- isolate(input$age_category_filter)
    
    class_choices <- c("All", age_classes)
    category_choices <- c("All", age_categories)
    
    updateSelectInput(
      session,
      "age_class_filter",
      choices = class_choices,
      selected = if (!is.null(current_class) &&
                     current_class %in% class_choices) {
        current_class
      } else {
        "All"
      }
    )
    
    updateSelectInput(
      session,
      "age_category_filter",
      choices = category_choices,
      selected = if (!is.null(current_category) &&
                     current_category %in% category_choices) {
        current_category
      } else {
        "All"
      }
    )
    
  })
  
  
  # =======================================================
  # 6. POPULATION OVERVIEW VALUE BOXES
  # =======================================================
  
  output$total_animals <- renderValueBox({
    
    valueBox(
      value = nrow(filtered_data()),
      subtitle = "Total Animals",
      icon = icon("paw"),
      color = "green"
    )
    
  })
  
  
  output$male_animals <- renderValueBox({
    
    valueBox(
      value = sum(filtered_data()$Sex == "Male", na.rm = TRUE),
      subtitle = "Male Animals",
      icon = icon("mars"),
      color = "blue"
    )
    
  })
  
  
  output$female_animals <- renderValueBox({
    
    valueBox(
      value = sum(filtered_data()$Sex == "Female", na.rm = TRUE),
      subtitle = "Female Animals",
      icon = icon("venus"),
      color = "purple"
    )
    
  })
  
  
  output$unknown_animals <- renderValueBox({
    
    valueBox(
      value = sum(
        filtered_data()$Sex %in% c("Unknown", "Unclassified"),
        na.rm = TRUE
      ),
      subtitle = "Unknown / Unclassified",
      icon = icon("question-circle"),
      color = "yellow"
    )
    
  })
  
  
  # =======================================================
  # 7. POPULATION BY SEX
  # =======================================================
  
  output$sex_plot <- renderPlot({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(empty_plot())
    }
    
    plot_data <- data %>%
      count(Sex)
    
    ggplot(
      plot_data,
      aes(x = Sex, y = n, fill = Sex)
    ) +
      geom_col() +
      geom_text(
        aes(label = n),
        vjust = -0.4,
        size = 5
      ) +
      scale_fill_brewer(palette = "Greens") +
      labs(
        x = "Sex",
        y = "Number of Animals"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  
  # =======================================================
  # 8. POPULATION BY AGE CLASS
  # =======================================================
  
  output$age_class_plot <- renderPlot({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(empty_plot())
    }
    
    plot_data <- data %>%
      count(Age_Class)
    
    ggplot(
      plot_data,
      aes(x = Age_Class, y = n)
    ) +
      geom_col(fill = "#2e8b57") +
      geom_text(
        aes(label = n),
        vjust = -0.4,
        size = 5
      ) +
      labs(
        x = "Age Class",
        y = "Number of Animals"
      ) +
      theme_minimal()
  })
  
  
  # =======================================================
  # 9. POPULATION BY AGE CATEGORY
  # =======================================================
  
  output$age_category_plot <- renderPlot({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(empty_plot())
    }
    
    plot_data <- data %>%
      count(Age_Category)
    
    ggplot(
      plot_data,
      aes(
        x = reorder(Age_Category, n),
        y = n
      )
    ) +
      geom_col(fill = "#66bb6a") +
      coord_flip() +
      geom_text(
        aes(label = n),
        hjust = -0.3,
        size = 5
      ) +
      labs(
        x = "Age Category",
        y = "Number of Animals"
      ) +
      theme_minimal()
  })
  
  
  # =======================================================
  # 10. NOTCH STATUS
  # =======================================================
  
  output$notch_plot <- renderPlot({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(empty_plot())
    }
    
    plot_data <- data %>%
      count(Notch_Status)
    
    ggplot(
      plot_data,
      aes(x = Notch_Status, y = n, fill = Notch_Status)
    ) +
      geom_col() +
      geom_text(
        aes(label = n),
        vjust = -0.4,
        size = 5
      ) +
      scale_fill_brewer(palette = "Greens") +
      labs(
        x = "Notch Status",
        y = "Number of Animals"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  
  # =======================================================
  # 11. DEMOGRAPHIC SEX DISTRIBUTION
  # =======================================================
  
  output$demographic_sex_plot <- renderPlot({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(empty_plot())
    }
    
    plot_data <- data %>%
      count(Sex)
    
    ggplot(
      plot_data,
      aes(x = "", y = n, fill = Sex)
    ) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      labs(fill = "Sex") +
      theme_void()
  })
  
  
  # =======================================================
  # 12. SEX BY AGE CLASS
  # =======================================================
  
  output$sex_age_class_plot <- renderPlot({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(empty_plot())
    }
    
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
      geom_col(position = "dodge") +
      labs(
        x = "Age Class",
        y = "Number of Animals",
        fill = "Sex"
      ) +
      scale_fill_brewer(palette = "Greens") +
      theme_minimal()
  })
  
  
  # =======================================================
  # 13. AGE CATEGORY BY SEX
  # =======================================================
  
  output$age_category_sex_plot <- renderPlot({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(empty_plot())
    }
    
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
      geom_col(position = "dodge") +
      labs(
        x = "Age Category",
        y = "Number of Animals",
        fill = "Sex"
      ) +
      scale_fill_brewer(palette = "Greens") +
      theme_minimal()
  })
  
  
  # =======================================================
  # 14. AGE ANALYSIS VALUE BOXES
  # =======================================================
  
  output$average_age <- renderValueBox({
    
    ages <- filtered_data()$Age
    ages <- ages[!is.na(ages)]
    
    average <- if (length(ages) > 0) {
      round(mean(ages), 1)
    } else {
      "N/A"
    }
    
    valueBox(
      value = average,
      subtitle = "Average Age (Years)",
      icon = icon("calculator"),
      color = "green"
    )
    
  })
  
  
  output$youngest_age <- renderValueBox({
    
    ages <- filtered_data()$Age
    ages <- ages[!is.na(ages)]
    
    youngest <- if (length(ages) > 0) {
      round(min(ages), 1)
    } else {
      "N/A"
    }
    
    valueBox(
      value = youngest,
      subtitle = "Youngest Animal (Years)",
      icon = icon("child"),
      color = "blue"
    )
    
  })
  
  
  output$oldest_age <- renderValueBox({
    
    ages <- filtered_data()$Age
    ages <- ages[!is.na(ages)]
    
    oldest <- if (length(ages) > 0) {
      round(max(ages), 1)
    } else {
      "N/A"
    }
    
    valueBox(
      value = oldest,
      subtitle = "Oldest Animal (Years)",
      icon = icon("paw"),
      color = "purple"
    )
    
  })
  
  
  # =======================================================
  # 15. AGE HISTOGRAM
  # =======================================================
  
  output$age_histogram <- renderPlot({
    
    data <- filtered_data() %>%
      filter(!is.na(Age))
    
    if (nrow(data) == 0) {
      return(empty_plot("No age information available"))
    }
    
    ggplot(
      data,
      aes(x = Age)
    ) +
      geom_histogram(
        bins = 10,
        fill = "#2e8b57",
        colour = "white"
      ) +
      labs(
        x = "Age (Years)",
        y = "Number of Animals"
      ) +
      theme_minimal()
  })
  
  
  # =======================================================
  # 16. BIRTHS BY YEAR
  # =======================================================
  
  output$births_by_year <- renderPlot({
    
    data <- filtered_data() %>%
      filter(!is.na(DOB)) %>%
      mutate(Birth_Year = format(DOB, "%Y")) %>%
      count(Birth_Year)
    
    if (nrow(data) == 0) {
      return(empty_plot("No dates of birth available"))
    }
    
    ggplot(
      data,
      aes(x = Birth_Year, y = n)
    ) +
      geom_col(fill = "#66bb6a") +
      geom_text(
        aes(label = n),
        vjust = -0.4
      ) +
      labs(
        x = "Birth Year",
        y = "Number of Animals"
      ) +
      theme_minimal()
  })
  
  
  # =======================================================
  # 17. ANIMAL REGISTER TABLE
  # =======================================================
  
  output$animal_table <- renderDT({
    
    data <- filtered_data()
    
    display_data <- data %>%
      select(
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
        Sightings_Last_6_Months
      )
    
    DT::datatable(
      display_data,
      rownames = FALSE,
      filter = "top",
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      )
    )
    
  })
  
  
  # =======================================================
  # 18. DOWNLOAD ANIMAL REGISTER
  # =======================================================
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      paste0(
        "Wildlife_Animal_Register_",
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

