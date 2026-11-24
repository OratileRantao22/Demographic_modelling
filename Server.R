# ============================================================
# WILDLIFE POPULATION DEMOGRAPHIC DASHBOARD
# SERVER.R - SERVER FUNCTIONS
# ============================================================

library(shiny)
library(shinydashboard)
library(readxl)
library(DT)
library(dplyr)
library(ggplot2)

# ============================================================
# 1. EMPTY ANIMAL REGISTER
# ============================================================

empty_register <- data.frame(
  
  Animal_ID = character(),
  Sex = character(),
  Notch_Number = character(),
  Date_of_Birth = as.Date(character()),
  Age = numeric(),
  Age_Class = character(),
  Age_Category = character(),
  
  stringsAsFactors = FALSE
  
)


# ============================================================
# 2. HELPER FUNCTIONS
# ============================================================

# ------------------------------------------------------------
# STANDARDISE COLUMN HEADINGS
# ------------------------------------------------------------

normalise_header <- function(x) {
  
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("")
  }
  
  x <- tolower(trimws(as.character(x)))
  
  gsub("[^a-z0-9]", "", x)
  
}


# ------------------------------------------------------------
# SAFELY CONVERT VALUES TO TEXT
# ------------------------------------------------------------

safe_text <- function(x) {
  
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(NA_character_)
  }
  
  result <- trimws(as.character(x))
  
  if (result == "") {
    return(NA_character_)
  }
  
  result
  
}


# ------------------------------------------------------------
# CONVERT AGE TO NUMERIC
# ------------------------------------------------------------

safe_numeric <- function(x) {
  
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(NA_real_)
  }
  
  x <- gsub(",", "", as.character(x))
  
  x <- gsub("[^0-9.-]", "", x)
  
  result <- suppressWarnings(as.numeric(x))
  
  if (length(result) == 0 || !is.finite(result)) {
    return(NA_real_)
  }
  
  result
  
}


# ------------------------------------------------------------
# CONVERT EXCEL DATES
# ------------------------------------------------------------

safe_date <- function(x) {
  
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(as.Date(NA))
  }
  
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }
  
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  
  # Excel numeric date values
  
  if (is.numeric(x)) {
    
    if (x > 0) {
      return(as.Date(x, origin = "1899-12-30"))
    }
    
    return(as.Date(NA))
  }
  
  x <- trimws(as.character(x))
  
  if (x == "") {
    return(as.Date(NA))
  }
  
  formats <- c(
    "%Y-%m-%d",
    "%d/%m/%Y",
    "%m/%d/%Y",
    "%d-%m-%Y",
    "%Y/%m/%d",
    "%d %b %Y",
    "%d %B %Y"
  )
  
  for (format in formats) {
    
    result <- suppressWarnings(
      as.Date(x, format = format)
    )
    
    if (!is.na(result)) {
      return(result)
    }
    
  }
  
  as.Date(NA)
  
}


# ------------------------------------------------------------
# STANDARDISE SEX VALUES
# ------------------------------------------------------------

normalise_sex <- function(x) {
  
  x <- tolower(trimws(as.character(x)))
  
  if (is.na(x) || x == "") {
    return("Unknown")
  }
  
  if (x %in% c("m", "male", "bull", "boy")) {
    return("Male")
  }
  
  if (x %in% c("f", "female", "cow", "girl")) {
    return("Female")
  }
  
  "Unknown"
  
}


# ============================================================
# 3. FIND ANIMAL REGISTER HEADINGS
# ============================================================

find_register_header <- function(sheet_data) {
  
  if (nrow(sheet_data) == 0 || ncol(sheet_data) == 0) {
    return(NULL)
  }
  
  max_rows <- min(nrow(sheet_data), 50)
  
  for (r in seq_len(max_rows)) {
    
    headings <- vapply(
      
      seq_len(ncol(sheet_data)),
      
      function(j) {
        normalise_header(sheet_data[[j]][[r]])
      },
      
      character(1)
      
    )
    
    # Recognised ID headings
    
    id_aliases <- c(
      "id",
      "idname",
      "animalid",
      "animalname",
      "individualid",
      "name"
    )
    
    # Recognised sex headings
    
    sex_aliases <- c(
      "sex",
      "gender"
    )
    
    # Recognised date of birth headings
    
    dob_aliases <- c(
      "dateofbirth",
      "dob",
      "birthdate"
    )
    
    # Recognised age headings
    
    age_aliases <- c(
      "age",
      "ageyears",
      "ageinyears"
    )
    
    id_col <- which(headings %in% id_aliases)
    
    sex_col <- which(headings %in% sex_aliases)
    
    dob_col <- which(headings %in% dob_aliases)
    
    age_col <- which(headings %in% age_aliases)
    
    if (
      
      length(id_col) > 0 &&
      length(sex_col) > 0 &&
      (length(dob_col) > 0 || length(age_col) > 0)
      
    ) {
      
      return(list(
        
        header_row = r,
        
        id_col = id_col[1],
        
        sex_col = sex_col[1],
        
        dob_col = if (length(dob_col) > 0) dob_col[1] else NA_integer_,
        
        age_col = if (length(age_col) > 0) age_col[1] else NA_integer_,
        
        headings = headings
        
      ))
      
    }
    
  }
  
  NULL
  
}


# ============================================================
# 4. EXTRACT ANIMAL RECORDS FROM A WORKSHEET
# ============================================================

extract_sheet_animals <- function(sheet_data) {
  
  header_info <- find_register_header(sheet_data)
  
  if (is.null(header_info)) {
    return(empty_register)
  }
  
  header_row <- header_info$header_row
  
  id_col <- header_info$id_col
  
  sex_col <- header_info$sex_col
  
  dob_col <- header_info$dob_col
  
  age_col <- header_info$age_col
  
  headings <- header_info$headings
  
  # Find optional columns
  
  notch_col <- which(headings %in% c(
    "notch",
    "notchnumber",
    "animalnotch"
  ))
  
  age_class_col <- which(headings %in% c(
    "ageclass",
    "class"
  ))
  
  age_category_col <- which(headings %in% c(
    "agecategory",
    "category"
  ))
  
  get_value <- function(row, column) {
    
    if (is.na(column)) {
      return(NA)
    }
    
    if (column > ncol(sheet_data)) {
      return(NA)
    }
    
    sheet_data[[column]][[row]]
    
  }
  
  if (header_row >= nrow(sheet_data)) {
    return(empty_register)
  }
  
  records <- list()
  
  for (r in seq.int(header_row + 1, nrow(sheet_data))) {
    
    animal_id <- safe_text(
      get_value(r, id_col)
    )
    
    if (is.na(animal_id)) {
      next
    }
    
    # Exclude common summary rows
    
    if (tolower(animal_id) %in% c(
      "total",
      "grand total",
      "subtotal",
      "male",
      "female"
    )) {
      next
    }
    
    sex_value <- normalise_sex(
      get_value(r, sex_col)
    )
    
    dob_value <- if (!is.na(dob_col)) {
      
      safe_date(get_value(r, dob_col))
      
    } else {
      
      as.Date(NA)
      
    }
    
    age_value <- if (!is.na(age_col)) {
      
      safe_numeric(get_value(r, age_col))
      
    } else {
      
      NA_real_
      
    }
    
    # Calculate age from date of birth if age is unavailable
    
    if (
      is.na(age_value) &&
      !is.na(dob_value)
    ) {
      
      age_value <- as.numeric(
        difftime(
          Sys.Date(),
          dob_value,
          units = "days"
        )
      ) / 365.25
      
    }
    
    # Do not include obvious non-animal records
    
    records[[length(records) + 1]] <- data.frame(
      
      Animal_ID = animal_id,
      
      Sex = sex_value,
      
      Notch_Number = if (length(notch_col) > 0) {
        safe_text(get_value(r, notch_col[1]))
      } else {
        NA_character_
      },
      
      Date_of_Birth = dob_value,
      
      Age = age_value,
      
      Age_Class = if (length(age_class_col) > 0) {
        safe_text(get_value(r, age_class_col[1]))
      } else {
        NA_character_
      },
      
      Age_Category = if (length(age_category_col) > 0) {
        safe_text(get_value(r, age_category_col[1]))
      } else {
        NA_character_
      },
      
      stringsAsFactors = FALSE
      
    )
    
  }
  
  if (length(records) == 0) {
    return(empty_register)
  }
  
  result <- bind_rows(records)
  
  result <- result %>%
    filter(
      !is.na(Animal_ID),
      Animal_ID != ""
    )
  
  result
  
}


# ============================================================
# 5. READ ALL WORKSHEETS FROM THE EXCEL WORKBOOK
# ============================================================

read_wildlife_workbook <- function(file_path) {
  
  sheets <- readxl::excel_sheets(file_path)
  
  workbook <- list()
  
  for (sheet in sheets) {
    
    workbook[[sheet]] <- readxl::read_excel(
      
      file_path,
      
      sheet = sheet,
      
      col_names = FALSE,
      
      .name_repair = "minimal"
      
    )
    
  }
  
  workbook
  
}


# ============================================================
# 6. EXTRACT ANIMALS FROM ALL WORKSHEETS
# ============================================================

extract_workbook_animals <- function(workbook) {
  
  if (length(workbook) == 0) {
    return(empty_register)
  }
  
  all_records <- list()
  
  for (sheet_name in names(workbook)) {
    
    sheet_data <- workbook[[sheet_name]]
    
    sheet_animals <- extract_sheet_animals(sheet_data)
    
    if (nrow(sheet_animals) > 0) {
      
      all_records[[length(all_records) + 1]] <- sheet_animals
      
    }
    
  }
  
  if (length(all_records) == 0) {
    return(empty_register)
  }
  
  bind_rows(all_records) %>%
    
    distinct(Animal_ID, .keep_all = TRUE)
  
}


# ============================================================
# 7. SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ==========================================================
  # A. MANUAL ANIMAL RECORDS
  # ==========================================================
  
  animal_records <- reactiveVal(empty_register)
  
  
  # ==========================================================
  # B. READ UPLOADED WORKBOOK
  # ==========================================================
  
  workbook_data <- reactive({
    
    req(input$excel_file)
    
    tryCatch({
      
      read_wildlife_workbook(
        input$excel_file$datapath
      )
      
    }, error = function(e) {
      
      showNotification(
        
        paste(
          "Workbook error:",
          conditionMessage(e)
        ),
        
        type = "error",
        duration = 10
        
      )
      
      NULL
      
    })
    
  })
  
  
  # ==========================================================
  # C. EXTRACT UPLOADED ANIMAL RECORDS
  # ==========================================================
  
  uploaded_animals <- reactive({
    
    req(workbook_data())
    
    extract_workbook_animals(
      workbook_data()
    )
    
  })
  
  
  # ==========================================================
  # D. COMBINE UPLOADED AND MANUAL RECORDS
  # ==========================================================
  
  combined_data <- reactive({
    
    uploaded <- uploaded_animals()
    
    manual <- animal_records()
    
    combined <- bind_rows(
      uploaded,
      manual
    )
    
    # Remove duplicate animal IDs.
    # Uploaded records are kept first.
    
    combined <- combined %>%
      
      filter(
        !is.na(Animal_ID),
        Animal_ID != ""
      ) %>%
      
      distinct(
        Animal_ID,
        .keep_all = TRUE
      )
    
    combined
    
  })
  
  
  # ==========================================================
  # E. FILTER DATA
  # ==========================================================
  
  filtered_data <- reactive({
    
    data <- combined_data()
    
    if (nrow(data) == 0) {
      return(data)
    }
    
    if (
      !is.null(input$filter_sex) &&
      input$filter_sex != "All"
    ) {
      
      data <- data %>%
        
        filter(
          Sex == input$filter_sex
        )
      
    }
    
    if (
      !is.null(input$filter_age_class) &&
      input$filter_age_class != "All"
    ) {
      
      data <- data %>%
        
        filter(
          Age_Class == input$filter_age_class
        )
      
    }
    
    data
    
  })
  
  
  # ==========================================================
  # F. UPDATE AGE CLASS FILTER
  # ==========================================================
  
  observe({
    
    data <- combined_data()
    
    age_classes <- sort(unique(
      na.omit(data$Age_Class)
    ))
    
    updateSelectInput(
      
      session,
      
      "filter_age_class",
      
      choices = c(
        "All",
        age_classes
      ),
      
      selected = "All"
      
    )
    
  })
  
  
  # ==========================================================
  # G. WORKBOOK STATUS
  # ==========================================================
  
  output$workbook_status <- renderText({
    
    if (is.null(input$excel_file)) {
      
      return(
        "Please upload an Excel workbook to begin."
      )
      
    }
    
    workbook <- workbook_data()
    
    if (is.null(workbook)) {
      
      return(
        "The workbook could not be read. Check that it is a valid Excel file."
      )
      
    }
    
    records <- uploaded_animals()
    
    sheet_count <- length(workbook)
    
    if (nrow(records) > 0) {
      
      paste0(
        
        "Workbook loaded successfully. ",
        
        sheet_count,
        
        " worksheets found. ",
        
        nrow(records),
        
        " individual animal records detected."
        
      )
      
    } else {
      
      paste0(
        
        "Workbook loaded successfully. ",
        
        sheet_count,
        
        " worksheets found, but no individual animal register was detected. ",
        
        "The workbook worksheets can still be previewed below."
        
      )
      
    }
    
  })
  
  
  # ==========================================================
  # H. UPDATE WORKSHEET DROPDOWN
  # ==========================================================
  
  observeEvent(workbook_data(), {
    
    workbook <- workbook_data()
    
    if (is.null(workbook)) {
      return()
    }
    
    updateSelectInput(
      
      session,
      
      "workbook_sheet",
      
      choices = names(workbook),
      
      selected = names(workbook)[1]
      
    )
    
  })
  
  
  # ==========================================================
  # I. ADD MANUAL ANIMAL RECORD
  # ==========================================================
  
  observeEvent(input$add_animal, {
    
    req(input$animal_id)
    
    animal_id <- trimws(input$animal_id)
    
    if (animal_id == "") {
      
      showNotification(
        "Please enter an animal ID or name.",
        type = "error"
      )
      
      return()
      
    }
    
    if (
      is.na(input$animal_dob) &&
      is.na(input$animal_age)
    ) {
      
      showNotification(
        "Please enter either a date of birth or an age.",
        type = "error"
      )
      
      return()
      
    }
    
    existing <- combined_data()
    
    if (
      nrow(existing) > 0 &&
      animal_id %in% existing$Animal_ID
    ) {
      
      showNotification(
        "This animal ID already exists in the register.",
        type = "error"
      )
      
      return()
      
    }
    
    dob <- safe_date(input$animal_dob)
    
    age <- safe_numeric(input$animal_age)
    
    if (
      is.na(age) &&
      !is.na(dob)
    ) {
      
      age <- as.numeric(
        difftime(
          Sys.Date(),
          dob,
          units = "days"
        )
      ) / 365.25
      
    }
    
    new_animal <- data.frame(
      
      Animal_ID = animal_id,
      
      Sex = input$animal_sex,
      
      Notch_Number = safe_text(input$animal_notch),
      
      Date_of_Birth = dob,
      
      Age = age,
      
      Age_Class = safe_text(input$animal_age_class),
      
      Age_Category = safe_text(input$animal_age_category),
      
      stringsAsFactors = FALSE
      
    )
    
    updated <- bind_rows(
      animal_records(),
      new_animal
    )
    
    animal_records(updated)
    
    showNotification(
      "Animal record added successfully.",
      type = "message"
    )
    
    updateTextInput(
      session,
      "animal_id",
      value = ""
    )
    
    updateTextInput(
      session,
      "animal_notch",
      value = ""
    )
    
    updateNumericInput(
      session,
      "animal_age",
      value = NA
    )
    
    updateTextInput(
      session,
      "animal_age_class",
      value = ""
    )
    
    updateTextInput(
      session,
      "animal_age_category",
      value = ""
    )
    
  })
  
  
  # ==========================================================
  # J. TOTAL POPULATION
  # ==========================================================
  
  output$total_animals <- renderValueBox({
    
    data <- filtered_data()
    
    valueBox(
      
      value = nrow(data),
      
      subtitle = "Total Animals",
      
      icon = icon("paw"),
      
      color = "green"
      
    )
    
  })
  
  
  # ==========================================================
  # K. MALE POPULATION
  # ==========================================================
  
  output$male_animals <- renderValueBox({
    
    data <- filtered_data()
    
    total <- sum(
      data$Sex == "Male",
      na.rm = TRUE
    )
    
    valueBox(
      
      value = total,
      
      subtitle = "Male Animals",
      
      icon = icon("mars"),
      
      color = "blue"
      
    )
    
  })
  
  
  # ==========================================================
  # L. FEMALE POPULATION
  # ==========================================================
  
  output$female_animals <- renderValueBox({
    
    data <- filtered_data()
    
    total <- sum(
      data$Sex == "Female",
      na.rm = TRUE
    )
    
    valueBox(
      
      value = total,
      
      subtitle = "Female Animals",
      
      icon = icon("venus"),
      
      color = "purple"
      
    )
    
  })
  
  
  # ==========================================================
  # M. UNKNOWN SEX
  # ==========================================================
  
  output$unknown_animals <- renderValueBox({
    
    data <- filtered_data()
    
    total <- sum(
      data$Sex == "Unknown",
      na.rm = TRUE
    )
    
    valueBox(
      
      value = total,
      
      subtitle = "Unknown Sex",
      
      icon = icon("question"),
      
      color = "yellow"
      
    )
    
  })
  
  
  # ==========================================================
  # N. POPULATION BY SEX
  # ==========================================================
  
  output$sex_plot <- renderPlot({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No individual animal records are available."
      )
    )
    
    sex_data <- data %>%
      
      count(Sex)
    
    ggplot(
      sex_data,
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
      
      theme_minimal() +
      
      theme(
        legend.position = "none"
      )
    
  })
  
  
  # ==========================================================
  # O. POPULATION BY AGE CLASS
  # ==========================================================
  
  output$age_class_plot <- renderPlot({
    
    data <- filtered_data() %>%
      
      filter(
        !is.na(Age_Class),
        Age_Class != ""
      )
    
    validate(
      need(
        nrow(data) > 0,
        "No age-class information is available."
      )
    )
    
    age_data <- data %>%
      
      count(Age_Class)
    
    ggplot(
      age_data,
      aes(
        x = reorder(Age_Class, -n),
        y = n
      )
    ) +
      
      geom_col(fill = "#2e7d32") +
      
      geom_text(
        aes(label = n),
        vjust = -0.5
      ) +
      
      labs(
        x = "Age Class",
        y = "Number of Animals"
      ) +
      
      theme_minimal()
    
  })
  
  
  # ==========================================================
  # P. POPULATION BY AGE CATEGORY
  # ==========================================================
  
  output$age_category_plot <- renderPlot({
    
    data <- filtered_data() %>%
      
      filter(
        !is.na(Age_Category),
        Age_Category != ""
      )
    
    validate(
      need(
        nrow(data) > 0,
        "No age-category information is available."
      )
    )
    
    category_data <- data %>%
      
      count(Age_Category)
    
    ggplot(
      category_data,
      aes(
        x = reorder(Age_Category, -n),
        y = n
      )
    ) +
      
      geom_col(fill = "#66bb6a") +
      
      geom_text(
        aes(label = n),
        vjust = -0.5
      ) +
      
      labs(
        x = "Age Category",
        y = "Number of Animals"
      ) +
      
      theme_minimal()
    
  })
  
  
  # ==========================================================
  # Q. POPULATION BY NOTCH NUMBER
  # ==========================================================
  
  output$notch_plot <- renderPlot({
    
    data <- filtered_data() %>%
      
      filter(
        !is.na(Notch_Number),
        Notch_Number != ""
      )
    
    validate(
      need(
        nrow(data) > 0,
        "No notch-number information is available."
      )
    )
    
    notch_data <- data %>%
      
      count(Notch_Number) %>%
      
      arrange(desc(n)) %>%
      
      head(20)
    
    ggplot(
      notch_data,
      aes(
        x = reorder(Notch_Number, n),
        y = n
      )
    ) +
      
      geom_col(fill = "#388e3c") +
      
      coord_flip() +
      
      labs(
        x = "Notch Number",
        y = "Number of Animals"
      ) +
      
      theme_minimal()
    
  })
  
  
  # ==========================================================
  # R. DEMOGRAPHIC SEX DISTRIBUTION
  # ==========================================================
  
  output$demographic_sex_plot <- renderPlot({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No animal records are available."
      )
    )
    
    sex_data <- data %>%
      
      count(Sex)
    
    ggplot(
      sex_data,
      aes(
        x = "",
        y = n,
        fill = Sex
      )
    ) +
      
      geom_col(
        width = 1
      ) +
      
      coord_polar("y") +
      
      labs(
        x = NULL,
        y = NULL,
        fill = "Sex"
      ) +
      
      theme_void()
    
  })
  
  
  # ==========================================================
  # S. SEX BY AGE CLASS
  # ==========================================================
  
  output$sex_age_class_plot <- renderPlot({
    
    data <- filtered_data() %>%
      
      filter(
        !is.na(Age_Class),
        Age_Class != ""
      )
    
    validate(
      need(
        nrow(data) > 0,
        "No age-class information is available."
      )
    )
    
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
        y = "Number of Animals",
        fill = "Sex"
      ) +
      
      theme_minimal()
    
  })
  
  
  # ==========================================================
  # T. AGE CATEGORY BY SEX
  # ==========================================================
  
  output$category_sex_plot <- renderPlot({
    
    data <- filtered_data() %>%
      
      filter(
        !is.na(Age_Category),
        Age_Category != ""
      )
    
    validate(
      need(
        nrow(data) > 0,
        "No age-category information is available."
      )
    )
    
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
        y = "Number of Animals",
        fill = "Sex"
      ) +
      
      theme_minimal()
    
  })
  
  
  # ==========================================================
  # U. AVERAGE AGE
  # ==========================================================
  
  output$average_age <- renderValueBox({
    
    data <- filtered_data()
    
    ages <- data$Age[
      !is.na(data$Age) &
        is.finite(data$Age)
    ]
    
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
  
  
  # ==========================================================
  # V. YOUNGEST ANIMAL
  # ==========================================================
  
  output$youngest_age <- renderValueBox({
    
    data <- filtered_data()
    
    ages <- data$Age[
      !is.na(data$Age) &
        is.finite(data$Age)
    ]
    
    youngest <- if (length(ages) > 0) {
      round(min(ages), 1)
    } else {
      "N/A"
    }
    
    valueBox(
      
      value = youngest,
      
      subtitle = "Youngest Age (Years)",
      
      icon = icon("baby"),
      
      color = "aqua"
      
    )
    
  })
  
  
  # ==========================================================
  # W. OLDEST ANIMAL
  # ==========================================================
  
  output$oldest_age <- renderValueBox({
    
    data <- filtered_data()
    
    ages <- data$Age[
      !is.na(data$Age) &
        is.finite(data$Age)
    ]
    
    oldest <- if (length(ages) > 0) {
      round(max(ages), 1)
    } else {
      "N/A"
    }
    
    valueBox(
      
      value = oldest,
      
      subtitle = "Oldest Age (Years)",
      
      icon = icon("paw"),
      
      color = "purple"
      
    )
    
  })
  
  
  # ==========================================================
  # X. AGE DISTRIBUTION
  # ==========================================================
  
  output$age_histogram <- renderPlot({
    
    data <- filtered_data() %>%
      
      filter(
        !is.na(Age),
        is.finite(Age)
      )
    
    validate(
      need(
        nrow(data) > 0,
        "No age information is available."
      )
    )
    
    ggplot(
      data,
      aes(x = Age)
    ) +
      
      geom_histogram(
        bins = 15,
        fill = "#2e7d32",
        colour = "white"
      ) +
      
      labs(
        x = "Age (Years)",
        y = "Number of Animals"
      ) +
      
      theme_minimal()
    
  })
  
  
  # ==========================================================
  # Y. BIRTHS BY YEAR
  # ==========================================================
  
  output$birth_year_plot <- renderPlot({
    
    data <- filtered_data() %>%
      
      filter(
        !is.na(Date_of_Birth)
      ) %>%
      
      mutate(
        Birth_Year = format(
          Date_of_Birth,
          "%Y"
        )
      ) %>%
      
      count(Birth_Year)
    
    validate(
      need(
        nrow(data) > 0,
        "No date-of-birth information is available."
      )
    )
    
    ggplot(
      data,
      aes(
        x = Birth_Year,
        y = n
      )
    ) +
      
      geom_col(fill = "#43a047") +
      
      geom_text(
        aes(label = n),
        vjust = -0.5
      ) +
      
      labs(
        x = "Birth Year",
        y = "Number of Animals"
      ) +
      
      theme_minimal()
    
  })
  
  
  # ==========================================================
  # Z. ANIMAL REGISTER TABLE
  # ==========================================================
  
  output$register_table <- renderDT({
    
    data <- filtered_data()
    
    DT::datatable(
      
      data,
      
      rownames = FALSE,
      
      options = list(
        
        pageLength = 10,
        
        scrollX = TRUE,
        
        autoWidth = TRUE
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # AA. DOWNLOAD ANIMAL REGISTER
  # ==========================================================
  
  output$download_register <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "Wildlife_Animal_Register_",
        Sys.Date(),
        ".csv"
      )
      
    },
    
    content = function(file) {
      
      write.csv(
        combined_data(),
        file,
        row.names = FALSE,
        na = ""
      )
      
    }
    
  )
  
  
  # ==========================================================
  # AB. PREVIEW SELECTED WORKSHEET
  # ==========================================================
  
  output$workbook_preview <- renderDT({
    
    req(workbook_data())
    
    req(input$workbook_sheet)
    
    workbook <- workbook_data()
    
    req(input$workbook_sheet %in% names(workbook))
    
    preview <- as.data.frame(
      workbook[[input$workbook_sheet]]
    )
    
    # Give the columns readable names
    
    colnames(preview) <- paste0(
      "Column_",
      seq_len(ncol(preview))
    )
    
    DT::datatable(
      
      preview,
      
      rownames = TRUE,
      
      options = list(
        
        pageLength = 15,
        
        scrollX = TRUE,
        
        scrollY = "500px",
        
        autoWidth = TRUE
        
      )
      
    )
    
  })
  
  
}

