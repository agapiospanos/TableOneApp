library(shiny)
library(readxl)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(shinyjs)

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 30*1024^2)
  
  source('step1SheetSel.R', local = T)
  source('step2ColSel.R', local = T)
  source('step3GroupSpecification.R', local = T)
  source('step4DichotomousSel.R', local = T)
  source('step5OrdinalSel.R', local = T)
  source('step6SetMedianIqr.R', local = T)
  source('step7SetVarNames.R', local = T)
  source('step8OutputWord.R', local = T)
  
  rv <- reactiveValues(
    all_excel_cols = c(),
    var_list_no_group = c(),
    var_names = c(),
    data_levels = c()
  )

  # ---------------------------------------- STEP 1 --------------------------------------------------------
  
  observeEvent(input$excel_file, {
    if (!is.na(input$excel_file$datapath)) {
      step1SheetSel(input, output, session)
    }
  })
  
  observeEvent(input$sheet, {
    removeUI(selector = "#tab1_gotab2")
    insertUI(
      selector = "#gostep2",
      where = "beforeEnd",
      ui = actionButton(inputId = "tab1_gotab2", class = "gotab2", label = "Next")
    )
  })
  
  observeEvent(input$tab1_gotab2, {
    step2ColSel(input, output, session, rv)
    updateTabsetPanel(session, "NavBar", selected = "panel2")
  })
  
  # ---------------------------------------- STEP 2 -------------------------------------------------------
  
  observeEvent(input$tab2_gotab1, {
    updateTabsetPanel(session, "NavBar", selected = "panel1")
    removeUI(selector = "#tab1_gotab2")
    insertUI(
      selector = "#gostep2",
      where = "beforeEnd",
      ui = actionButton(inputId = "tab1_gotab2", class = "gotab2", label = "Next")
    )
  })
  
  observeEvent(input$tab2_gotab3, {
    if (is.null(input$selected_cols)) {
      removeUI(selector = ".error-message", multiple = TRUE)
      insertUI(
        selector = "#step2nav",
        where = "beforeBegin",
        ui = h6(class = "error-message", "Please choose at least one column to continue!")
      )
    } else {
      step3GroupSpecification(input, output, session)
      updateTabsetPanel(session, "NavBar", selected = "panel3")
    }
  })
  
  # ---------------------------------------- STEP 3 --------------------------------------------------------
  
  observeEvent(input$grouping_col, {
    
    index_of_sel <- which(rv$all_excel_cols %in% input$grouping_col)
    data <- read_excel(input$excel_file$datapath, as.numeric(input$sheet), range = cell_cols(index_of_sel))
    data_levels <- levels(as.factor(unlist(data)))
    rv$data_levels <- data_levels
    
    removeUI(selector = "#treatment_val, #control_val, #treatment_var .form-group, #control_var .form-group", multiple = TRUE)
    insertUI(
      selector = "#treatment_var",
      where = "afterBegin",
      ui = radioGroupButtons(inputId = "treatment_val", label = "Select treatment group value", choices = data_levels, selected = character(0))
    )
    
  })
  
  observeEvent(input$treatment_val, {
    removeUI(selector = "#control_val, #control_var .form-group", multiple = TRUE)
    rest_data_levels <- rv$data_levels[-which(rv$data_levels == input$treatment_val)]
    insertUI(
      selector = "#control_var",
      where = "afterBegin",
      ui = radioGroupButtons(inputId = "control_val", label = "Select control group value", choices = rest_data_levels, selected = character(0))
    )
  })
  
  observeEvent(input$control_val, {
    removeUI(selector = "#tab3_gotab4", multiple = TRUE)
    insertUI(
      selector = "#step3nav",
      where = "beforeEnd",
      ui = actionButton(inputId = "tab3_gotab4", class = "gotab4", label = "Next")
    )
  })
  
  observeEvent(input$tab3_gotab2, {
    removeUI(selector = ".error-message", multiple = TRUE)
    updateTabsetPanel(session, "NavBar", selected = "panel2")
  })
  
  observeEvent(input$tab3_gotab4, {
    removeUI(selector = ".error-message", multiple = TRUE)
    if (length(input$grouping_col) > 0 & length(input$treatment_val) > 0 & length(input$control_val) > 0) {
      step4DichotomousSel(input, output, session, rv)
      updateTabsetPanel(session, "NavBar", selected = "panel4")
    } else {
      insertUI(
        selector = "#step3nav",
        where = "beforeBegin",
        ui = h6(class = "error-message", "Please make sure that you chose at least one option for all the above fields!")
      )
    }
  })
  
  # ---------------------------------------- STEP 4 --------------------------------------------------------
  
  observeEvent(input$tab4_gotab5, {
    step5OrdinalSel(input, output, session, rv)
    updateTabsetPanel(session, "NavBar", selected = "panel5")
  })
  
  observeEvent(input$tab4_gotab3, {
    removeUI(selector = ".error-message", multiple = TRUE)
    updateTabsetPanel(session, "NavBar", selected = "panel3")
  })
  
  # ---------------------------------------- STEP 5 --------------------------------------------------------
  
  observeEvent(input$tab5_gotab6, {
    step6SetMedianIqr(input, output, session, rv)
    updateTabsetPanel(session, "NavBar", selected = "panel6")
  })
  
  observeEvent(input$tab5_gotab4, {
    removeUI(selector = ".error-message", multiple = TRUE)
    updateTabsetPanel(session, "NavBar", selected = "panel4")
  })
  
  # ---------------------------------------- STEP 6 --------------------------------------------------------
  
  observeEvent(input$tab6_gotab7, {
    step7SetVarNames(input, output, session, rv)
    updateTabsetPanel(session, "NavBar", selected = "panel7")
  })
  
  observeEvent(input$tab6_gotab5, {
    removeUI(selector = ".error-message", multiple = TRUE)
    updateTabsetPanel(session, "NavBar", selected = "panel5")
  })
  
  # ---------------------------------------- STEP 7 --------------------------------------------------------
  
  observeEvent(input$tab7_gotab8, {
    
    removeUI(selector = ".error-message", multiple = TRUE)
    all_fields_filled <- TRUE
    var_list <- rv$var_list_no_group
    var_names <- c()
    
    for (i in 1:length(var_list)) {
      field <- input[[var_list[i]]]
      var_names <- c(var_names, field)  
      if (is.null(field) | field == "") {
        all_fields_filled <- FALSE
      }
    }
    rv$var_names <- var_names
    
    if (all_fields_filled) {
      step8OutputWord(input, output, session, rv)
      updateTabsetPanel(session, "NavBar", selected = "panel8")
      
      shinyjs::runjs(code = '$(".container-fluid").addClass("wide");')
    } else {
      insertUI(
        selector = "#step7nav",
        where = "beforeBegin",
        ui = h6(class = "error-message", "Please fill all the above fields!")
      )
    }
    
  })
  
  observeEvent(input$tab7_gotab6, {
    removeUI(selector = ".error-message", multiple = TRUE)
    updateTabsetPanel(session, "NavBar", selected = "panel6")
  })
  
  # ---------------------------------------- STEP 8 --------------------------------------------------------
  
  observeEvent(input$tab8_gotab7, {
    removeUI(selector = ".error-message", multiple = TRUE)
    updateTabsetPanel(session, "NavBar", selected = "panel7")
  })
  
  observeEvent(input$tab8_reset, {
    shinyjs::runjs(code = 'location.reload();')
  })
}