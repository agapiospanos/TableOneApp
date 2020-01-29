step2EventObservers <- function (input, output, rv, session) {
  
  # Back Button observer
  observeEvent(input$tab2_gotab1, {
    updateTabsetPanel(session, "NavBar", selected = "panel1")
    removeUI(selector = "#tab1_gotab2")
    # insertUI(
    #   selector = "#gostep2",
    #   where = "beforeEnd",
    #   ui = actionButton(inputId = "tab1_gotab2", class = "gotab2", label = "Next")
    # )
  })
  
  # Next button observer
  observeEvent(input$tab2_gotab3, {
    if (is.null(input$selected_cols)) {
      removeUI(selector = ".error-message", multiple = TRUE)
      insertUI(
        selector = "#step2nav",
        where = "beforeBegin",
        ui = h6(class = "error-message", "Please choose at least one column to continue!")
      )
    } else {
      step3GroupSpecification(input, output, session, rv)
      updateTabsetPanel(session, "NavBar", selected = "panel3")
      
      # This workaround is required because when we go back from step3 the observeEvents are not triggered 
      # when we go from step 2 to step 3 since the input$grouping_col has already assigned the desired value.
      if (rv$using_example && !is.null(input$grouping_col)) {
        grouping_col_changed()
        treatment_val_changed()
        control_val_changed()
      }
    }
  })
}

step2ColSel <- function(input, output, session, rv) {
  
  if (rv$using_example) {
    data <- read_excel('./data/Tsivgoulis_2019.xls', as.numeric(rv$sheet))
  } else {
    # importing data from file
    if (rv$filetype == 'csv') {
      data <- as.data.frame(fread(file = input$excel_file$datapath))
    } else {
      data <- read_excel(input$excel_file$datapath, as.numeric(rv$sheet))
    }
  }
  

  # keeping the excel data in a global accessible var
  rv$data <- data
  
  excel_cols <- names(data)
  
  rv$all_excel_cols <- excel_cols
  
  removeUI( selector = "#excel_cols .form-group, #tab2_gotab1, #tab2_gotab3", multiple = TRUE)
  
  insertUI(
    selector = "#step2nav",
    where = "beforeEnd",
    ui = actionButton(inputId = "tab2_gotab1", class = "gotab1", label = "Back")
  )
  
  if (length(excel_cols) > 0) {
    insertUI(
      selector = "#excel_cols",
      where = "beforeEnd",
      ui = pickerInput(
        "selected_cols","Select which excel columns you want to include in TableOne export and click next", 
        choices = excel_cols, 
        options = list(`actions-box` = TRUE),
        multiple = T,
        selected = rv$selected_cols
      )
    )
    
    insertUI(
      selector = "#step2nav",
      where = "beforeEnd",
      ui = actionButton(inputId = "tab2_gotab3", class = "gotab3", label = "Next")
    )
  } else {
    insertUI(
      selector = "#step2nav",
      where = "afterBegin",
      ui = h6(class = "error-message", "The sheet you specified does not contain any columns. Please go back and select another one and make sure that it has columns.")
    )
  }
}