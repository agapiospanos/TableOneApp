step2ColSel <- function(input, output, session, rv) {
  
  # importing data from file
  if (rv$filetype == 'csv') {
    data <- as.data.frame(fread(file = input$excel_file$datapath))
  } else {
    data <- read_excel(input$excel_file$datapath, as.numeric(input$sheet))
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
      ui = pickerInput("selected_cols","Select which excel columns you want to include in TableOne export and click next", choices = excel_cols, options = list(`actions-box` = TRUE),multiple = T)
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