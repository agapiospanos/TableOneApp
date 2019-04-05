step2ColSel <- function(input, output, session) {
  
  # chosen_sheet <- reactive({
  #   validate(
  #     need(is.numeric(input$sheet), "Please input a number")
  #   )
  # })
  
  # importing data from the excel file
  data <- read_excel(input$excel_file$datapath, as.numeric(input$sheet), n_max = 1)
  
  excel_cols <- names(data)
  
  rv$all_excel_cols <- excel_cols
  
  removeUI( selector = "#selected_cols, .gotab1, .gotab3", multiple = TRUE)
  
  insertUI(
    selector = "#step2nav",
    where = "beforeEnd",
    ui = actionButton(inputId = "tab2_gotab1", class = "gotab1", label = "Back")
  )
  
  if (length(excel_cols) > 0){
    insertUI(
      selector = "#excel_cols",
      where = "beforeEnd",
      ui = checkboxGroupInput("selected_cols", "Select which excel columns you want to include in TableOne export and click next", excel_cols)
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
  
  # for (i in 1:length(excel_cols)) {
  #   insertUI(
  #     selector = "#excel-cols",
  #     where = "beforeEnd",
  #     ui = div(class = "select-vars", id = paste0("select-var-", excel_cols[i]))
  #   )
  #   insertUI(
  #     selector = paste0("#select-var", i),
  #     where = "beforeEnd",
  #     ui = checkboxInput(paste0("select-v", i), excel_cols[i])
  #   )
  # }
      
}