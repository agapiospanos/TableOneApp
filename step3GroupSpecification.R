step3GroupSpecification <- function(input, output, session) {
  
  removeUI( selector = "#grouping_col, .gotab2, .gotab4", multiple = TRUE)
  
  insertUI(
    selector = "#step3nav",
    where = "beforeEnd",
    ui = actionButton(inputId = "tab3_gotab2", class = "gotab2", label = "Back")
  )
  
  insertUI(
    selector = "#grouping_list",
    where = "beforeEnd",
    # ui = checkboxGroupInput("grouping_col", "Select the excel column that contains the groupping information for the treatment and control group", input$selected_cols)
    ui = radioButtons("grouping_col", label = "Select the excel column that contains the groupping information for the treatment and control group", choices = input$selected_cols, selected = character(0))
  ) 
  
  
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