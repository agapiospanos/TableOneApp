step4DichotomousSel <- function(input, output, session) {
  removeUI(selector = "#dichotomous_cols, .gotab5, .gotab3", multiple = TRUE)
  
  var_list <- input$selected_cols
  var_list_no_group <- var_list[!var_list %in% input$grouping_col]
  rv$var_list_no_group <- var_list_no_group
  
  insertUI(
    selector = "#dichotomous",
    where = "beforeEnd",
    ui = checkboxGroupInput("dichotomous_cols", "Select the excel columns that contains dichotomous values", var_list_no_group)
  ) 
  
  insertUI(
    selector = "#step4nav",
    where = "beforeEnd",
    ui = actionButton(inputId = "tab4_gotab3", class = "gotab3", label = "Back")
  )
  
  insertUI(
    selector = "#step4nav",
    where = "beforeEnd",
    ui = actionButton(inputId = "tab4_gotab5", class = "gotab5", label = "Next")
  )
}