step3GroupSpecification <- function(input, output, session) {
  
  removeUI( selector = "#grouping_list .form-group, #tab3_gotab2, #tab3_gotab4, #treatment_var .form-group, #control_var .form-group", multiple = TRUE)
  
  insertUI(
    selector = "#step3nav",
    where = "beforeEnd",
    ui = actionButton(inputId = "tab3_gotab2", class = "gotab2", label = "Back")
  )
  
  insertUI(
    selector = "#grouping_list",
    where = "beforeEnd",
    ui = radioGroupButtons(inputId = "grouping_col", label = "Select the excel column that contains the groupping information for the treatment and control group", choices = input$selected_cols, direction = "vertical", selected = character(0))
  ) 
}