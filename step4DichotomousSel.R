step4EventObservers <- function(input, output, rv, session) {
  # Next button observer
  observeEvent(input$tab4_gotab5, {
    step5OrdinalSel(input, output, session, rv)
    updateTabsetPanel(session, "NavBar", selected = "panel5")
  })
  
  # Back button observer
  observeEvent(input$tab4_gotab3, {
    removeUI(selector = ".error-message", multiple = TRUE)
    updateTabsetPanel(session, "NavBar", selected = "panel3")
  })
}

step4DichotomousSel <- function(input, output, session, rv) {
  
  removeUI(selector = "#dichotomous .form-group, #tab4_gotab3, #tab4_gotab5", multiple = TRUE)
  
  var_list <- input$selected_cols
  var_list_no_group <- var_list[!var_list %in% input$grouping_col]
  rv$var_list_no_group <- var_list_no_group
  
  insertUI(
    selector = "#dichotomous",
    where = "beforeEnd",
    ui = pickerInput(
      "dichotomous_cols","Select the excel columns that contains dichotomous values", 
      choices = var_list_no_group, 
      options = list(`actions-box` = TRUE), 
      multiple = T,
      selected = rv$dichotomous_vars
    )
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