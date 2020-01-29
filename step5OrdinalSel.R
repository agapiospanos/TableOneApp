step5EventObservers <- function(input, output, rv, session) {
  # Next button
  observeEvent(input$tab5_gotab6, {
    step6SetMedianIqr(input, output, session, rv)
    updateTabsetPanel(session, "NavBar", selected = "panel6")
  })
  
  # Back button
  observeEvent(input$tab5_gotab4, {
    removeUI(selector = ".error-message", multiple = TRUE)
    updateTabsetPanel(session, "NavBar", selected = "panel4")
  })
}

step5OrdinalSel <- function(input, output, session, rv) {
  removeUI(selector = "#ordinal .form-group, #tab5_gotab4, #tab5_gotab6", multiple = TRUE)
  
  var_list_no_dich <- rv$var_list_no_group[!rv$var_list_no_group %in% input$dichotomous_cols]
  rv$var_list_dichotomous <- rv$var_list_no_group[rv$var_list_no_group %in% input$dichotomous_cols]
  rv$var_list_no_dich <- var_list_no_dich
  
  insertUI(
    selector = "#ordinal",
    where = "beforeEnd",
    ui = pickerInput(
      "ordinal_cols","Select the excel columns that contains ordinal values", 
      choices = var_list_no_dich, 
      options = list(`actions-box` = TRUE), 
      multiple = T,
      selected = rv$ordinal_vars
    )
  ) 
  
  insertUI(
    selector = "#step5nav",
    where = "beforeEnd",
    ui = actionButton(inputId = "tab5_gotab4", class = "gotab4", label = "Back")
  )
  
  insertUI(
    selector = "#step5nav",
    where = "beforeEnd",
    ui = actionButton(inputId = "tab5_gotab6", class = "gotab6", label = "Next")
  )
}