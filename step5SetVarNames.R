step5SetVarNames <- function(input, output, session) {
  removeUI(selector = ".gotab6, .gotab4")
  var_list <- input$selected_cols
  var_list_no_group <- var_list[!var_list %in% input$grouping_col]

  removeUI(selector = "#var_names_container, .gotab4, .gotab6", multiple = TRUE)
  
  insertUI(
    selector = "#var_names",
    where = "beforeEnd",
    ui = div(id = "var_names_container",  "")
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
  
  for (i in 1:length(var_list_no_group)) {
    insertUI(
      selector = "#var_names_container",
      where = "beforeEnd",
      ui = textInput(var_list_no_group[i], paste("Name for", var_list_no_group[i]), value = var_list_no_group[i])
    )
  }
}