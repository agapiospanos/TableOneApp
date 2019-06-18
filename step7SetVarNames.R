step7SetVarNames <- function(input, output, session, rv) {

  removeUI(selector = "#var_names_container, #tab7_gotab6, #tab7_gotab8", multiple = TRUE)
  
  insertUI(
    selector = "#var_names",
    where = "beforeEnd",
    ui = div(id = "var_names_container",  "")
  )
  
  insertUI(
    selector = "#step7nav",
    where = "beforeEnd",
    ui = actionButton(inputId = "tab7_gotab6", class = "gotab6", label = "Back")
  )
  
  insertUI(
    selector = "#step7nav",
    where = "beforeEnd",
    ui = actionButton(inputId = "tab7_gotab8", class = "gotab8", label = "Next")
  )
  
  for (i in 1:length(rv$var_list_no_group)) {
    insertUI(
      selector = "#var_names_container",
      where = "beforeEnd",
      ui = textInput(rv$var_list_no_group[i], paste("Name for", rv$var_list_no_group[i]), value = rv$var_list_no_group[i])
    )
  }
}