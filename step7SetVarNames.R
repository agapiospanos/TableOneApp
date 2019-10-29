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
    
    thisvar <- rv$var_list_no_group[i]
    
    if (thisvar %in% rv$var_list_dichotomous) {
      
      val <- levels(as.factor(unlist(rv$data[thisvar])))
      insertUI(
        selector = "#var_names_container",
        where = "beforeEnd",
        ui = textInput(thisvar, paste("Name for", thisvar, '( for value: ', val[2], ')'), value = paste0(thisvar, '-', val[2]))
      )
      
    } else {
      
      insertUI(
        selector = "#var_names_container",
        where = "beforeEnd",
        ui = textInput(thisvar, paste("Name for", thisvar), value = thisvar)
      )
    }
  }
}