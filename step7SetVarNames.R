step7EventObservers <- function(input, output, rv, session) {
  # Next button
  observeEvent(input$tab7_gotab8, {
    
    removeUI(selector = ".error-message", multiple = TRUE)
    all_fields_filled <- TRUE
    var_list <- rv$var_list_no_group
    var_names <- c()
    
    # Checking for empty fields in the var names list
    for (i in 1:length(var_list)) {
      field <- input[[var_list[i]]]
      var_names <- c(var_names, field)  
      if (is.null(field) | field == "") {
        all_fields_filled <- FALSE
      }
    }
    rv$var_names <- var_names
    
    if (all_fields_filled) {
      step8OutputWord(input, output, session, rv)
      updateTabsetPanel(session, "NavBar", selected = "panel8")
      
      # We are making the container wider to have the table displayed.
      shinyjs::runjs(code = '$(".container-fluid").addClass("wide");')
    } else {
      insertUI(
        selector = "#step7nav",
        where = "beforeBegin",
        ui = h6(class = "error-message", "Please fill all the above fields!")
      )
    }
    
  })
  
  # Back button
  observeEvent(input$tab7_gotab6, {
    removeUI(selector = ".error-message", multiple = TRUE)
    updateTabsetPanel(session, "NavBar", selected = "panel6")
  })
}

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
  
  # Variable names for example
  # We keep the variable names and the desired display names so that we can match the variables to display variable names in case the user has changed the selected variables.
  example_var_names <- c('Death', 'MRS1', 'Age', 'SBP', 'DBP', 'NIHSS1', 'female_sex', 'smoking')
  example_display_vars <- c('Death', 'Modified Rankin Scale', 'Age', 'Systolic Blood Pressure', 'Diastolic Blood Pressure', 'National Institute of Health Stroke Scale', 'Sex (Female)', 'Smoking')
  
  for (i in 1:length(rv$var_list_no_group)) {
    
    thisvar <- rv$var_list_no_group[i]
    
    # keeping the index of the variable in example_var_names so that we can match the name in example_display_vars
    var_ind <- which(example_var_names == thisvar)
    example_display_name <- example_display_vars[var_ind]
    
    if (thisvar %in% rv$var_list_dichotomous) {
      val <- levels(as.factor(unlist(rv$data[thisvar])))
      
      # if the user loaded the example dataset predefine the variable names
      if (rv$using_example) {
        var_value <- example_display_name
      } else {
        var_value <- paste0(thisvar, '-', val[2])
      }
      
      insertUI(
        selector = "#var_names_container",
        where = "beforeEnd",
        ui = textInput(thisvar, paste("Name for", thisvar, '( for value: ', val[2], ')'), value = var_value)
      )
      
    } else {
      
      # if the user loaded the example dataset predefine the variable names
      if (rv$using_example) {
        var_value <- example_display_name
      } else {
        var_value <- paste0(thisvar, '-', val[2])
      }
      
      insertUI(
        selector = "#var_names_container",
        where = "beforeEnd",
        ui = textInput(thisvar, paste("Name for", thisvar), value = var_value)
      )
    }
  }
}