step3EventObservers <- function(input, output, rv, session) {
  
  # Grouping col selection listener
  observeEvent(input$grouping_col, {
    grouping_col_changed()
  })
  
  # we created a function so that we can easily trigger it. 
  # We need to manually trigger it in case the user goes back to step 2 and then to step 3 
  # because we have already selected the Grouping col ESUS in the example and the observeEvent 
  # cannot trigger since its value is not changed. We trigger this function in step2 file.
  grouping_col_changed <- function() {
    # the data of the grouping column
    grouping_data <- rv$data[input$grouping_col]
    
    # finding levels of grouping column data
    data_levels <- levels(as.factor(unlist(grouping_data)))
    
    # keeping the levels of grouping col in a global var
    rv$data_levels <- data_levels
    
    removeUI(selector = "#treatment_val, #control_val, #treatment_var .form-group, #control_var .form-group", multiple = TRUE)
    insertUI(
      selector = "#treatment_var",
      where = "afterBegin",
      ui = radioGroupButtons(inputId = "treatment_val", label = "Select treatment group value", choices = data_levels, selected = rv$selected_treatment_var)
    )
  }
  
  observeEvent(input$treatment_val, {
    treatment_val_changed()
  })
  
  treatment_val_changed <- function() {
    removeUI(selector = "#control_val, #control_var .form-group", multiple = TRUE)
    rest_data_levels <- rv$data_levels[-which(rv$data_levels == input$treatment_val)]
    
    if (length(rest_data_levels) > 1) {
      sel <- character(0)
    } else {
      sel <- rest_data_levels
    }
    
    insertUI(
      selector = "#control_var",
      where = "afterBegin",
      ui = radioGroupButtons(inputId = "control_val", label = "Select control group value", choices = rest_data_levels, selected = sel)
    )
  }
  
  observeEvent(input$control_val, {
    control_val_changed()
  })
  
  control_val_changed <- function() {
    removeUI(selector = "#tab3_gotab4", multiple = TRUE)
    insertUI(
      selector = "#step3nav",
      where = "beforeEnd",
      ui = actionButton(inputId = "tab3_gotab4", class = "gotab4", label = "Next")
    )
  }
  
  # Back button observer
  observeEvent(input$tab3_gotab2, {
    removeUI(selector = ".error-message", multiple = TRUE)
    updateTabsetPanel(session, "NavBar", selected = "panel2")
  })
  
  # Next button observer
  observeEvent(input$tab3_gotab4, {
    removeUI(selector = ".error-message", multiple = TRUE)
    if (length(input$grouping_col) > 0 & length(input$treatment_val) > 0 & length(input$control_val) > 0) {
      step4DichotomousSel(input, output, session, rv)
      updateTabsetPanel(session, "NavBar", selected = "panel4")
    } else {
      insertUI(
        selector = "#step3nav",
        where = "beforeBegin",
        ui = h6(class = "error-message", "Please make sure that you chose at least one option for all the above fields!")
      )
    }
  })
}


step3GroupSpecification <- function(input, output, session, rv) {
  
  removeUI( selector = "#grouping_list .form-group, #tab3_gotab2, #tab3_gotab4, #treatment_var .form-group, #control_var .form-group", multiple = TRUE)
  
  insertUI(
    selector = "#step3nav",
    where = "beforeEnd",
    ui = actionButton(inputId = "tab3_gotab2", class = "gotab2", label = "Back")
  )
  
  
  insertUI(
    selector = "#grouping_list",
    where = "beforeEnd",
    ui = radioGroupButtons(
      inputId = "grouping_col", 
      label = "Select the excel column that contains the groupping information for the treatment and control group", 
      choices = input$selected_cols, 
      direction = "vertical",
      selected = rv$selected_grouping_col
    )
  )
  
  # updateRadioGroupButtons(session, 'grouping_col', selected = rv$selected_grouping_col)
  if(rv$using_example) {
    rv$updated_grouping_col <- rv$updated_grouping_col + 1
  }
  
}