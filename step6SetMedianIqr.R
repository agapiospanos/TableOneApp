step6SetMedianIqr <- function(input, output, session, rv) {
  removeUI(selector = "#medianiqr .form-group, #tab6_gotab5, #tab6_gotab7", multiple = TRUE)
  
  var_list_cont <- rv$var_list_no_group[!rv$var_list_no_group %in% c(input$dichotomous_cols, input$ordinal_cols)]
  rv$var_list_continuous <- var_list_cont
  rv$var_list_ordinal <- input$ordinal_cols
  
  insertUI(
    selector = "#medianiqr",
    where = "beforeEnd",
    ui = pickerInput("medianiqr_cols","Select the excel columns for which you would like to have the median and IQR calculated instead of mean and SD", choices = var_list_cont, options = list(`actions-box` = TRUE), multiple = T)
  ) 
  
  insertUI(
    selector = "#step6nav",
    where = "beforeEnd",
    ui = actionButton(inputId = "tab6_gotab5", class = "gotab5", label = "Back")
  )
  
  insertUI(
    selector = "#step6nav",
    where = "beforeEnd",
    ui = actionButton(inputId = "tab6_gotab7", class = "gotab7", label = "Next")
  )
}