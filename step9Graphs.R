step9Graphs <- function(input, output, session, rv) {
  
  removeUI( selector = "#tab9_gotab8, #graph_selector .form-group, #graphvar1_selector .form-group, #graphvar2_selector .form-group, #plot1 .plot-container.plotly, #plot2 .plot-container.plotly", multiple = TRUE)
  
  insertUI(
    selector = "#step9nav",
    where = "beforeEnd",
    ui = actionButton(inputId = "tab9_gotab8", class = "gotab8", label = "Back to table")
  )
  
  insertUI(
    selector = "#graph_selector",
    where = "beforeEnd",
    ui = pickerInput("selected_graph","Select a graph type", selected = -1, options = list(title = "Select a graph type..."), choices = c('Barplot', 'Stacked Barplot', 'Clustered Barplot', 'Boxplot', 'Scatterplot', 'Pie Chart', 'Histogram'), multiple = F)
  )

}