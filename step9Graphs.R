step9EventObservers <- function(input, output, rv, session) {
  # Back button
  observeEvent(input$tab9_gotab8, {
    removeUI(selector = ".error-message", multiple = TRUE)
    updateTabsetPanel(session, "NavBar", selected = "panel8")
  })
  
  # Select graph dropdown listener
  observeEvent(input$selected_graph, {
    removeUI( selector = "#graphvar1_selector .form-group, #graphvar2_selector .form-group, #plot1 .plot-container.plotly, #plot2 .plot-container.plotly", multiple = TRUE)
    
    # Will show next options based the type of graph the user selected
  	selectedGraphHandler(input, rv)
  })
  
  # Variable X listener
  observeEvent(input$graph_varx, {
    if (input$graph_varx != '' & input$selected_graph == 'Scatterplot') {
      removeUI( selector = "#graphvar2_selector .form-group, #plot1 .plot-container.plotly, #plot2 .plot-container.plotly", multiple = TRUE)
      insertUI(
        selector = '#graphvar2_selector',
        where = "beforeEnd",
        ui = pickerInput("graph_vary","Select the y-axis variable", options = list(title = "Select the y-axis variable..."), selected = -1, choices = rv$var_list_continuous[!rv$var_list_continuous %in% input$graph_varx], multiple = F)
      )
    }
  })
  
  # Variable Y listener
  observeEvent(input$graph_vary, {
    if (input$graph_vary != '' & input$graph_varx != '') {
      if (input$selected_graph == 'Scatterplot') {
        generateScatterplot(input, output, rv, session)
      }
    }
  })
  
  observeEvent(input$selected_graphvar, {
    if (input$selected_graphvar != '') {
      switch(input$selected_graph,
        'Barplot' = {generateBarplot(input, output, rv, session)},
        'Stacked Barplot' = {generateStackedBarplot(input, output, rv, session)},
        'Clustered Barplot' = {generateClusteredBarplot(input, output, rv, session)},
        'Boxplot' = {generateBoxplot(input, output, rv, session)},
        'Pie Chart' = {generatePieChart(input, output, rv, session)},
        'Histogram' = {generateHistogram(input, output, rv, session)}
      )
    }
  })
}

selectedGraphHandler <- function(input, rv) {
  if (input$selected_graph == 'Barplot') {
    insertUI(
      selector = "#graphvar1_selector",
      where = "beforeEnd",
      ui = pickerInput("selected_graphvar","Select variable to create graph", options = list(title = "Select variable for the graph..."), selected = -1, choices = rv$var_list_dichotomous, multiple = F)
    )
  } else if (input$selected_graph == 'Scatterplot') {
    insertUI(
      selector = "#graphvar1_selector",
      where = "beforeEnd",
      ui = pickerInput("graph_varx","Select the x-axis variable", options = list(title = "Select the x-axis variable..."), selected = -1, choices = rv$var_list_continuous, multiple = F)
    )
  } else if (input$selected_graph == 'Stacked Barplot') {
    removeUI( selector = "#plot1 .plot-container.plotly, #plot2 .plot-container.plotly", multiple = T)
    insertUI(
      selector = "#graphvar1_selector",
      where = "beforeEnd",
      ui = pickerInput("selected_graphvar","Select an ordinal variable", options = list(title = "Select an ordinal variable..."), selected = -1, choices = rv$var_list_ordinal, multiple = F)
    )
  } else if (input$selected_graph == 'Boxplot') {
    removeUI( selector = '#plot1 .plot-container.plotly, #plot2 .plot-container.plotly', multiple = T)
    insertUI(
      selector = '#graphvar1_selector',
      where = 'beforeEnd',
      ui = pickerInput('selected_graphvar', 'Select a continuous variable', options = list(title = 'Select a continuous variable...'), selected = -1, choices = rv$var_list_continuous, multiple = F)
    )
  } else if (input$selected_graph == 'Clustered Barplot') {
    removeUI( selector = '#plot1 .plot-container.plotly, #plot2 .plot-container.plotly', multiple = T)
    insertUI(
      selector = '#graphvar1_selector',
      where = 'beforeEnd',
      ui = pickerInput('selected_graphvar', 'Select dichotomous variables', options = list(`actions-box` = TRUE), selected = -1, choices = rv$var_list_dichotomous, multiple = T)
    )
  } else if (input$selected_graph == 'Pie Chart') {
    removeUI( selector = '#plot1 .plot-container.plotly, #plot2 .plot-container.plotly', multiple = T)
    insertUI(
      selector = '#graphvar1_selector',
      where = 'beforeEnd',
      ui = pickerInput('selected_graphvar', 'Select a dichotomous or ordinal variable', options = list(title = 'Select a dichotomous or ordinal variable...'), selected = -1, choices = c(rv$var_list_dichotomous, rv$var_list_ordinal), multiple = F)
    )
  } else if (input$selected_graph == 'Histogram') {
    removeUI( selector = '#plot1 .plot-container.plotly, #plot2 .plot-container.plotly', multiple = T)
    insertUI(
      selector = '#graphvar1_selector',
      where = 'beforeEnd',
      ui = pickerInput('selected_graphvar', 'Select a continuous variable', options = list(title = 'Select a continuous variable...'), selected = -1, choices = rv$var_list_continuous, multiple = F)
    )
  }
}

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

# ----------------------- BARPLOT --------------------------------

generateBarplot <- function(input, output, rv, session) {
  d <- rv$tabledata
  graphvar <- input$selected_graphvar
  
  # we search in the var_list_no_group as the grouping variable is excluded from the variable naming step.
  graphvar_ind <- which(rv$var_list_no_group == graphvar)
  # then we find the matching display name from the var_names list
  display_name_Var <- rv$var_names[graphvar_ind]
  
  row.no <- which(d$name.var == graphvar)
  graph.data <- data.frame(group = c('treatment', 'control'), value = as.numeric(c(d[row.no,]$t.percent, d[row.no,]$c.percent)), count = as.numeric(c(d[row.no,]$t.count, d[row.no,]$c.count)))
  
  output$plot1 <- renderPlotly({
    graph.data %>% 
      plot_ly() %>% 
      add_trace(x = ~group, y = ~value, type = 'bar', name = ' ', text = paste0(graph.data$value, '% (', graph.data$count, ') '), textposition = 'auto', hovertemplate = paste0('%{x}: %{text}')) %>%
      layout(yaxis = list(title = display_name_Var, range = c(0, 100), tick0 = 0, dtick = 10), xaxis = list(title = 'Group'), barmode = 'group')
  })
}

# ---------------------- STACKED BARPLOT ---------------------------

generateStackedBarplot <- function(input, output, rv, session) {
  d <- rv$tabledata
  graphvar <- input$selected_graphvar
  
  # we search in the var_list_no_group as the grouping variable is excluded from the variable naming step.
  graphvar_ind <- which(rv$var_list_no_group == graphvar)
  # then we find the matching display name from the var_names list
  display_name_Var <- rv$var_names[graphvar_ind]
  
  row.no <- which(d$name.var == graphvar)
  var.levels <- as.numeric(d$ordinal.levels[row.no])
  
  treat_percent <- as.numeric(d$t.percent[(row.no + 1):(row.no + var.levels)])
  contr_percent <- as.numeric(d$c.percent[(row.no + 1):(row.no + var.levels)])
  
  treat_count <- as.numeric(d$t.count[(row.no + 1):(row.no + var.levels)])
  contr_count <- as.numeric(d$c.count[(row.no + 1):(row.no + var.levels)])
  
  lev.names <- substr(d$name.display[(row.no + 1):(row.no + var.levels)], 13, 1000)
  
  graph.data <- data.frame(levels = c(rep(lev.names, 2)), group = c(rep('treatment', var.levels), rep('control', var.levels)), value = as.numeric(c(treat_percent, contr_percent)), count = as.numeric(c(treat_count, contr_count)))
  
  output$plot1 <- renderPlotly({
    graph.data %>%
      plot_ly() %>% 
      add_trace(x = ~group, y = ~value, type = 'bar', color = ~levels, text = paste0(graph.data$value, '% (', graph.data$count, ') '), textposition = 'auto', colors = viridis(var.levels), textfont = list(color = 'white'), hovertemplate = paste0('%{x}: %{text}')) %>%
      layout(yaxis = list(title = display_name_Var, range = c(0, 100), tick0 = 0, dtick = 10), xaxis = list(title = 'Group'), barmode = 'stack')
  })
}

# -------------------------- CLUSTERED BARPLOT ---------------------------

generateClusteredBarplot <- function(input, output, rv, session) {
  d <- rv$tabledata
  graphvar <- input$selected_graphvar
  
  display_name_vars <- c()
  graph.data.percent <- data.frame(group = c('treatment', 'control'))
  graph.data.count <- data.frame(group = c('treatment', 'control'))
  
  for (this_graphvar in graphvar) {
    # we search in the var_list_no_group as the grouping variable is excluded from the variable naming step.
    graphvar_ind <- which(rv$var_list_no_group == this_graphvar)
    # then we find the matching display name from the var_names list
    display_name_vars <- cbind(display_name_vars, rv$var_names[graphvar_ind])
    
    row.no <- which(d$name.var == this_graphvar)
    row.data.percent <- as.numeric(c(d[row.no,]$t.percent, d[row.no,]$c.percent))
    row.data.count <- as.numeric(c(d[row.no,]$t.count, d[row.no,]$c.count))
    
    graph.data.percent <- cbind(graph.data.percent, row.data.percent)
    graph.data.count <- cbind(graph.data.count, row.data.count)
  }
  
  names(graph.data.percent) <- c('group', graphvar)
  names(graph.data.count) <- c('group', graphvar)
  
  p <- graph.data.percent %>% 
    plot_ly(x = ~group, y = unlist(graph.data.percent[graphvar[1]]), type = 'bar', text = paste0(unlist(graph.data.percent[graphvar[1]]), '% (', unlist(graph.data.count[graphvar[1]]), ')'), textposition = 'auto', name = display_name_vars[1]) %>%
    layout(yaxis = list(title = '%', range = c(0, 100), tick0 = 0, dtick = 10), xaxis = list(title = 'Group'))
  
  if (length(graphvar) > 1) {
    for (i in 2:length(graphvar)) {
      p <- add_trace(p, x = ~group, y = unlist(graph.data.percent[graphvar[i]]), type = 'bar', text = paste0(unlist(graph.data.percent[graphvar[i]]), '% (', unlist(graph.data.count[graphvar[i]]), ')'), textposition = 'auto', name = display_name_vars[i])
    }
  }
  
  output$plot1 <- renderPlotly({p})
}

# --------------------------- BOXPLOT ------------------------------------

generateBoxplot <- function(input, output, rv, session) {
  graphvar <- input$selected_graphvar
  
  graphvar.col <- which(names(rv$data) == graphvar)
  
  group <- unlist(rv$data[input$grouping_col])
  control <- unlist(rv$data[which(group == input$control_val), graphvar.col])
  treatment <- unlist(rv$data[which(group == input$treatment_val), graphvar.col])
  
  # we search in the var_list_no_group as the grouping variable is excluded from the variable naming step.
  graphvar_ind <- which(rv$var_list_no_group == graphvar)
  # then we find the matching display name from the var_names list
  display_name_Var <- rv$var_names[graphvar_ind]
  
  output$plot1 <- renderPlotly({
    plot_ly() %>%
      add_boxplot(y = treatment, jitter = 0.3, boxpoints = 'outliers', type = 'box', name = 'Treatment') %>%
      add_boxplot(y = control, jitter = 0.3, boxpoints = 'outliers', type = 'box', name = 'Control') %>%
      layout(yaxis = list(title = display_name_Var), xaxis = list(title = 'Group'), barmode = 'stack')
  })
}

# -------------------------------- SCATTERPLOT ----------------------------------

generateScatterplot <- function(input, output, rv, session) {

  # we get plot data from rv$data instead of rv$tabledata because in this case we need all the data read from the excel and not the calculated ones from the table one. 
  d <- rv$data
  varx <- unlist(d[input$graph_varx])
  vary <- unlist(d[input$graph_vary])
  group <- unlist(d[input$grouping_col])
  group.factors <- as.factor(group)
  
  # matching the group names with the levels that we found
  group.names <- c()
  group.names[which(group == input$treatment_val)] <- 'Treatment Group'
  group.names[which(group == input$control_val)]   <- 'Control Group'
  
  # we search in the var_list_no_group as the grouping variable is excluded from the variable naming step.
  graphvar_indx <- which(rv$var_list_no_group == input$graph_varx)
  graphvar_indy <- which(rv$var_list_no_group == input$graph_vary)
  
  # then we find the matching display name from the var_names list
  display_name_Varx <- rv$var_names[graphvar_indx]
  display_name_vary <- rv$var_names[graphvar_indy]
  
  output$plot1 <- renderPlotly({
    rv$data %>%
      plot_ly() %>%
      add_trace(x = varx, y = vary, color = group.factors, type = 'scatter', name = group.names) %>%
      layout(yaxis = list(title = display_name_vary), xaxis = list(title = display_name_Varx))
  })
}

# -------------------------------- PIE CHART ----------------------------------

generatePieChart <- function(input, output, rv, session) {
  d <- rv$tabledata
  graphvar <- input$selected_graphvar
  
  row.no <- which(d$name.var == graphvar)
  
  # Dichotomous var case
  if (d$ordinal.levels[row.no] == '') {
    
    var.levels <- levels(as.factor(unlist(rv$data[graphvar])))
    
    t.percent <- as.numeric(d$t.percent[row.no])
    t.count   <- as.numeric(d$t.count[row.no])
    c.percent <- as.numeric(d$c.percent[row.no])
    c.count   <- as.numeric(d$c.count[row.no])
    
    t.total.count <- length(rv$data[graphvar] == input$treatment_val)
    c.total.count <- length(rv$data[graphvar] == input$control_val)
    
    # we use the second factor of the variable in our calculations so we have to use 100-t.data first
    graph.data.t <- data.frame(levels = var.levels, value = c(100 - t.percent, t.percent), count = c(t.total.count - t.count, t.count))
    graph.data.c <- data.frame(levels = var.levels, value = c(100 - c.percent, c.percent), count = c(c.total.count - c.count, c.count))
    
  # Ordinal var case
  } else {

    var.levels <- as.numeric(d$ordinal.levels[row.no])
    
    t.percent <- as.numeric(d$t.percent[(row.no + 1):(row.no + var.levels)])
    t.count   <- as.numeric(d$t.count[(row.no + 1):(row.no + var.levels)])
    c.percent <- as.numeric(d$c.percent[(row.no + 1):(row.no + var.levels)])
    c.count   <- as.numeric(d$c.count[(row.no + 1):(row.no + var.levels)])
    
    # we are getting the substring from the tabledata var.names. We display ---- level: 1 so we need to get the substring from the character 13 and to the right.
    lev.names <- substr(d$name.display[(row.no + 1):(row.no + var.levels)], 13, 1000)
    
    graph.data.t <- data.frame(levels = lev.names, value = as.numeric(t.percent), count = as.numeric(t.count))
    graph.data.c <- data.frame(levels = lev.names, value = as.numeric(c.percent), count = as.numeric(c.count))
    
  }
  
  # we search in the var_list_no_group as the grouping variable is excluded from the variable naming step.
  graphvar_ind <- which(rv$var_list_no_group == graphvar)
  # then we find the matching display name from the var_names list
  display_name_Var <- rv$var_names[graphvar_ind]
  
  p.t <- graph.data.t %>%
            plot_ly() %>% 
            add_trace(name = ' ', labels = paste0('Level: ', graph.data.t$levels, ' (', graph.data.t$value, '%)'), values = ~value, type = 'pie', text = paste0('Level: ', graph.data.t$levels, ' (', graph.data.t$count, ') '), hovertemplate = paste0('%{text}')) %>%
            layout(title = paste(display_name_Var, '(Treatment Group)'))
  
  p.c <- graph.data.c %>%
            plot_ly() %>% 
            add_trace(name = ' ', labels = paste0('Level: ', graph.data.c$levels, ' (', graph.data.c$value, '%)'), values = ~value, type = 'pie', text = paste0('Level: ', graph.data.c$levels, ' (', graph.data.c$count, ') '), hovertemplate = paste0('%{text}')) %>%
            layout(title = paste(display_name_Var, '(Control Group)'))
  
  output$plot1 <- renderPlotly({p.t})
  
  output$plot2 <- renderPlotly({p.c})
}

# ---------------------------- HISTOGRAM ---------------------------------

generateHistogram <- function(input, output, rv, session) {
  graphvar <- input$selected_graphvar
  
  dat <- unlist(rv$data[graphvar])
  group <- unlist(rv$data[input$grouping_col])
  
  dat.t <- dat[which(group == input$treatment_val)]
  dat.c <- dat[which(group == input$control_val)]
  
  # we search in the var_list_no_group as the grouping variable is excluded from the variable naming step.
  graphvar_ind <- which(rv$var_list_no_group == graphvar)
  # then we find the matching display name from the var_names list
  display_name_Var <- rv$var_names[graphvar_ind]
  
  output$plot1 <- renderPlotly({
    plot_ly(alpha = 0.7) %>%
      add_histogram(x = dat.t, name = 'Treatment Group', hovertemplate = paste0(display_name_Var, ': %{x} <br>Frequency: %{y}')) %>%
      add_histogram(x = dat.c, name = 'Control Group', hovertemplate = paste0(display_name_Var, ': %{x} <br>Frequency: %{y}')) %>%
      layout(barmode = "overlay", xaxis = list(title = display_name_Var), yaxis = list(title = 'Frequency'))
  })
}