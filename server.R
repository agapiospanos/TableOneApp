library(shiny)
library(readxl)
library(writexl)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(shinyjs)
library(officer)
library(flextable)
library(easycsv)
library(ggplot2)
library(plotly)
library(viridis)
library(tools)

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 30*1024^2)
  
  source('step1SheetSel.R', local = T)
  source('step2ColSel.R', local = T)
  source('step3GroupSpecification.R', local = T)
  source('step4DichotomousSel.R', local = T)
  source('step5OrdinalSel.R', local = T)
  source('step6SetMedianIqr.R', local = T)
  source('step7SetVarNames.R', local = T)
  source('step8OutputWord.R', local = T)
  source('step9Graphs.R', local = T)
  source('tableone.R', local = T)
  
  rv <- reactiveValues(
    all_excel_cols = c(),
    var_list_no_group = c(),
    var_list_dichotomous = c(),
    var_list_ordinal = c(),
    var_list_continuous = c(),
    var_names = c(),
    data_levels = c(),
    data = data.frame(),
    filetype = ''
  )

  # ---------------------------------------- STEP 1 --------------------------------------------------------
  
  observeEvent(input$excel_file, {
    if (!is.na(input$excel_file$datapath)) {
      filetype <- file_ext(input$excel_file$name)
      rv$filetype <- filetype
      if (filetype == 'xls' || filetype == 'xlsx') {
        step1SheetSel(input, output, session)
      } else if (filetype == 'csv') {
        removeUI(selector = "#tab1_gotab2")
        insertUI(
          selector = "#gostep2",
          where = "beforeEnd",
          ui = actionButton(inputId = "tab1_gotab2", class = "gotab2", label = "Next")
        )
      } else {
        insertUI(
          selector = "#step1nav",
          where = "afterBegin",
          ui = h6(class = "error-message", "The file you have chosen is not compatible. Acceptable file types are .xls, .xlsx, .csv.")
        )
      }
    }
  })
  
  observeEvent(input$sheet, {
    removeUI(selector = "#tab1_gotab2")
    insertUI(
      selector = "#gostep2",
      where = "beforeEnd",
      ui = actionButton(inputId = "tab1_gotab2", class = "gotab2", label = "Next")
    )
  })
  
  observeEvent(input$tab1_gotab2, {
    step2ColSel(input, output, session, rv)
    updateTabsetPanel(session, "NavBar", selected = "panel2")
  })
  
  # ---------------------------------------- STEP 2 -------------------------------------------------------
  
  observeEvent(input$tab2_gotab1, {
    updateTabsetPanel(session, "NavBar", selected = "panel1")
    removeUI(selector = "#tab1_gotab2")
    insertUI(
      selector = "#gostep2",
      where = "beforeEnd",
      ui = actionButton(inputId = "tab1_gotab2", class = "gotab2", label = "Next")
    )
  })
  
  observeEvent(input$tab2_gotab3, {
    if (is.null(input$selected_cols)) {
      removeUI(selector = ".error-message", multiple = TRUE)
      insertUI(
        selector = "#step2nav",
        where = "beforeBegin",
        ui = h6(class = "error-message", "Please choose at least one column to continue!")
      )
    } else {
      step3GroupSpecification(input, output, session)
      updateTabsetPanel(session, "NavBar", selected = "panel3")
    }
  })
  
  # ---------------------------------------- STEP 3 --------------------------------------------------------
  
  observeEvent(input$grouping_col, {
    
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
      ui = radioGroupButtons(inputId = "treatment_val", label = "Select treatment group value", choices = data_levels, selected = character(0))
    )
    
  })
  
  observeEvent(input$treatment_val, {
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
  })
  
  observeEvent(input$control_val, {
    removeUI(selector = "#tab3_gotab4", multiple = TRUE)
    insertUI(
      selector = "#step3nav",
      where = "beforeEnd",
      ui = actionButton(inputId = "tab3_gotab4", class = "gotab4", label = "Next")
    )
  })
  
  observeEvent(input$tab3_gotab2, {
    removeUI(selector = ".error-message", multiple = TRUE)
    updateTabsetPanel(session, "NavBar", selected = "panel2")
  })
  
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
  
  # ---------------------------------------- STEP 4 --------------------------------------------------------
  
  observeEvent(input$tab4_gotab5, {
    step5OrdinalSel(input, output, session, rv)
    updateTabsetPanel(session, "NavBar", selected = "panel5")
  })
  
  observeEvent(input$tab4_gotab3, {
    removeUI(selector = ".error-message", multiple = TRUE)
    updateTabsetPanel(session, "NavBar", selected = "panel3")
  })
  
  # ---------------------------------------- STEP 5 --------------------------------------------------------
  
  observeEvent(input$tab5_gotab6, {
    step6SetMedianIqr(input, output, session, rv)
    updateTabsetPanel(session, "NavBar", selected = "panel6")
  })
  
  observeEvent(input$tab5_gotab4, {
    removeUI(selector = ".error-message", multiple = TRUE)
    updateTabsetPanel(session, "NavBar", selected = "panel4")
  })
  
  # ---------------------------------------- STEP 6 --------------------------------------------------------
  
  observeEvent(input$tab6_gotab7, {
    step7SetVarNames(input, output, session, rv)
    updateTabsetPanel(session, "NavBar", selected = "panel7")
  })
  
  observeEvent(input$tab6_gotab5, {
    removeUI(selector = ".error-message", multiple = TRUE)
    updateTabsetPanel(session, "NavBar", selected = "panel5")
  })
  
  # ---------------------------------------- STEP 7 --------------------------------------------------------
  
  observeEvent(input$tab7_gotab8, {
    
    removeUI(selector = ".error-message", multiple = TRUE)
    all_fields_filled <- TRUE
    var_list <- rv$var_list_no_group
    var_names <- c()
    
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
      
      shinyjs::runjs(code = '$(".container-fluid").addClass("wide");')
    } else {
      insertUI(
        selector = "#step7nav",
        where = "beforeBegin",
        ui = h6(class = "error-message", "Please fill all the above fields!")
      )
    }
    
  })
  
  observeEvent(input$tab7_gotab6, {
    removeUI(selector = ".error-message", multiple = TRUE)
    updateTabsetPanel(session, "NavBar", selected = "panel6")
  })
  
  # ---------------------------------------- STEP 8 --------------------------------------------------------
  
  observeEvent(input$tab8_gotab7, {
    removeUI(selector = ".error-message", multiple = TRUE)
    updateTabsetPanel(session, "NavBar", selected = "panel7")
  })
  
  observeEvent(input$tab8_reset, {
    shinyjs::runjs(code = 'location.reload();')
  })
  
  observeEvent(input$tab8graphs, {
    updateTabsetPanel(session, "NavBar", selected = "panel9")
    step9Graphs(input, output, session, rv)
  })
  
  # ---------------------------------------- STEP 9 --------------------------------------------------------
  
  observeEvent(input$tab9_gotab8, {
    removeUI(selector = ".error-message", multiple = TRUE)
    updateTabsetPanel(session, "NavBar", selected = "panel8")
  })
  
  observeEvent(input$selected_graph, {
    removeUI( selector = "#graphvar1_selector .form-group, #graphvar2_selector .form-group, #plot1 .plot-container.plotly, #plot2 .plot-container.plotly", multiple = TRUE)
    
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
  })
  
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
  
  observeEvent(input$graph_vary, {
    if (input$graph_vary != '' & input$graph_varx != '') {
      if (input$selected_graph == 'Scatterplot') {
        output$plot1 <- renderPlotly({
          plot_ly(data = rv$data, x = unlist(rv$data[input$graph_varx]), y = unlist(rv$data[input$graph_vary]), color = as.factor(unlist(rv$data[input$grouping_col])), type = 'scatter') %>% layout(yaxis = list(title = input$graph_vary), xaxis = list(title = input$graph_varx))
        })
      }
    }
  })
  
  observeEvent(input$selected_graphvar, {
    if (input$selected_graphvar != '') {
      if (input$selected_graph == 'Barplot') {
        
        d <- rv$tabledata
        graphvar <- input$selected_graphvar
        
        row.no <- which(d$name.var == graphvar)
        graph.data <- data.frame(group = c('treatment', 'control'), value = as.numeric(c(d[row.no,]$t.percent, d[row.no,]$c.percent)))
        
        output$plot1 <- renderPlotly({
          graph.data %>% 
            plot_ly() %>% 
            add_trace(x = ~group, y = ~value, type = 'bar', text = ~value, textposition = 'auto') %>%
            layout(yaxis = list(title = graphvar, range = c(0, 100), tick0 = 0, dtick = 10), xaxis = list(title = 'Group'), barmode = 'group')
        })
      } else if (input$selected_graph == 'Stacked Barplot') {
        
        d <- rv$tabledata
        graphvar <- input$selected_graphvar
        
        row.no <- which(d$name.var == graphvar)
        var.levels <- as.numeric(d$ordinal.levels[row.no])
        
        treat <- as.numeric(d$t.percent[(row.no + 1):(row.no + var.levels)])
        contr <- as.numeric(d$c.percent[(row.no + 1):(row.no + var.levels)])
        lev.names <- substr(d$name.display[(row.no + 1):(row.no + var.levels)], 13, 1000)
        
        graph.data <- data.frame(levels = c(rep(lev.names, 2)), group = c(rep('treatment', var.levels), rep('control', var.levels)), value = as.numeric(c(treat, contr)))
        
        output$plot1 <- renderPlotly({
          graph.data %>%
            plot_ly() %>% 
            add_trace(x = ~group, y = ~value, type = 'bar', color = ~levels, text = ~value, textposition = 'auto', colors = viridis(var.levels), textfont = list(color = 'white')) %>%
            layout(yaxis = list(title = graphvar, range = c(0, 100), tick0 = 0, dtick = 10), xaxis = list(title = 'Group'), barmode = 'stack')
        })
        
      } else if (input$selected_graph == 'Boxplot') {

        graphvar <- input$selected_graphvar
        
        graphvar.col <- which(names(rv$data) == graphvar)
        
        group <- unlist(rv$data[input$grouping_col])
        control <- unlist(rv$data[which(group == input$control_val), graphvar.col])
        treatment <- unlist(rv$data[which(group == input$treatment_val), graphvar.col])
        
        output$plot1 <- renderPlotly({
          plot_ly() %>%
            add_boxplot(y = treatment, jitter = 0.3, boxpoints = 'outliers', type = 'box', name = 'Treatment') %>%
            add_boxplot(y = control, jitter = 0.3, boxpoints = 'outliers', type = 'box', name = 'Control') %>%
            layout(yaxis = list(title = graphvar), xaxis = list(title = 'Group'), barmode = 'stack')
        })
        
      } else if (input$selected_graph == 'Clustered Barplot') {

        d <- rv$tabledata
        graphvar <- input$selected_graphvar
        
        graph.data <- data.frame(group = c('treatment', 'control'))
        
        for (i in 1:length(graphvar)) {
          row.no <- which(d$name.var == graphvar[i])
          row.data <- as.numeric(c(d[row.no,]$t.percent, d[row.no,]$c.percent))
          
          graph.data <- cbind(graph.data, row.data)
        }
        
        names(graph.data) <- c('group', graphvar)
        
        p <- graph.data %>% 
          plot_ly(x = ~group, y = unlist(graph.data[graphvar[1]]), type = 'bar', name = graphvar[1]) %>%
          layout(yaxis = list(title = '%', range = c(0, 100), tick0 = 0, dtick = 10), xaxis = list(title = 'Group'))
        
        if (length(graphvar) > 1) {
          for (i in 2:length(graphvar)) {
            p <- add_trace(p, x = ~group, y = unlist(graph.data[graphvar[i]]), type = 'bar', text = graph.data[graphvar[i]], textposition = 'auto', name = graphvar[i])
          }
        }
        
        output$plot1 <- renderPlotly({p})
        
      } else if (input$selected_graph == 'Pie Chart') {

        d <- rv$tabledata
        graphvar <- input$selected_graphvar
        
        row.no <- which(d$name.var == graphvar)
        
        if (d$ordinal.levels[row.no] == ''){
          # we have dichotomous var
          
          lev <- levels(as.factor(unlist(rv$data[graphvar])))
          
          t.data <- as.numeric(d$t.percent[row.no])
          c.data <- as.numeric(d$c.percent[row.no])
          
          # we use the second factor of the variable in our calculations so we have to use 100-t.data first
          graph.data.t <- data.frame(levels = lev, value = c(100 - t.data, t.data))
          graph.data.c <- data.frame(levels = lev, value = c(100 - c.data, c.data))
          
          p.t <- plot_ly(graph.data.t, labels = ~levels, values = ~value, type = 'pie') %>%
            layout(title = paste('Variable:', graphvar, '(treatment group)'))
          
          p.c <- plot_ly(graph.data.c, labels = ~levels, values = ~value, type = 'pie') %>%
            layout(title = paste('Variable:', graphvar, '(control group)'))
          
        } else {
          # we have ordinal var
          var.levels <- as.numeric(d$ordinal.levels[row.no])
          
          treat <- as.numeric(d$t.percent[(row.no + 1):(row.no + var.levels)])
          contr <- as.numeric(d$c.percent[(row.no + 1):(row.no + var.levels)])
          lev.names <- substr(d$name.display[(row.no + 1):(row.no + var.levels)], 13, 1000)
          
          graph.data.t <- data.frame(levels = lev.names, value = as.numeric(treat))
          graph.data.c <- data.frame(levels = lev.names, value = as.numeric(contr))
          
          p.t <- plot_ly(graph.data.t, labels = ~levels, values = ~value, type = 'pie') %>%
            layout(title = paste('Variable:', graphvar, '(treatment group)'))
          
          p.c <- plot_ly(graph.data.c, labels = ~levels, values = ~value, type = 'pie') %>%
            layout(title = paste('Variable:', graphvar, '(control group)'))
          
        }
        
        output$plot1 <- renderPlotly({p.t})
        
        output$plot2 <- renderPlotly({p.c})
        
      } else if (input$selected_graph == 'Histogram') {
        
        graphvar <- input$selected_graphvar
        
        dat <- unlist(rv$data[graphvar])
        group <- unlist(rv$data[input$grouping_col])
        
        dat.t <- dat[which(group == input$treatment_val)]
        dat.c <- dat[which(group == input$control_val)]
      
        output$plot1 <- renderPlotly({
          plot_ly(alpha = 0.7) %>%
            add_histogram(x = dat.t, name = 'Treatment Group') %>%
            add_histogram(x = dat.c, name = 'Control Group') %>%
            layout(barmode = "overlay", xaxis = list(title = graphvar), yaxis = list(title = 'Frequency'))
        })
        
      }
    }
    
  })
}