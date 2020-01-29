step8EventObservers <- function(input, output, rv, session) {
  # Back button
  observeEvent(input$tab8_gotab7, {
    removeUI(selector = ".error-message", multiple = TRUE)
    updateTabsetPanel(session, "NavBar", selected = "panel7")
  })
  
  # Reset Button
  observeEvent(input$tab8_reset, {
    shinyjs::runjs(code = 'location.reload();')
  })
  
  # Graphs button
  observeEvent(input$tab8graphs, {
    updateTabsetPanel(session, "NavBar", selected = "panel9")
    step9Graphs(input, output, session, rv)
  })
}

step8OutputWord <- function(input, output, session, rv) {
  
  removeUI( selector = ".tab8_reset, #tab8_gotab7", multiple = TRUE)
  
  insertUI(
    selector = "#step8nav",
    where = "beforeEnd",
    ui = actionButton(inputId = "tab8_gotab7", class = "gotab7", label = "Back")
  )
  
  insertUI(
    selector = "#step8nav",
    where = "beforeEnd",
    ui = actionButton(inputId = "tab8_reset", class = "tab8_reset", label = "Reset")
  )

  # Creating a random filename for the generated file.
  a <- do.call(paste0, replicate(10, sample(LETTERS, 1, TRUE), FALSE))
  random_filename <- paste0(a, sprintf("%04d", sample(9999, 1, TRUE)), sample(LETTERS, 1, TRUE))
  
  tryCatch(
    {
      dataframetable <- tableone(
        import.col.names = rv$var_list_no_group,
        output.var.names = rv$var_names,
        dichotomous = input$dichotomous_cols,
        ordinal = input$ordinal_cols,
        median.iqr = input$medianiqr_cols,
        group.col.name = input$grouping_col,
        control.value = input$control_val,
        treatment.value = input$treatment_val,
        data = rv$data,
        # excel.path = input$excel_file$datapath,
        export.path = tempdir(),
        sheet = as.numeric(rv$sheet),
        # tableone.col.names = 
        export.filename = random_filename
        # show.stats = 
      )
      
      # create table
      insertUI(
        selector = "#table-display",
        where = "beforeEnd",
        ui = DT::dataTableOutput("dataframetable")
      )
      output$dataframetable <- DT::renderDataTable(dataframetable$table, server = FALSE)
      
      if (dataframetable$continuity_correction) {
        insertUI(
          selector = "#continuity-correction",
          where = "beforeEnd",
          ui = p(paste('Note: Continuity correction was applied for the variables: ', paste(dataframetable$continuity_correction_vars, collapse = ', ')))
        )
      }
      
      rv$tabledata <- dataframetable$tabledata
    },
    error = function(e) {
      stop(safeError(e))
    }
  )
  
  # download table one word button
  output$export_word <- downloadHandler(
    filename = function() { "TableOneWord.docx" },
    content = function(file) { file.copy(paste0(tempdir(), '/', random_filename, '.docx'), file)}
  )
  
  # download table one csv button
  output$export_csv <- downloadHandler(
    filename = function() { "TableOneData.csv" },
    content = function(file) { write.csv(dataframetable$table, file) }
  )
  
  # download table one xls button
  output$export_xls <- downloadHandler(
    filename = function() { "TableOneData.xlsx" },
    content = function(file) { write_xlsx(as.data.frame(dataframetable$table), file, col_names = T, format_headers = T) }
  )
  
  # download analysis data csv button
  output$export_data_csv <- downloadHandler(
    filename = function() { "TableOneAnalysisData.csv" },
    content = function(file) { write.csv(dataframetable$tabledata, file) }
  )
  
  # download analysis_data xls button
  output$export_data_xls <- downloadHandler(
    filename = function() { "TableOneAnalysisData.xlsx" },
    content = function(file) { write_xlsx(as.data.frame(dataframetable$tabledata), file, col_names = T, format_headers = T) }
  )
}