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

  a <- do.call(paste0, replicate(10, sample(LETTERS, 1, TRUE), FALSE))
  random_filename <- paste0(a, sprintf("%04d", sample(9999, 1, TRUE)), sample(LETTERS, 1, TRUE))
  
  tryCatch(
    {
      dataframetable <- TableOne::tableone(
        import.col.names = rv$var_list_no_group,
        output.var.names = rv$var_names,
        dichotomous = input$dichotomous_cols,
        ordinal = input$ordinal_cols,
        median.iqr = input$medianiqr_cols,
        group.col.name = input$grouping_col,
        control.value = input$control_val,
        treatment.value = input$treatment_val,
        excel.path = input$excel_file$datapath,
        export.path = tempdir(),
        sheet = as.numeric(input$sheet),
        # tableone.col.names = 
        export.filename = random_filename
        # show.stats = 
      )
      
      insertUI(
        selector = "#table-display",
        where = "beforeEnd",
        ui = DT::dataTableOutput("dataframetable")
      )
      
      output$dataframetable <- DT::renderDataTable(dataframetable, server = FALSE)
    },
    error = function(e) {
      stop(safeError(e))
    }
  )
  
  output$export_word <- downloadHandler(
    filename = function() { "TableOneWord.docx" },
    content = function(file) { file.copy(paste0(tempdir(), '/', random_filename, '.docx'), file)}
  )
}