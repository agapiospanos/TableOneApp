step6OutputWord <- function(input, output, session) {
  
  removeUI( selector = ".tab6_reset, .gotab5", multiple = TRUE)
  
  insertUI(
    selector = "#step6nav",
    where = "beforeEnd",
    ui = actionButton(inputId = "tab6_gotab5", class = "gotab5", label = "Back")
  )
  
  insertUI(
    selector = "#step6nav",
    where = "beforeEnd",
    ui = actionButton(inputId = "tab6_reset", class = "tab6_reset", label = "Reset")
  )

  a <- do.call(paste0, replicate(10, sample(LETTERS, 1, TRUE), FALSE))
  random_filename <- paste0(a, sprintf("%04d", sample(9999, 1, TRUE)), sample(LETTERS, 1, TRUE))
  
  tryCatch(
    {
      TableOne::tableone(
        export.path = tempdir(),
        export.filename = random_filename,
        excel.path = input$excel_file$datapath,
        dichotomous = input$dichotomous_cols,
        group.col.name = input$grouping_col,
        control.value = input$control_val,
        treatment.value = input$treatment_val,
        excel.col.names = rv$var_list_no_group,
        output.var.names = rv$var_names
      )
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