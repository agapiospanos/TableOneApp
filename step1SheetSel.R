step1EventObservers <- function(input, output, rv, session) {
  
  # File input listener
  observeEvent(input$excel_file, {
    # Initializing the values for the case that the user will not use example
    selected_cols = character(0)
    selected_grouping_col = character(0)
    selected_group_var = character(0)
    selected_treatment_var = character(0)
    dichotomous_vars = character(0)
    ordinal_vars = character(0)
    
    # If the user has uploaded a file and not clicked cancel
    if (!is.na(input$excel_file$datapath)) {
      filetype <- file_ext(input$excel_file$name)
      rv$filetype <- filetype
      
      # If we have an excel file we should prompt the user to choose which sheet contains the data.
      if (filetype == 'xls' || filetype == 'xlsx') {
        step1SheetSel(input, output, rv, session)
        
        # If we have a csv file we can go directly to step 2.
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
    rv$sheet <- input$sheet
    removeUI(selector = "#tab1_gotab2")
    insertUI(
      selector = "#gostep2",
      where = "beforeEnd",
      ui = actionButton(inputId = "tab1_gotab2", class = "gotab2", label = "Next")
    )
  })
  
  observeEvent(input$tab1_gotab2, {
    rv$using_example <- F
    step2ColSel(input, output, session, rv)
    updateTabsetPanel(session, "NavBar", selected = "panel2")
  })
  
  observeEvent(input$use_example, {
    rv$using_example <- T
    rv$filetype <- 'xls'
    rv$sheet <- 1
    rv$selected_cols <- c('ESUS', 'Death', 'MRS1', 'Age', 'DBP', 'SBP', 'NIHSS1', 'female_sex', 'smoking')
    rv$selected_grouping_col <- 'ESUS'
    rv$selected_treatment_var <- 1
    rv$dichotomous_vars <- c('Death', 'female_sex', 'smoking')
    rv$ordinal_vars <- c('MRS1')
    step2ColSel(input, output, session, rv)
    updateTabsetPanel(session, "NavBar", selected = "panel2")
  })
  
  output$download_example <- downloadHandler(
    filename = function() { "Tsivgoulis_2019.xls" },
    content = function(file) { file.copy("./data/Tsivgoulis_2019.xls", file) } 
  )
}


step1SheetSel <- function(input, output, rv, session) {
  
  avail_sheets <- excel_sheets(input$excel_file$datapath)
  removeUI("#sheet_select > div", multiple = TRUE)

  insertUI(
    selector = "#sheet_select",
    where = "afterBegin",
    ui = radioGroupButtons(inputId = "sheet", label = "Select a sheet to read data", choiceNames = avail_sheets, direction = "vertical", choiceValues = c(1:length(avail_sheets)))
  )

}