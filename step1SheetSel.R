step1SheetSel <- function(input, output, session) {
  
  avail_sheets <- excel_sheets(input$excel_file$datapath)
  removeUI("#sheet_select > div", multiple = TRUE)

  insertUI(
    selector = "#sheet_select",
    where = "afterBegin",
    ui = radioGroupButtons(inputId = "sheet", label = "Select a sheet to read data", choiceNames = avail_sheets, direction = "vertical", choiceValues = c(1:length(avail_sheets)))
  )

}