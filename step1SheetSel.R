step1SheetSel <- function(input, output, session) {
  
  avail_sheets <- excel_sheets(input$excel_file$datapath)

  insertUI(
    selector = "#sheet_select",
    where = "afterBegin",
    ui = radioButtons("sheet", label = "Select a sheet to read data", choiceNames = avail_sheets, choiceValues = c(1:length(avail_sheets)))
  )

}