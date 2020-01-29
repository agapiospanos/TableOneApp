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
  
  # Setting maximum upload file size to 30MB.
  options(shiny.maxRequestSize = 30*1024^2)
  
  # Loading all the functions that exist in other files
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
  
  # Keeping reactiveValues so that we can easily keep values in a global application store.
  rv <- reactiveValues(
    all_excel_cols = c(),
    var_list_no_group = c(),
    var_list_dichotomous = c(),
    var_list_ordinal = c(),
    var_list_continuous = c(),
    var_names = c(),
    data_levels = c(),
    data = data.frame(),
    filetype = '',
    # The variables below are required for the example run
    using_example = F,
    sheet = NA,
    selected_cols = character(0),
    selected_grouping_col = character(0),
    selected_group_var = character(0),
    selected_treatment_var = character(0),
    dichotomous_vars = character(0),
    ordinal_vars = character(0)
  )

  step1EventObservers(input, output, rv, session)
  step2EventObservers(input, output, rv, session)
  step3EventObservers(input, output, rv, session)
  step4EventObservers(input, output, rv, session)
  step5EventObservers(input, output, rv, session)
  step6EventObservers(input, output, rv, session)
  step7EventObservers(input, output, rv, session)
  step8EventObservers(input, output, rv, session)
  step9EventObservers(input, output, rv, session)
}