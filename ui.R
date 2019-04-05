library(shiny)

ui <- fluidPage(
  
  theme = "custom.css",
  
  # Application title
  # titlePanel("TableOne App"),
  img(id="logo", src = 'tableone-logo.png', align = "center"),
  
  tags$hr(),
  
  navbarPage("", id = "NavBar",
             tabPanel("", value = "panel1",
                      fluidRow(
                        column(
                          12,
                          h4("Choose an excel file"),
                          fileInput(
                            "excel_file",
                            "Please choose the excel file that contains the data",
                            accept = c(".xlsx", ".xls")
                          ),
                          div(id = "sheet_select"),
                          div(id = "gostep2", class = "bottom-nav")
                        )
                      )
             ),
             tabPanel("", value = "panel2",
                      fluidRow(
                        column(
                          12,
                          div(id = "excel_cols"),
                          div(id = "step2nav", class = "bottom-nav")
                        )
                      )
             ),
             tabPanel("", value = "panel3",
                      fluidRow(
                        column(
                          12,
                          h4("Specify the grouping information"),
                          div(id = "grouping_list",  ""),
                          br(),
                          # textInput("group_var_name", "Fill in the column name as it listed in excel", placeholder = "e.g. treatment"),
                          div(id = "treatment_var"),
                          div(id = "control_var"),
                          # textInput("treatment_var", "Fill in the value that indicates the treatment group in the above specified column", placeholder = "e.g. 1"),
                          # textInput("control_var", "Fill in the value that indicates the control group in the above specified column", placeholder = "e.g. 0"),
                          div(id = "step3nav", class = "bottom-nav")
                        )
                      )
             ),
             tabPanel("", value = "panel4", 
                      fluidRow(
                        column(
                          12,
                          h4("Specify the dichotomous variables"),
                          div(id = "dichotomous",  ""),
                          div(id = "step4nav", class = "bottom-nav")
                        )
                      )
             ),
             tabPanel("", value = "panel5",
                      fluidRow(
                        column(
                          12,
                          h4("Change the display names of the variables for the exported table"),
                          div(id = "var_names",  ""),
                          div(id = "step5nav", class = "bottom-nav")
                        )
                      )
             ),
             tabPanel("", value = "panel6",
                      fluidRow(
                        column(
                          12,
                          h4("Download the generated Word Document"),
                          br(),
                          downloadButton("export_word", "Download TableOne Word Doc"),
                          div(id = "step6nav", class = "bottom-nav")
                        )
                      )
             )
  )
)
