library(shiny)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  theme = "custom.css",
  
  img(id = "logo", src = 'tableone-logo.png', align = "center"),
  
  tags$hr(),
  
  navbarPage("", id = "NavBar",
             tabPanel("", value = "panel1",
                      fluidRow(
                        column(
                          12,
                          h4("Instructions"),
                          p('In the excel file that you will use, please make sure that all the columns have names and there are not empty column names in the 1st row.'),
                          p('If you experience any difficulties do not hesitate to contact us via the contact form in our website!'),
                          a('https://esm.uoi.gr/en/contact-page/'),
                          br(),
                          br(),
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
                          div(id = "treatment_var"),
                          div(id = "control_var"),
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
                          h4("Specify the ordinal variables"),
                          div(id = "ordinal",  ""),
                          div(id = "step5nav", class = "bottom-nav")
                        )
                      )
             ),
             tabPanel("", value = "panel6", 
                      fluidRow(
                        column(
                          12,
                          h4("Specify excel columns for Median and IQR"),
                          div(id = "medianiqr",  ""),
                          div(id = "step6nav", class = "bottom-nav")
                        )
                      )
             ),
             tabPanel("", value = "panel7",
                      fluidRow(
                        column(
                          12,
                          h4("Change the display names of the variables for the exported table"),
                          div(id = "var_names",  ""),
                          div(id = "step7nav", class = "bottom-nav")
                        )
                      )
             ),
             tabPanel("", value = "panel8",
                      fluidRow(
                        column(
                          12,
                          h4("This is the generated tableOne"),
                          div(id = "table-display", ""),
                          div(id = "continuity-correction", ""),
                          br(),
                          br(),
                          h4("Download the generated Word Document"),
                          downloadButton("export_word", "Download TableOne Word Doc"),
                          div(id = "step8nav", class = "bottom-nav")
                        )
                      )
             )
  ),
  includeHTML('www/loading.html'),
  includeHTML('www/footer.html')
)
