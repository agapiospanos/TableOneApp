library(shiny)
library(plotly)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  theme = "custom.css",
  
  img(id = "logo", src = 'tableone-logo.png', align = "center"),
  
  tags$hr(),
  
  navbarPage("", id = "NavBar",
             
             # ------------------------------------------- STEP 1 --------------------------------------------------
             
             tabPanel("", value = "panel1",
                      fluidRow(
                        column(
                          12,
                          h4("Instructions"),
                          p('In the file that you will use, please make sure that all the columns have names and there are not empty column names in the 1st row.'),
                          p('If you experience any difficulties do not hesitate to contact us via the contact form in our website!'),
                          a('https://esm.uoi.gr/en/contact-page/'),
                          br(),
                          br(),
                          h4("Choose an excel file"),
                          fileInput(
                            "excel_file",
                            "Please choose the excel file that contains the data (.csv, .xlsx, .xls)",
                            accept = c(".xlsx", ".xls", ".csv")
                          ),
                          div(id = "sheet_select"),
                          div(id = "gostep2", class = "bottom-nav"),
                          br(),
                          h4("Or use the example data"),
                          div(class = "example-buttons",
                            actionButton(inputId = "use_example", class = "use_example", label = "Use Example"),
                            downloadButton("download_example", class = "download_example", label = "Download Example Data"),
                          ),
                          p('Example data reference:'),
                          a('Tsivgoulis G, Kargiotis O, Katsanos AH, et al. Incidence, characteristics and outcomes in patients with embolic stroke of undeterminedsource: a population-based study. J Neurol Sci 2019;401:5-11.', href='https://www.sciencedirect.com/science/article/abs/pii/S0022510X19301650')
                          
                        )
                      )
             ),
             
             # ------------------------------------------- STEP 2 --------------------------------------------------
             
             tabPanel("", value = "panel2",
                      fluidRow(
                        column(
                          12,
                          div(id = "excel_cols"),
                          div(id = "step2nav", class = "bottom-nav")
                        )
                      )
             ),
             
             # ------------------------------------------- STEP 3 --------------------------------------------------
             
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
             
             # ------------------------------------------- STEP 4 --------------------------------------------------
             
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
             
             # ------------------------------------------- STEP 5 --------------------------------------------------
             
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
             
             # ------------------------------------------- STEP 6 --------------------------------------------------
             
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
             
             # ------------------------------------------- STEP 7 --------------------------------------------------
             
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
             
             # ------------------------------------------- STEP 8 --------------------------------------------------
             
             tabPanel("", value = "panel8",
                      fluidRow(
                        column(
                          12,
                          h4("This is the generated tableOne"),
                          div(id = "table-display", ""),
                          div(id = "continuity-correction", ""),
                          actionButton(inputId = "tab8graphs", class = "tab8gographs", label = "Create graphs from data"),
                          br(),
                          br(),
                          h4("Download TableOne"),
                          downloadButton("export_word", "Word file (.docx)"),
                          downloadButton("export_xls", "Excel file (.xlsx)"),
                          downloadButton("export_csv", "CSV file (.csv)"),
                          br(),
                          br(),
                          h4("Download Analysis Data"),
                          downloadButton("export_data_xls", "Excel file (.xlsx)"),
                          downloadButton("export_data_csv", "CSV file (.csv)"),
                          div(id = "step8nav", class = "bottom-nav")
                        )
                      )
             ),
             
             # ------------------------------------------- STEP 9 --------------------------------------------------
             
             tabPanel("", value = "panel9",
                      fluidRow(
                        column(
                          12,
                          h4("Select variable to create a graph"),
                          div(id = "graph_selector", ""),
                          div(id = "graphvar1_selector", ""),
                          div(id = "graphvar2_selector", ""),
                          plotlyOutput("plot1"),
                          plotlyOutput("plot2"),
                          br(),
                          br(),
                          div(id = "step9nav", class = "bottom-nav")
                        )
                      )
             )
  ),
  includeHTML('www/loading.html'),
  includeHTML('www/footer.html')
)
