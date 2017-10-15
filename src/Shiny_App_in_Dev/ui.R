#Shiny app for the med data
#by Chris Morano

#ui.R

dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Predicting Patient Outcomes",
                  titleWidth=310),
  dashboardSidebar(    
    sidebarMenu(
      menuItem("Medical Data Input", tabName = "input", icon = icon("medkit")),
      menuItem("Model 1: 0 vs ABC", tabName = "model1", icon = icon("bar-chart")),
      menuItem("Model 2: 0A vs BC", tabName = "model2", icon = icon("bar-chart")),
      menuItem("Model 3: A vs B vs C", tabName = "model3", icon = icon("bar-chart")),
      menuItem("Model Diagnostics", tabName = "info", icon = icon("stethoscope"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      tabItem(tabName = "input",
              fluidRow(
                bsAlert("intro")
              ),
              fluidRow(
                h3("Data Input:")
              ),
              fluidRow(box(title = strong("Data Input:"),
                           width = 10,
                           column(1,
                                  numericInput("Day_I",
                                               h4("Day I"),
                                               value = NA)),
                           column(1,
                                  numericInput("Day_II",
                                               h4("Day II"),
                                               value = NA)),
                           column(1,
                                  numericInput("Day_III",
                                               h4("Day III"),
                                               value = NA)),
                           column(1,
                                  numericInput("Day_IV",
                                               h4("Day IV"),
                                               value = NA)),
                           column(1,
                                  numericInput("Day_V",
                                               h4("Day V"),
                                               value = NA)),
                           column(1,
                                  numericInput("Day_VI",
                                               h4("Day VI"),
                                               value = NA)),
                           column(1,
                                  numericInput("Day_VII",
                                               h4("Day VII"),
                                               value = NA))
              )),
              fluidRow(box(title = strong("Statistical Output:"),
                           width = 10,
                           tableOutput('stat_table')
              ))),
      tabItem(tabName = "model1",
              fluidRow(
                bsAlert("model1")
              ),
              fluidRow(
                box(plotOutput("model1_plot", height = 350), title = 'Model Output:'),
                box(plotOutput("lime_plot1", height = 350), title = 'Model Explanation:')
              )),
      tabItem(tabName = "model2",
              fluidRow(
                bsAlert("model2")
              ),
              #              fluidRow(
              #                h3(textOutput("text1"))
              #              ),
              fluidRow(
                box(plotOutput("model2_plot", height = 350), title = 'Model Output:'),
                box(plotOutput("lime_plot2", height = 350), title = 'Model Explanation:')
              )),
      tabItem(tabName = "model3",
              fluidRow(
                bsAlert("model3")
              ),
              fluidRow(
                box(h3(textOutput("text1")), width = 12)
              ),
              fluidRow(
                box(plotOutput("model3_plot", height = 350), title = 'Model Output:'),
                box(plotOutput("lime_plot3", height = 350), title = 'Model Explanation:')
              )),
      tabItem(tabName = "info",
              fluidRow(
                bsAlert("info_model1")
              ),
              fluidRow(box(title = strong("Model 1 Confusion Matrices:"),
                           width = 4,
                           column(4,
                                  br(),
                                  strong("Training Data"),
                                  br(), br(),
                                  tableOutput('train_conf1'),
                                  br(), br(),
                                  strong("Test Data"),
                                  br(), br(),
                                  tableOutput('test_conf1'),
                                  br()
                           )),
                       box(title = strong("ROC Curve:"),
                           width = 6,
                           column(10,
                                  plotOutput('roc1')))),
              fluidRow(box(title = strong("Other Metrics:"),
                           width = 10,
                           tableOutput('model1_metric')
              )),
              fluidRow(
                bsAlert("info_model2")
              ),
              fluidRow(box(title = strong("Model 2 Confusion Matrices:"),
                           width = 4,
                           column(4,
                                  br(),
                                  strong("Training Data"),
                                  br(), br(),
                                  tableOutput('train_conf2'),
                                  br(), br(),
                                  strong("Test Data"),
                                  br(), br(),
                                  tableOutput('test_conf2'),
                                  br()
                           )),
                       box(title = strong("ROC Curve:"),
                           width = 6,
                           column(10,
                                  plotOutput('roc2')))),
              fluidRow(box(title = strong("Other Metrics:"),
                           width = 10,
                           tableOutput('model2_metric')
              )),
              fluidRow(
                bsAlert("info_model3")
              ),
              fluidRow(box(title = strong("Model 3 Confusion Matrices:"),
                           width = 5,
                           column(4,
                                  br(),
                                  strong("Training Data"),
                                  br(), br(),
                                  tableOutput('train_conf3'),
                                  br(), br(),
                                  strong("Test Data"),
                                  br(), br(),
                                  tableOutput('test_conf3'),
                                  br()
                           )),
                       box(title = strong("ROC Curve:"),
                           width = 6,
                           column(10,
                                  plotOutput('roc3')))),
              fluidRow(box(title = strong("Other Metrics:"),
                           width = 10,
                           tableOutput('model3_metric')
              )))
      
    )
  )
)
