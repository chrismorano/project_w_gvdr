#Shiny app for the med data
#by Chris Morano

#ui.R

shinyUI(fluidPage(theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("Predicting Responses with Test Results:"),
  
  # Inputing the Seven days of Amylase Test Results:
  fluidRow(
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
  
    ),
  fluidRow(
    column(8,
           plotOutput("histPlot")),
    column(4,
           textOutput("text1"),
           textOutput("text2"))
    #       textOutput("text3"), 
    #       textOutput("text4"))
  )
  )
)
