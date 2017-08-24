#Shiny app for the med data
#Updated with better model: 0 vs ABC:
#by Chris Morano

#server.R

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  input_data <- reactive({
    data <- tibble(day = c("I", "II", "III", "IV", "V", "VI", "VII"), 
                  result = c(input$Day_I, input$Day_II, input$Day_III, input$Day_IV,
                             input$Day_V, input$Day_VI, input$Day_VII))
    data <-  data %>% 
      filter(!is.na(result))
    
    if (nrow(data) < 2) {
      output_data <- tibble(num_of_results=NA,
                            min_result=NA,
                            mean_result=NA,
                            max_result=NA,
                            var_result=NA,
                            diff_of_last_and_max=NA,
                            diff_of_last_and_min=NA)

    } else {
    #order the factors:
      data$day <- as.ordered(data$day)
    
      output_data <- data %>% 
        summarize(num_of_results=n(),
                  min_result=min(result),
                  mean_result=mean(result),
                  max_result=max(result),
                  var_result=var(result),
                  diff_of_last_and_max=ifelse(num_of_results==1, NA, max_result-result[num_of_results]),
                  diff_of_last_and_min=ifelse(num_of_results==1, NA, result[num_of_results]-min_result))
    }
    
    return(output_data)

  }) 
  
  post_model_data <- reactive({
    
    if (is.na(input_data()$num_of_results)){
      model_data <- tibble(class = c('0', 'ABC'), prob = c(0, 0))
    } else {
      model_data <- as.tibble(predict(model_0vsABC.rf, input_data(), type='prob'))
      model_data <- gather(model_data, class, prob)
      
      model_data$class <- as.factor(model_data$class)
      model_data$prob <- as.numeric(model_data$prob)
    }
    
    return(model_data)
    
  })
  output$histPlot <- renderPlot({
    ggplot(post_model_data(), aes(x=class, y=prob, fill=class)) +
      geom_col() +
      scale_fill_manual(values=c('lightblue3', 'red3'))
    
  })
  
  output$text1 <- renderText({
    if (is.na(input_data()$num_of_results)){
      "Please input the results from each day."
    } else {
      paste0("The probability of '0' is: ", as.character(100*post_model_data()[1,]$prob), "%.")
    }
  })

  output$text2 <- renderText({
    if (is.na(input_data()$num_of_results)){
      "Probabilities will be given when 2 or more test results are given."
    } else {
      paste0("The probability of 'A, B, or C' is: ", as.character(100*post_model_data()[2,]$prob), "%.")
    }
      
  })

  # output$text3 <- renderText({
  #   if (is.na(input_data()$num_of_results)){
  #     ""
  #   } else {
  #     paste0("The probability of 'B' is: ", as.character(100*post_model_data()[3,]$prob), "%.")
  #   }
  #   
  # })
  # 
  # output$text4 <- renderText({
  #   if (is.na(input_data()$num_of_results)){
  #     ""
  #   } else {
  #     paste0("The probability of 'C' is: ", as.character(100*post_model_data()[4,]$prob), "%.")
  #   }
  #   
  # })
  
  })
