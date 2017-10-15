#Shiny app for the med data
#In-development version.
#Possibly not working...

#by Chris Morano

#server.R

shinyServer(function(input, output, session) {
  
  #creating the 'Alerts' which introduce each tab:
  createAlert(session, anchorId = "intro", title = "Medical Data Input:",
              content = paste0("Please input at least two test results below.  This will output a numbers of statistics, some of which are used to create the predictive models. <br/>",
                               "Click the tabs on the left to see the output from these models."
              ))
  
  createAlert(session, anchorId = "model1", title = "Predicting 0 vs. A, B, or C:",
              content = paste0("This model achieves about 95% accuracy in classifying patients as either '0' or 'not 0'. <br/>",
                               "The results below show the probabilities that the patient, whose info was input on the data input tab, is either 0 or one of A, B or C. <br/>",
                               "The plots below on the right use the `lime` package to explain the results obtained by the `randomForest` model."
              ))

  createAlert(session, anchorId = "model2", title = "Predicting 0 or A vs. B or C:",
              content = paste0("This model achieves about 74% accuracy in classifying patients in two groups: '0' or 'A' vs. 'B' or 'C'. <br/>",
                               "The results below show the probabilities that the patient, whose info was input on the data input tab, is either 0A or BC. <br/>",
                               "The plots below on the right use the `lime` package to explain the results obtained by the `randomForest` model."
              ))
  
  createAlert(session, anchorId = "model3", title = "Predicting A vs B vs C:",
              content = paste0("This model achieves roughly 59% accuracy in classifying patients into the groups 'A', 'B', or 'C'. <br/>",
                               "The results below show the probabilities that the patient, whose info was input on the data input tab, is in one of these groups. <br/>",
                               "The plots below on the right use the `lime` package to explain the results obtained by the `randomForest` model. <br/><br/>",
                               "Please keep in mind that the patient may not be predicted to be in groups A, B, or C and that this model only achieved 59% accuracy on test data."
                               ))

  createAlert(session, anchorId = "info_model1", title = "Model 1 Information:",
              content = paste0("Model: Random Forest classifying 0 vs. ABC. <br/>",
                               "Package: `randomForest` run through the `train` function in `caret`. <br/>"
              ))
  
  createAlert(session, anchorId = "info_model2", title = "Model 2 Information:",
              content = paste0("Model: Random Forest classifying 0A vs. BC. <br/>",
                               "Package: `randomForest` run through the `train` function in `caret`. <br/>"
              ))
  
  createAlert(session, anchorId = "info_model3", title = "Model 3 Information:",
              content = paste0("Model: Random Forest classifying A vs. B vs. C. <br/>",
                               "Package: `randomForest` run through the `train` function in `caret`. <br/>"
              ))
  
  
   output$text1 <- renderText({
     if (is.na(input_data()$num_of_results)){
       "Please input the results from each day."
     } else {
       paste0("The probability that the patient is in class A, B, or C is: ", as.character(100*post_model1_data()[2,]$prob), "%.")
     }
   })
  
                
  #getting the statistics from the input: 
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
      #abs_diff_of_last_two=NA)
    } else {
      #order the factors:
      data$day <- as.ordered(data$day)
      
      output_data <- data %>% 
        summarize(num_of_results=n(),
                  min_result=min(result),
                  mean_result=mean(result),
                  max_result=max(result),
                  var_result=stats::var(result),
                  diff_of_last_and_max=ifelse(num_of_results==1, NA, max_result-result[num_of_results]),
                  diff_of_last_and_min=ifelse(num_of_results==1, NA, result[num_of_results]-min_result)
        )
    }
    
    return(output_data)
    
  }) 
  
  #output of the stats of the input data:
  output$stat_table <- renderTable(input_data())
  
  #running the model 0 vs ABC and getting the probabilities:
  post_model1_data <- reactive({
    if (is.na(input_data()$num_of_results)){
      model_data <- tibble(class = c('0', 'ABC'), prob = c(0, 0))
    } else {
      model_data <- as.tibble(predict(data.0vsABC.rf, input_data(), type='prob'))
      model_data <- gather(model_data, class, prob)
      
      model_data$class <- as.factor(model_data$class)
      model_data$prob <- as.numeric(model_data$prob)
    }
    
    return(model_data)
    
  })

  #the plot for model 1:
  output$model1_plot <- renderPlot({
     ggplot(post_model1_data(), aes(x=class, y=prob, fill=class)) +
       geom_col() +
       scale_x_discrete(limits=c('O', 'ABC')) +
       scale_fill_manual(values=c('red3', 'green3')) +
       labs(title='Probability of the Patient Having the Result 0 or ABC:')
     
  })
  
  #getting the lime explanation:
  data.0vsABC.explanation <- reactive({
    lime::explain(select(input_data(), max_result, diff_of_last_and_max, diff_of_last_and_min), 
                  explainer.0vsABC, n_labels=2, n_features=3)
  })
  
  #plotting lime results for model 1:
  output$lime_plot1 <- renderPlot({
    windowsFonts(Times=windowsFont("TT Times New Roman"))
    plot_features(data.0vsABC.explanation())
  })
  
  #running the model 0A vs BC and getting the probabilities:
  post_model2_data <- reactive({
    if (is.na(input_data()$num_of_results)){
      model_data <- tibble(class = c('0', 'ABC'), prob = c(0, 0))
    } else {
      model_data <- as.tibble(predict(data.0AvsBC.rf, input_data(), type='prob'))
      model_data <- gather(model_data, class, prob)
      
      model_data$class <- as.factor(model_data$class)
      model_data$prob <- as.numeric(model_data$prob)
    }
    
    return(model_data)
    
  })
  
  #the plot for model 1:
  output$model2_plot <- renderPlot({
    ggplot(post_model2_data(), aes(x=class, y=prob, fill=class)) +
      geom_col() +
      scale_x_discrete(limits=c('OA', 'BC')) +
      scale_fill_manual(values=c('red3', 'green3')) + 
      labs(title='Probability of the Patient Having the Result 0A or BC:')
    
  })
  
  #getting the lime explanation:
  data.0AvsBC.explanation <- reactive({
    lime::explain(select(input_data(), max_result, diff_of_last_and_max, diff_of_last_and_min), 
                  explainer.0AvsBC, n_labels=2, n_features=3)
  })
  
  #plotting lime results for model 1:
  output$lime_plot2 <- renderPlot({
    windowsFonts(Times=windowsFont("TT Times New Roman"))
    plot_features(data.0AvsBC.explanation())
  })
  
  
  #running the model A vs B vs C and getting the probabilities:
  post_model3_data <- reactive({
    if (is.na(input_data()$num_of_results)){
      model_data <- tibble(class = c('A', 'B', 'C'), prob = c(0, 0, 0))
    } else {
      model_data <- as.tibble(predict(data.ABC.rf, input_data(), type='prob'))
      model_data <- gather(model_data, class, prob)
      
      model_data$class <- as.factor(model_data$class)
      model_data$prob <- as.numeric(model_data$prob)
    }
    
    return(model_data)
    
  })
  
  #the plot for model 2:
  output$model3_plot <- renderPlot({
    ggplot(post_model3_data(), aes(x=class, y=prob, fill=class)) +
      geom_col() +
      scale_fill_manual(values=c('blue3', 'yellow2', 'red3')) +
      labs(title='Probability of the Patient Having the Result A, B, or C:')
    
  })
  
  #getting the lime explanation for model 2:
  data.ABC.explanation <- reactive({
    lime::explain(select(input_data(), max_result, diff_of_last_and_max, diff_of_last_and_min), 
                  explainer.ABC, n_labels=3, n_features=3)
  })
  
  #plotting lime results for model 2:
  output$lime_plot3 <- renderPlot({
    windowsFonts(Times=windowsFont("TT Times New Roman"))
    plot_features(data.ABC.explanation())
  })
  
  #Diagnostics:
  
  #For Model 1:
  
  #output of the training data confusion table:
  output$train_conf1 <- renderTable(
    as.tibble(data.0vsABC.rf$finalModel$confusion) %>% 
      rename(pred_0 = O, pred_ABC = ABC) %>% 
      mutate(Training_Data = c("true_0", "true_ABC"),
             pred_0 = as.integer(pred_0),
             pred_ABC = as.integer(pred_ABC)) %>% 
      select(Training_Data, everything()),
    bordered = TRUE)
  
  #output of the test data confusion table:
  output$test_conf1 <- renderTable(
    as.tibble(confusionMatrix(predict(data.0vsABC.rf, data.0vsABC.test), data.0vsABC.test$Result_2016)$table) %>% 
      spread(Prediction, n) %>% 
      rename(pred_0 = O, pred_ABC = ABC) %>% 
      mutate(Test_Data = c("true_ABC", "true_0")) %>% 
      select(Test_Data, pred_0, pred_ABC) %>% 
      dplyr::arrange(Test_Data), 
    bordered = TRUE)
  
  output$roc1 <- renderPlot({
    plot(data.0vsABC.mroc)
  })
  
  output$model1_metric <- renderTable(
    tibble(AUC = data.0vsABC.auc[1],
           Accuracy = data.0vsABC.coords[[1]],
           Sensitivity = data.0vsABC.coords[[2]],
           Specificity = data.0vsABC.coords[[3]],
           `Pos. Pred. Value` = data.0vsABC.coords[[4]],
           `Neg. Pred. Value` = data.0vsABC.coords[[5]]),
    digits = 4)

  #For Model 2:
  
  #output of the training data confusion table:
  output$train_conf2 <- renderTable(
    as.tibble(data.0AvsBC.rf$finalModel$confusion) %>% 
      rename(pred_0A = OA, pred_BC = BC) %>% 
      mutate(Training_Data = c("true_BC", "true_0A"),
             pred_0A = as.integer(pred_0A),
             pred_BC = as.integer(pred_BC)) %>% 
      select(Training_Data, pred_0A, everything()) %>% 
      dplyr::arrange(Training_Data),
    bordered = TRUE)

    #output of the test data confusion table:
  output$test_conf2 <- renderTable(
    as.tibble(confusionMatrix(predict(data.0AvsBC.rf, data.0AvsBC.test), data.0AvsBC.test$Result_2016)$table) %>% 
      spread(Prediction, n) %>% 
      rename(pred_0A = OA, pred_BC = BC) %>% 
      mutate(Test_Data = c("true_BC", "true_0A")) %>% 
      select(Test_Data, pred_0A, pred_BC) %>% 
      dplyr::arrange(Test_Data),
    bordered = TRUE)
  
  output$roc2 <- renderPlot({
    plot(data.0AvsBC.mroc)
  })
  
  output$model2_metric <- renderTable(
    tibble(AUC = data.0AvsBC.auc[1],
           Accuracy = data.0AvsBC.coords[[1]],
           Sensitivity = data.0AvsBC.coords[[2]],
           Specificity = data.0AvsBC.coords[[3]],
           `Pos. Pred. Value` = data.0AvsBC.coords[[4]],
           `Neg. Pred. Value` = data.0AvsBC.coords[[5]]),
    digits = 4)

  
  #For Model 3:
  
  #output of the training data confusion table:
  output$train_conf3 <- renderTable(
    as.tibble(data.ABC.rf$finalModel$confusion) %>% 
      rename(pred_A = A, pred_B = B, pred_C = C) %>% 
      mutate(Training_Data = c("true_A", "true_B", "true_C"),
             pred_A = as.integer(pred_A),
             pred_B = as.integer(pred_B),
             pred_C = as.integer(pred_C)) %>% 
      select(Training_Data, everything()),
    bordered = TRUE)
  
  #output of the test data confusion table:
  output$test_conf3 <- renderTable(
    as.tibble(confusionMatrix(predict(data.ABC.rf, data.ABC.test), data.ABC.test$Result_2016)$table) %>% 
      spread(Prediction, n) %>% 
      rename(pred_A = A, pred_B = B, pred_C = C) %>% 
      mutate(Test_Data = c("true_A", "true_B", "true_C")) %>% 
      select(Test_Data, pred_A, pred_B, pred_C),
        bordered = TRUE)
  
  output$roc3 <- renderPlot({
    plot(data.ABC.mroc)
  })
  
  output$model3_metric <- renderTable(
    tibble(AUC = data.ABC.auc[1],
           Accuracy = data.ABC.coords[[1]],
           Sensitivity = data.ABC.coords[[2]],
           Specificity = data.ABC.coords[[3]],
           `Pos. Pred. Value` = data.ABC.coords[[4]],
           `Neg. Pred. Value` = data.ABC.coords[[5]]),
    digits = 4)
})

