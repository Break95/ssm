library(dplyr)
library(ggplot2)
library(plotly)

# Define server logic
server <- function(input, output, session) {
  
  # Outputs
  output$variable <- renderUI({
    selectInput("variable1", "Select predictor variable:", names(get(input$dataset)))
  })
  
  output$objective <- renderUI({
    selectInput("variable2", "Select objective variable:", names(get(input$dataset)))
  })
  
  output$plot <- renderPlot({
    p <-ggplot(data = get(input$dataset), aes(x = get(input$variable1), y = get(input$variable2))) +
      geom_point() + 
      geom_abline(intercept = input$theta0, slope = input$theta1) + 
      labs(title="Linear Regresion", x = input$variable1, y = input$variable2)
    
    p <- ggplotly(p)
    print(p)
  })
  
  
  ############################
  ###### Observed Events #####
  ############################
  
  # Complete one step of gradient descent.
  observeEvent(input$step, {
    var1 = input$variable1
    var2 = input$variable2
    data = get(input$dataset)
    th0 = input$theta0
    th1 =  input$theta1
    
    # 
    calc_error <- function(x){
      pred <- th0 + (th1 * x[var1])
      error <- pred - x[var2]
      error
    }
    
    
    
    output$error <- renderText({
      toString(sum(apply(data, 1, calc_error)))
    })
    
    # Theta update function
    th1_error <- function(x){
      pred <- th0 + (th1 * x[var1])
      print(x[var1])
      error <- (pred - x[var2]) * x[var1]
      error
    }
    
    lr <- input$learnRate
    
    # Update Thetas
    newTheta0 <- th0 - (input$learnRate / nrow(data)) * sum(apply(data, 1, calc_error))
    newTheta1 <- th1 - (input$learnRate / nrow(data)) * sum(apply(data, 1, th1_error) )
    
    updateNumericInput(session, "theta0", value = newTheta0)
    updateNumericInput(session, "theta1", value = newTheta1)
  })
  
  # Automatically reduce error, slow for visualization.
  observeEvent(input$slowSteps, {
    
  })
  
  # Get fitted model through normal ecuation.
  observeEvent(input$final, {
    
  })
  
  ############################
  ###### Other functions #####
  ############################
  
  # Normalizattion of the dataframe to range [-1,1]
  variable_normalization <- function(dataset, cols){
    # Check if the specified cols are in range [-1,1].
    
    
    # Modify specified cols. 
    mins <- apply(dataset, 2, max_cols)
    maxs <- apply(dataset, 2, min_cols)
    means <- apply(dataset, 2, mean_cols)
    
    for(col in cols){
      for(i in 1:nrow(dataset)){
        dataset[i,col] = (dataset[i,col] - means[j]) / (maxs[j] - mins[j])
      }
    }
    
    dataset
  }
  
  # Gets the maximun value of each column.
  max_cols <- function(x){
    max(x)
  }
  
  # Gets the min value of each column.
  min_cols <- function(x){
    minx(x)
  }
  
  # Gets the mean of each column.
  mean_cols <- function(x){
    mean(x)
  }
  
}