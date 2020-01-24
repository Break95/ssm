#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

library(dplyr)
library(ggplot2)

# Define UI
ui <- dashboardPagePlus(
    header = dashboardHeaderPlus(
        
        # Application title
        title = span(tagList(img(src = "logo_mecydes.png", height = '25px', width = '25px'), "Step by Step Models")),
        
        # Right side bar
        enable_rightsidebar = TRUE,
        rightSidebarIcon = "info-circle"
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Simple Linear Regression", tabName="slr", icon = icon("box")),
            menuItem("Multiple Linear Regression", tabName = "mlr", icon = icon("boxes")),
            menuItem("Logistic Regression", tabName = "logr", icon = icon("wave-sine"))
        )
    ),
    dashboardBody(
        tabItems(
            # Simple Linear Regression
            tabItem(tabName = "slr",
                h2("Simple Linear Regression"),
                # Select data
                sidebarLayout(
                    sidebarPanel(
                        selectInput("dataset", "Choose a dataset:", c('longley', 'BostonHousing', 'abelone')),
                        uiOutput("variable"),
                        uiOutput("objective"),
                        fluidRow(
                            column(4,
                                   numericInput("theta0", "t0:", 0)),
                            column(4,
                                   numericInput("theta1", "t1:", 0)),
                            column(4,
                                   numericInput("learnRate", HTML("&alpha;:") , 0.05 ))
                        )
                    ),
                    
                    mainPanel(
                        plotOutput("plot")
                    )
                ),
                hr(),
                actionButton("step", "Step"),
                actionButton("slowSteps", "Slow Steps"),
                actionButton("final", "Final"),
                textOutput("error")
            ),
            
            # Multi Linear Regression
            tabItem(tabName = "mlr",
                h2("Multiple Linear Regression")
            ),
            
            # Logistic Regression
            tabItem(tabName = "logr",
                h2("Logistic Regression")
            )
        )
    ),
    rightsidebar = rightSidebar(
        backgroud = "dark",
        rightSidebarTabContent(
            id = 1,
            title = h3("About"),
            icon = "address-card",
            active = TRUE,
            ("Información de la página, [...]")
        ),
        rightSidebarTabContent(
            id = 2,
            title = "Updates",
            icon = "wrench",
            textOutput("v0.1 ......")
        )
    )
)

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
    variable_normalization <- function(dataset){
        mins <- apply(dataset, 2, max_cols)
        maxs <- apply(dataset, 2, min_cols)
        means <- apply(dataset, 2, mean_cols)
        
        
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
    
    # Transform 
    normaliza_cols <- function(x, min, max, mean){
        for(i in 1:nrow(x)){
            x[i] = (x[i] - mean) / (max - min)
        }
    }
}

# Run the application 
shinyAppDir(".")
shinyApp(ui = ui, server = server)
