#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
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
                            column(3,
                                   numericInput("theta0", "t0:", 0)),
                            column(3,
                                   numericInput("theta1", "t1:", 0))
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
                uiOutput("error")
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

    # Observed Events
    observeEvent(input$step, {
        variable = input$variable1
        data = get(input$dataset)
        t0 = input$theta0
        t1 =  input$theta1
        
        calc_error <- function(th0, th1, x){
            test <- th0+(th1 * x)
            test
        }
        
        error <- data$variable %>% calc_error(t0, t1, .) %>% sum()
        #error <- do(data, aggregate((t1 + t0 * .$variable)))
        
       print(error)
    })
    
    observeEvent(input$slowSteps, {
        
    })
    
    observeEvent(input$final, {
        
    })
    
}

# Run the application 
shinyAppDir(".")
shinyApp(ui = ui, server = server)
