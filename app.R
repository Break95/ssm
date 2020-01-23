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

library(ggplot2)

# Define UI
ui <- dashboardPage(
    dashboardHeader(
        # Application title
        title = "Step by Step Models"
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Simple Linear Regression", tabName="slr"),
            menuItem("Multiple Linear Regression", tabName = "mlr"),
            menuItem("Logistic Regression", tabName = "logr")
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
                        uiOutput("objective")
                    ),
                    
                    mainPanel(
                        plotOutput("plot")
                    )
                )
            ),
            tabItem(tabName = "mlr",
                h2("Multiple Linear Regression")
            ),
            tabItem(tabName = "logr",
                h2("Logistic Regression")
            )
        )
    )
)

# Define server logic
server <- function(input, output, session) {

    output$variable <- renderUI({
        selectInput("variable1", "Select predictor variable:", names(get(input$dataset)))
    })

    output$objective <- renderUI({
        selectInput("variable2", "Select objective variable:", names(get(input$dataset)))
    })
    
    output$plot <- renderPlot({
        p <-ggplot(data = get(input$dataset), aes(x = get(input$variable1), y = get(input$variable2))) +
            geom_point() + 
            labs(title="Linear Regresion", x = input$variable1, y = input$variable2)
        
        print(p)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
