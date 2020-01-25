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

# Define UI
ui <- dashboardPagePlus(
  # Header ----
  header = dashboardHeaderPlus(
    
    # Application title
    title = span(tagList(img(src = "logo_mecydes.png", height = '25px', width = '25px'), "Step by Step Models")),
    
    # Right side bar
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "info-circle"
  ),
  # Sidebar ----
  dashboardSidebar(
    sidebarMenu(
      menuItem("Simple Linear Regression", tabName="slr", icon = icon("box")),
      menuItem("Multiple Linear Regression", tabName = "mlr", icon = icon("boxes")),
      menuItem("Logistic Regression", tabName = "logr", icon = icon("wave-sine"))
    )
  ),
  # Dashboard body ----
  dashboardBody(
    tabItems(
      # SLR Tab ----
      tabItem(tabName = "slr",
              h2("Simple Linear Regression - Gradient Descent"),
              tabsetPanel(type = "tabs", 
                  # SRL - App ----
                  tabPanel("App", # Select data
                    sidebarLayout(
                      sidebarPanel(
                        hr(),
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
                  # SLR - Notes ----
                  tabPanel("Notes", verbatimTextOutput("Summary"))
              )
      ),
      
      # MLR Tab ----
      tabItem(tabName = "mlr",
              h2("Multiple Linear Regression")
      ),
      
      # LR Tab ----
      tabItem(tabName = "logr",
              h2("Logistic Regression")
      )
    )
  ),
  # Right side bar ----
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