library(shiny)
library(plotly)
source('./scripts/visitation_data.R')
my.ui <- navbarPage(
  theme = 'styles.css',
  
  # Application title
  "Pacific Northwest Climate Change",
  
  tabPanel("Home"
  ),
  
  # Sidebar with a slider input for number of bins 
  tabPanel('Hydrology',
    tabsetPanel(type = 'pills',
      tabPanel('Analysis',
        tags$div(
          class = 'hydro-chart half-width',
          plotOutput('seasonal.runoff')
        ),
        plotOutput('snowpack.vs.runoff'),
        plotOutput('snowpack.changes'),
        plotOutput('summer.runoff.projections')
      ),
      
      tabPanel('Playground',
        sidebarLayout(
          sidebarPanel(
            selectInput('playground.type', 'Chart type', c(
              'Monthly Avg' = 'monthly',
              'Seasonal Avg' = 'seasonal',
              'Geographical Comparison' = 'geo'
            ), selected = 'monthly'),
            uiOutput('hydro.widgets')
          ),
          mainPanel(
            plotOutput('hydro.playground.chart')
          )
        )
      )
    )
    
  ), 
  
  tabPanel("blank",
    sidebarLayout(
      sidebarPanel(
        
      ),
      mainPanel(
      
      )
    )
  ),
  
  tabPanel("blank",
      sidebarLayout(
      sidebarPanel(
        
      ),
      mainPanel(
        
      )
    )
  ),
  
  tabPanel("Visitation Data",
    sidebarLayout(
      sidebarPanel(
        selectInput('season',"Select a Season",
        choices = c("Summer" = 'summer', "Winter" = 'winter', 'Fall' = 'fall', 'Spring' = 'spring', 'All' = 'all' ), selected = "summer")
      ),
      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput("distPlot")
      )
    )
  
  )
)

# Define UI for application that draws a histogram
shinyUI(my.ui)