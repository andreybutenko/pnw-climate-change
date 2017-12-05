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
  tabPanel('Water Availability',
    tags$div(
      class = 'hydro-chart half-width',
      plotOutput('seasonal.runoff')
    ),
    plotOutput('snowpack.vs.runoff'),
    plotOutput('snowpack.changes'),
    plotOutput('summer.runoff.projections')
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