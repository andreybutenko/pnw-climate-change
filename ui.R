library(shiny)
library(plotly)
source('./scripts/visitation_data.R')
my.ui <- navbarPage(
  
  # Application title
  "Pacific Northwest Climate Change",
  
  # Sidebar with a slider input for number of bins 
  tabPanel("",
           sidebarLayout(
             sidebarPanel(
              
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               
             )
           )
  ), 
  tabPanel("",
              sidebarLayout(
                sidebarPanel(
                  
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                  
                )
              )
  ),
  tabPanel("",
           sidebarLayout(
             sidebarPanel(
               
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               
             )
           )
  ),
  tabPanel("Visitation Data",
           sidebarLayout(
             sidebarPanel(
               selectInput('season',"Select a Season",
                           choices = c("Summer" = 'summer', "Winter" = 'winter', 'Fall' = 'fall', 'Spring' = 'spring', 'Anual' = 'all' ), selected = "summer")
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