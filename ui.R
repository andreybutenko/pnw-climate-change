library(shiny)
library(plotly)
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
  tabPanel("",
           sidebarLayout(
             sidebarPanel(
               
             ),
             # Show a plot of the generated distribution
             mainPanel(
               
             )
           )
           
  )
)

# Define UI for application that draws a histogram
shinyUI(my.ui)