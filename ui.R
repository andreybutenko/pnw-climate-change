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
    tags$div(
      class = 'hydrology-container',
      tabsetPanel(type = 'pills',
        tabPanel('Analysis',
          tags$div(
            class = 'hydrology-narrow',
            h1('Intro to runoff and the snowpack'),
            p('What is runoff? What is the snowpack? How is these affected by seasons? And why are they important to Washington State?', class = 'lead'),
            p('Runoff is water that flows into streams, rivers, and oceans from rain or melting snow.'),
            p('Because it is so rainy and snowy in the winters, we get a lot of runoff we use for water! This water is used for drinking, agriculture, and everything else you can think of!'),
            p('But in the summer, it rains much less, and our runoff comes from the melting "snowpack".'),
            p('In the winter, snow builds up on the top of our mountains. This is called "snowpack".'),
            p('In  the summer, we rely on the melting snowpack for water. This next chart shows how significantly runoff is determined by season:'),
            tags$div(
              class = 'hydro-chart half-width',
              plotOutput('seasonal.runoff')
            ),
            p('As you can see, in the winter, there\'s a lot of runoff everywhere on the West side of the mountains. The mountains block rainclouds, so Eastern Washington is very dry.'),
            p('In the summer, all of the runoff comes from mountain peaks where the snowpack is melting.'),
            
            tags$hr(),
            
            p('This next chart shows the relationship between the snowpack and runoff:'),
            plotOutput('snowpack.vs.runoff'),
            p('In Washington State, our runoff has historically been pretty consistent throughout the year because of the snowpack!'),
            p('It is clear that in the winter, the snowpack grows to its peak, and in the summer it reduces to almost nothing.'),
            
            tags$hr(),
            
            h1('How will climate change affect water availability?'),
            p('Climate change affects everyone around the world. Often, this happens in ways you wouldn\'t expect!'),
            p('In the Pacific Northwest, we will have shorter but much more intense snow seasons. This means the snowpack will have much less time to build up, leaving less water for the summer.'),
            p('We will also have much drier summers. In combination with reduced snowpack, this means we will experience consistent drought every summer.'),
            p('There are two future scenarios we can consider in projections. A1B, the higher-emissions scenario, will occur if we take small steps to reduce our carbon emissions. B1, the lower-emissions scenario, will occuur if everyone in the world significantly reduces their carbon emissions.'),
            p('We can compare historic averages to these scenarios to see how snowpack will be affected by climate change:'),
            plotOutput('snowpack.changes'),
            p('This chart reveals that ever if we take big steps towards reducing our carbon emissions, we\'re already "locked in" for some changes. Both in the higher-emissions and lower-emissions scenarios, we will have snowpack that builds up much less and melts much earlier.'),
            p('How will summer-time runoff be affected by climate change?'),
            plotOutput('summer.runoff.projections'),
            p('Because of the reduced snowpack, the mountains will have much less runoff during the summer. But everywhere is getting drier, too: there will be less rain in all areas of Washington State.'),
            
            tags$hr(),
            
            tags$div(
              class = 'hydro-explorations',
              h1('Explorations'),
              p('Do you want to ask your own questions about the impacts of climate change on the hydrology of the Pacific Northwest?', class = 'lead'),
              p('Scroll to the top of the page and press "playground" to create your own monthly, seasonal, or geographical charts.')
            )
          )
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