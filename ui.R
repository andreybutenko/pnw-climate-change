library(shiny)
library(plotly)
source('./scripts/visitation_data.R')
my.ui <- navbarPage(

  theme = 'styles.css',

  # Application title
  "Pacific Northwest Climate Change",

  tabPanel("Home",
           plotOutput("tep.temp.chart.white"),
           h1("Pacific Northwest Climate Change"),
           p("Our project studies the relationship between climate change and its effects on the Pacific Northwest. The entire globe will be affected by climate change, but most of the time, the effects of climate change seem to always only be present inn far off places, like the North Pole. We decided it was important to show how climate change really does hit close to home for us and other University of Washington students. In doing so, we hope to inspire more students to take part in the fight to save the environment."),
           h2("Page Description"),
           p("The Hydrology tab describes how climate change will affect the runoff and snowpack in the Pacific Northwest."), 
           p("If you click on the Salmon tab, you will find data on how climate change will affect stream temperatures and the salmon population, which the economy of the PNW is heavily reliant on."),
           p("The fourth tab, Temperature and Precipitation, is on temperature and precipitation predictions in the PNW for the 21st century."),
           p("Looking at the economy tab will show what jobs are impacted by climate change, something that will most definitely change the day-to-day lives of Washington residents."),
           p("The data in the Visitation tab discusses National Park visitation rates and how those will be affected by warmer climates."),
           p("The Olympic National Park tab delves deeper into how national parks are affected by centering on the Olympic National Park. It does not explore visitation rates, but what will happen to the park itself based on climate trends.")
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
            p('What is runoff? What is the snowpack? How are these affected by seasons? And why are they important to Washington State?', class = 'lead'),
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

  tabPanel('Salmon',
    tags$div(
      class = 'hydrology-narrow salmon-container',
      h1('Climate Change and Stream Temperature'),
      p('What are stream temperatures like today? How will they change in the future? What will this mean for Salmon spawning?', class = 'lead'),
      p('As climate change continues, air temperature will go up, but the temperature of water will also go up.'),
      p('This may not affect humans directly, but this means a lot to water creatures of all kinds. This is not just something happening far away: this affects us in Washington State, too.'),
      p('Salmon are important for the Washington State fishing industry, and hold special significance to Native groups. Many factors influence where Salmon can spawn, and temperature is just one of them.'),
      p('We can compare historic stream temperatures to projected future stream temperatures to see how rising temperatures will appear in the Olympic Penninsula:'),
      plotOutput('stream.temp.chart'),
      plotOutput('stream.temp.diff.chart'),

      tags$hr(),

      h1('Salmon Spawning'),
      p('Salmon spawning is sensitive to temperature! Less than 18 degC is ideal, and anything more than 22 degC can be fatal.'),
      p('Salmon populations in Washington, Oregon, and California are already shrinking and becoming more unhealthy, which is caused by overfishing, pollution, and rising temperatures.'),
      plotOutput('wa.stream.temp.chart'),
      plotOutput('suitability.chart')
    )
  ),

  tabPanel("Temperature and Precipitation",
         tags$div(
           class = 'hydrology-narrow',
           h1("Pacific Northwest Temperature and Precipitation Predictions"),
           p("The data on this page was retrieved from the University of Washington Climate Impacts Group Resources page. This data was based off of the Coupled Model Intercomparison Projects Phase 5, or CMIP5. It uses Global Climate Models to predict how climate change will affect temperature and precipitation in the 21st century. The datasets included different possible scenarios, called Representative Concentration Pathways, in terms of our current climate emissions and how we continue to regulate them. RCP 2.6 signifies the lowest emissions with the most mitigations, RCP 8.5 signifies the highest emissions with the least mitigations, and RCPs 4.5 and 6 fall inbetween."),
       h2("Average Change Projected"),
    sidebarLayout(
      sidebarPanel(
        selectInput("gcm.scenario",
                    "Scenario",
                    c("RCP2.6",
                      "RCP4.5",
                      "RCP6",
                      "RCP8.5"))
      ),
      mainPanel(
        plotOutput("avg.prec.chart"),
        plotOutput("avg.temp.chart")
      )
    ),
    p("The charts above show the average change of temperature and precipitation in the years 2040, 2050, and 2080. The 'Scenario' selection allows you to change which RCP was used to calculate these averages."),
    h2("Time Evolving Projections"),
    sidebarLayout(
      sidebarPanel(
        selectInput("gcm.season",
                    "Season",
                    c("Annual",
                      "Winter",
                      "Spring",
                      "Summer",
                      "Autumn"))
      ),
      mainPanel(
        plotOutput("tep.prec.chart"),
        plotOutput("tep.temp.chart.gray"),
        p("These charts display data on Time Evolving Projections. The line in orange signifies what data points were recorded and used to develop the different RCPs. The other lines that diverge after 2000 are representing the different RCPs, which extend until 2095. The transparent lines are a line connecting the actual data points, and the bold show a general curve for each grouping. The 'Season' selection allows you to choose which season you would like to see these calculations in.")
      )
    )
   )
  ),
  
  tabPanel("Economy",
           tags$div(
             class = "hydrology-narrow",
             h1("Impact on Employment, Gross Receipt, and Industries"),
             tags$img(src = 'http://www.bottomupeconomy.org/wp-content/uploads/2015/11/new-economy.jpg', alt='Image of local economy being impacted by climate change', class = 'image-center leaf-image'),
             p("Climate change being a global issue, we often times forget about its impact on our local economies. Sparking our research with such concern, we have gathered data on how the economy,specifically employment and gross receipt, of the counties in Western Washington near the ocean are impacted."),
             p("This data set describes the self-employed workers whose jobs directly depend on the resources of the oceans and Great Lakes. Data are derived from Census Nonemployer Statistics and include the number of self-employed workers and gross receipts for the six sectors defined by ENOW. These time-series data are available at the national, regional, state, and county levels."),
             p("This page specifically focuses on the economy of the counties in Washington to showcase the impact of climate change on our local economy."),
             h1(""),
             h1("")
           ),
           
           h1('Employment and Gross Receipt by Year'),
           sidebarLayout(
             sidebarPanel(
               
               selectInput('economy.year', 'Year', choices = 2005:2014, selected = 2005:2014, multiple = T),
               selectInput('economy.industry', 'Industries',
                 choices = c('Marine Construction', 'Living Resources', 'Offshore Mineral Resources', 'Ship and Boat Building', 'Tourism and Recreation', 'Marine Transportation'),
                 selected = c('Marine Construction', 'Living Resources', 'Offshore Mineral Resources', 'Ship and Boat Building', 'Tourism and Recreation', 'Marine Transportation'),
                 multiple = T
               ),
               selectInput('economy.column', 'Operation', choices = c(
                 'Revenue' = 'GrossReceipts',
                 'Employment' = 'Employment'
               )),
               selectInput('economy.operation', 'Operation', choices = c(
                 'Sum' = 'sum',
                 'Mean' = 'mean'
               ))
             ),
             mainPanel(
               class = 'economy-chart-container',
               plotOutput('economy.chart')
             )
             
           ),
           
           tags$div(
             class = "hydrology-narrow economy-reflection",
             h1("Reflecting on the data"),
             p("Early in May of this year, Trump administration announced the withdrawl of U.S. from the Paris Climage Agreement. This action showed the weakening of efforts to combat global warming under the concern of our own economy. However, many researches show a long term effect of climate change threatening our economy."),
             p("By mapping out the detailed, comprehensive changes of the employment and gross receipt in counties near the body of water from the past years, we are introduced to the local dependency on ocean resources through different metrics that are presented above: number of employment and gross revenue of different sectors of ocean industry."),
             p("The data being narrowed down to a county level, it shows how the changes in the ocean industry are creating impacts on individual levels.")
           )
  ),
  
  tabPanel("Visitation Data",
         tags$div(
           class = 'hydrology-narrow',
           h1('National Park Visitation Data'),
           p("What does national park visitation have to do with climate change in the Pacific North West?", class = 'lead'),
           p("As we all probably know, Washington State, and the Pacific North West in general is a beautiful area, and because of this we get tourists. These tourists, combined with our own local visitors help make Washington state's parks some of the most visited in the country. Here are graphs to show just how many people visit our parks and how much we risk to lose as climate change destroys our parks"),
           p(""),
           h1('Average Visitation per Month'),
           #Section 1
           plotlyOutput("visitationmonthPlot"),
           h1('Seasonal Visitation Trends'),
           #Section 2
           sidebarLayout(
             sidebarPanel(
               selectInput('season',"Select a Season", choices = c("Summer" = 'summer', "Winter" = 'winter', 'Fall' = 'fall', 'Spring' = 'spring' ), selected = "summer")

             ),
             mainPanel(plotlyOutput("visitationPlot"))
           ),
           h1('Annual Visitation Trends'),
           #Section 3
           sidebarLayout(
             sidebarPanel(
               radioButtons('chart.toggle', label = h3("Plot Type"),
                            choices = list("Mean" = 1, "Total" = 2),
                            selected = 2),
               radioButtons('trend', label = h3('Show trend'),
                            choices = list('Trend' = TRUE, 'No Trend' = FALSE),
                            selected = FALSE)
             ),
             mainPanel(plotlyOutput('annualPlot'))
           ),
           p("As you can see, our state's visitation really increases during the summer. In an article on future park visitation and how it would be affected by climate change written by PLOS, they sugested that parks that would have increased due to climate change would have increased visitation while those that would become colder would suffer from lower visitation. So, interestingly, it seems that our parks would benefit from global warming at first. However, the study did mention there was a sharp decline in visitation after crossing 25 degrees celcius. So, at least until we hit that threshold, we could possibly have increased visitation in parks. Fisichelli, Nicholas A., Gregor W. Schuurman, William B. Monoham, and Pamela S. Ziesler. Protected Area Tourism in a Changing Climate: Will Visitation at US National Parks Warm Up or Overheat? PLOS, journals.plos.org/plosone/article?id=10.1371/journal.pone.0128226#sec006. Accessed 6 Dec. 2017.")
         )
  ),

  tabPanel("Olympic National Park",
           tags$div(
             class = "hydrology-narrow",
             h1("Spotlight On Olympic National Park"),
             tags$img(src = 'https://i.imgur.com/fA33vWo.png', alt='Map and picture of Olympic National Park park', class = 'image-center'),
             p("Olympic National Park is a 1,442 mi² park located in the Olympic Peninsula of Washington State. Not only is it home to the only rainforest in the continental United States it is also one of the largest remaining untouched habitats for many endangered specieses. As a result of its obvious natural appeal this place is was visited by 50,148 different people in 2016 which while a good thing for the local economy does threaten the pristine habitat"),
             p("This park is also a place at risk of eventually being ruined by the effects of climate change and poor air quality from adjacent cities. Below you can explore visitation data, historical temperature data, as well as air quality data to see just how at risk this region is by the growth of these problems."),
             h1("")

             ),

           h1('Wildfire Trends'),
           sidebarLayout(
             sidebarPanel(
               sliderInput('wildfire.years', 'Years',
                 min = 1997, max = 2016,
                 value = c(1997, 2016)), 

               selectInput('wildfire.causes', 'Causes', choices = c(
                 'Human', 'Natural'
                  ), selected = c('Human', 'Natural'), multiple = T),

               selectInput('wildfire.classes', 'Fire Classes', choices = c(
                 'A: 0 - 1/4 acres' = 'A',
                 'B: 1/4 - 10 acres' = 'B',
                 'C: 10 - 100 acres' = 'C',
                 'D: 100 - 300 acres' = 'D',
                 'E: 300 - 1000 acres' = 'E',
                 'F: 1000 - 5000 acres' = 'F',
                 'G: 5000+ acres' = 'G',
                 'NR: No Record' = 'NR'
                ), selected = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'NR'), multiple = T),

               selectInput('wildfire.group', 'X-Axis', choices = c(
                 'Monthly Averages' = 'month',
                 'Yearly Averages' = 'year'
                ), selected = 'month')
             ),
             mainPanel(
               plotOutput('wildfire.chart')
             )
           ),

           h1('Visitorship Trends'),
           sidebarLayout(
             sidebarPanel(
               radioButtons("spt.one.toggle", label = h3("Organize By"),
                            choices = list("Month" = 1, "Year" = 2),
                            selected = 2),
               uiOutput("spt.one.ui")
             ),
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("spt.one.plot")
             )
           ),

           h1('Temperature Trends'),
           sidebarLayout(
             sidebarPanel(
               radioButtons("spt.one.togglegroup", label = h3("Group By"),
                            choices = list("None" = 0, "Months" = 2, "Years" = 1),
                            selected = 1),
               radioButtons("spt.one.togglehilow", label = h3("Show Min and Max"),
                            choices = list("Yes" = 1, "No" = 2),
                            selected = 2),
               uiOutput("spt.two.ui")
             ),
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("spt.two.plot")
             )
           ),

           h1('Air Quality Trends'),
           sidebarLayout(
             sidebarPanel(
               selectInput("spt.three.data", label = h5("Select Year"),
                           choices = list("Any" = "Any", "1996" = "1996","1997" = "1997","1998" = "1998","1999" = "1999","2000" = "2000","2001" = "2001","2002" = "2002","2003" = "2003","2004" = "2004","2005" = "2005","2006" = "2006","2007" = "2007","2008" = "2008","2009" = "2009","2010" = "2010","2011" = "2011","2012" = "2012","2013" = "2013","2014" = "2014","2015" = "2015","2016" = "2016"),
                           selected = 1)
             ),
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("spt.three.plot")
             )
           )

  ),
  tabPanel('About',
   tags$div(
     class = 'hydrology-narrow salmon-container',
     h1('About This Project.'),
     p('Climate change is often seen as an issue that affects only far-off regions of Earth: melting ice in the Arctic, extreme flooding in isolated island-nations, and increased food insecurity throughout Africa are just some examples. However, climate change affects every region in different ways.'),
     p('Our project is an educational resource to explain the local impact of climate change to students in Washington State. We have a particular interest in ocean health, and how that will not only affect the environment, but the economic state of Washington as well.'),
     p('We used a lot of great data sets in working on this project:'),
     tags$ul(
       tags$li(
         tags$a(href = 'https://cig.uw.edu/datasets/downscaled-global-climate-model-projections-for-the-pnw/', target = '_blank', 'UW: Global Climate Model Projections for the PNW')
       ),
       tags$li(
         tags$a(href = 'https://cig.uw.edu/datasets/north-pacific-region-hydroclimate-scenarios/', target = '_blank', 'UW: North Pacific Region Hydroclimate Scenarios')
       ),
       tags$li(
         tags$a(href = 'https://cig.uw.edu/datasets/washington-state-water-temperature-projections/', target = '_blank', 'UW: Washington State Water Temperature Projections')
       ),
       tags$li(
         tags$a(href = 'https://coast.noaa.gov/digitalcoast/data/enow-nes.html', target = '_blank', 'NOAA: ENOW for Self-Employed Workers')
       ),
       tags$li(
         tags$a(href = 'https://irma.nps.gov/Stats/', target = '_blank', 'NPS: Visitor Use Statistics')
       ),
       tags$li(
         tags$a(href = 'https://www.nps.gov/subjects/socialscience/vse.htm', target = '_blank', 'NPS: Economic Contributions of National Park Visitor Spending')
       ),
       tags$li(
         tags$a(href = 'https://m3challenge.siam.org/node/336', target = '_blank', 'M3: National Park Modelling Data Sets')
       )
     ),
     p('A special thanks to Professor Dargan Frierson who connected us with many of these data sets.'),

     tags$hr()
    ),

     tags$div(
       class = 'bio-container',

       tags$div(
         id = 'andrey',
         class = 'bio-node',
         img(src = 'andrey-silly.png'),
         tags$div(
           class = 'bio-content',
           tags$h2('Andrey Butenko'),
           p('Andrey Butenko is a student studying Informatics at the University of Washington. He has a background in web and mobile app development, and is interested in software to enable sustainable and informed decision-making.')
         )
       ),

       tags$div(
         id = 'jaren',
         class = 'bio-node',
         img(src = 'jaren-silly.png'),
         tags$div(
           class = 'bio-content',
           tags$h2('Jaren Tilley'),
           p('Jaren Tilley is a current UW student studying Informatics who loves R. He also loves Bao Dinh, Mike Freeman, and Info201 - he describes it as "The best class at UW hands down".')
         )
       ),
       
       tags$div(
         id = 'alexis',
         class = 'bio-node',
         img(src = 'alexis-silly.png'),
         tags$div(
           class = 'bio-content',
           tags$h2('Alexis Choi'),
           p('Alexis Choi is a Sophomore at the University of Washington studying Informatics. She is interested in applying her skills to increase the public\'s access to the legal system.')
         )
       ),

       tags$div(
         id = 'caleb',
         class = 'bio-node',
         img(src = 'caleb-silly.png'),
         tags$div(
           class = 'bio-content',
           tags$h2('Caleb Kierum'),
           p('Caleb Kierum is an aspiring Computer Scientist currently studying at UW. Outside of his technical endeavors Caleb loves writing and performing music.')
         )
       ),
       
       tags$div(
         id = 'sabrina',
         class = 'bio-node',
         img(src = 'sabrina-silly.png'),
         tags$div(
           class = 'bio-content',
           tags$h2('Sabrina Hersh'),
           p('Sabrina Hersh is studying Environmental Science and Biology at the University of Washington. She enjoys being able to try new things and watching movies with friends.')
         )
       )
     )
  )
)

# Define UI for application that draws a histogram
shinyUI(my.ui)
