library(shiny)
library(shinyjs)
library(shinycssloaders)

shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "my_styles.css")
  ),
  
  tags$style(HTML("@import url('https://fonts.googleapis.com/css?family=Roboto');
@import url('https://fonts.googleapis.com/css?family=Prosto One');
  ")),
  
  useShinyjs(),
  
  # Application title
  titlePanel("Population Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      submitButton(text = "Run simulation", icon = NULL, width = NULL), 
      # actionButton("reset_all", "Reset all", icon = NULL, width = NULL), # never was able to get this to work
      
      # put a div around all the inputs so we can reset them all at once:
      div(id = "inputs",
        sliderInput("first_ave_child_rate",
                     "Average children per women at the beginning of the session:",
                     min = 0,
                     max = 8,
                     value = 2.1,
                     step = 0.1),
        sliderInput("mimr",
                    "Starting male infant mortality, deaths per 1,000 in first year",
                    min = 0,
                    max = 500,
                    value = 3.4, 
                    step = 0.1),
        sliderInput("fimr",
                    "Starting female infant mortality, deaths per 1,000 in first year",
                    min = 0,
                    max = 500,
                    value = 2.8, 
                    step = 0.1),
        sliderInput("death_mult",
                    "Death rates (other than in first year) compared to rich country in 2015",
                    min = 0.2,
                    max = 5,
                    value = 1, 
                    step = 0.1),
        
        sliderInput("ssr", 
                    "Number of births of boys for every 100 births of girls",
                    min = 90, 
                    max = 120,
                    value = 107),
        
        sliderInput("birth_rate_perturb",
                     "Size and direction of random change in average children per women",
                     min = -0.2,
                     max = 0.2,
                     value = 0),
         sliderInput("death_rate_perturb",
                     "Size and direction of random change in death rates for age groups",
                     min = -0.5,
                     max = 0.5,
                     value = 0),
         sliderInput("mean_starting_age_f",
                     "Average age for females at beginning of simulation",
                     min = 10,
                     max = 80,
                     value =30),
         sliderInput("mean_starting_age_m",
                     "Average age for males at beginning of simulation",
                     min = 10,
                     max = 80,
                     value = 30),
         sliderInput("starting_n",
                     "Number of couples to start with",
                     min = 10,
                     max = 10000,
                     value = 1000),
        sliderInput("number_years",
                     "Number of years to run the simulation over",
                     min = 50, max = 1000, value = 250)
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
         withSpinner(plotOutput("fourplots", height = "650px"), type = 6)
         ),
        tabPanel("Demographic pyramids",
         withSpinner(plotOutput("pyramids", height = "650px"), type = 6)
        )
    ),
    HTML("<p>See <a href='http://freerangestats.info/blog/2018/06/26/fertility-rate'>Free Range Statistics</a>
for discussion and explanation of this demonstration population simulator.</p>")
    )
  )
))
