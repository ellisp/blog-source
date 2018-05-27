library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Demo demographic simulation"),
  
  sidebarLayout(
    sidebarPanel(
       sliderInput("first_ave_child_rate",
                   "Average children per women at the beginning of the session:",
                   min = 0,
                   max = 8,
                   value = 2.1,
                   step = 0.1),
       sliderInput("birth_rate_perturb",
                   "Size and direction of random change in average children per women",
                   min = -0.05,
                   max = 0.05,
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
       sliderInput("sd_starting_age", 
                   "Standard deviation of distribution of ages at beginning",
                   min = 0, 
                   max = 25,
                   value = 10),
       sliderInput("ssr", 
                   "Number of births of boys for every 100 births of girls",
                   min = 90, 
                   max = 120,
                   value = 107),
       sliderInput("starting_n",
                   "Number of couples to start with",
                   min = 10,
                   max = 10000,
                   value = 1000)
    ),
    
    mainPanel(
       plotOutput("multiplot"),
       plotOutput("populations")
    )
  )
))
