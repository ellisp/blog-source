library(shiny)
library(ggvis)

load("dimensions.rda")

# Define UI for application that draws a histogram
shinyUI(fluidPage(style='width: 1000px',
   tags$style(HTML("
@import url('https://fonts.googleapis.com/css?family=Lato');
                   
                   body {font-family: 'Lato', 'Lucida Grande', Verdana, Lucida, Helvetica, Arial, Calibri, sans-serif;}
                   ")),
     
  # Application title
  titlePanel("Modelled individual party vote in the New Zealand 2014 General Election"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(4, 
       selectInput("WorkStatus",
                   "Working status",
                   WorkStatuses,
                   selected = "Full time"),
       checkboxInput("SuperviseAnyone",
                     "Supervise someone in the workplace"),
       checkboxInput("HHMemberTradeUnion",
                     "You or household member belong to a trade union"),
       checkboxInput("HHMemberProfAssoc",
                     "You or household member belong to a professional association",
                     value = TRUE),
       checkboxInput("IdentifyWorkingClass",
                     "Identify as working class"),
       checkboxInput("Student",
                     "Currently a student"),
       selectInput("HighestQual",
                   "Highest education / training qualification",
                   HighestQuals,
                   selected = "University"),
       selectInput("HHIncome",
                   "Household income",
                   HHIncomes,
                   selected = "Higher"),
       checkboxInput("City",
                     "Live in a city",
                     TRUE),
       checkboxInput("Male",
                     "Male"),
       checkboxInput("Marital",
                     "Married or de facto"),
       checkboxInput("European",
                    "European ethnicity",
                    value = TRUE),
       checkboxInput("Maori",
                     "Maori ethnicity"),
       checkboxInput("NZBorn",
                     "Born in New Zealand",
                     TRUE)
       
       
    ),
    
    # Show a plot of the generated distribution
    column(8, 
       ggvisOutput("distPlot"),
       sliderInput("dage",
                   "Age:",
                   min = 18,
                   max = 99,
                   value = 45),
       selectInput("Religion",
                   "Religion",
                   Religions,
                   selected = "No religion"),
       checkboxInput("OwnHouseOrFlat",
                     "Own the house or flat you live in"),
      HTML("<hr><p>Modelled probabilities are based on analysis of the New Zealand Election Study, 
with survey data collected soon after the 2014 election.  These are statistical inferences drawn
from what individuals told researchers about their own information and voting behaviour.</p>  

<p>For the technically minded, the probabilities you see are the average of 
two different predictive models - a random forest from Wright and Ziegler's <a href = 'https://CRAN.R-project.org/package=ranger'>ranger</a> 
R package, and 
and a multinomial log-linear model from Venables and Ripley's <a href = 'http://www.stats.ox.ac.uk/pub/MASS4'>nnet</a> R package.  See 
<a href = 'https://ellisp.github.io/blog/2017/05/14/nzes-app'>Peter's Stats Stuff</a> for details and for 
<a href = 'http://ellisp.github.io/elections/elections.html'>more elections analysis and forecasts</a>.</p>")
       
       
    )
  )
))
