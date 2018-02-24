library(shiny)
library(leaflet)
load("variables.rda")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("New Zealand on census night 2013, at a glance"),
  
  sidebarLayout(
    sidebarPanel(
       radioButtons("area", "Area type",
                    choices = c("Regional Council", "Territorial Authority", "Area Unit"),
                    selected = "Territorial Authority"),
       selectInput("variable", "Variable for colour", 
                   choices = variables, 
                   selected = "NoChildren"),
       p("...as a proportion of all dwellings, households, or individuals (whichever is relevant)."),
       hr(),
       selectInput("colour_scheme", "Colour Scheme",
                   choices = c("Reds", "Blues", "Greys", 
                               "Spectral", "YlOrRd", "RdYlBu", "RdGy"),
                   selected = "Spectral"),
       radioButtons("colour_order", "Colour order?",
                    choices = c("Original", "Reverse"),
                    selected = "Original")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       h3(textOutput("the_title")),
       leafletOutput('myMap', width = 600, height = 600),
       p("Regions are expanded or shrunk to be proportionate to the usual resident population in 2013."),
       HTML("<p>Source: Statistics New Zealand Census 2013.  Analysed via the <a href = 'https://github.com/ellisp/nzelect'>
nzcensus</a> R package.  Map was drawn with <a href='http://scapetoad.choros.ch/'>ScapeToad</a>.  See 
<a href ='https://ellisp.github.io'>Peter's Stats Stuff</a> for more detail.")
    )
  )
))
