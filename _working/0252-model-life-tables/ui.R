# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  
  use_googlefont("Prosto One"),
  use_googlefont("Sarala"),
  use_googlefont("Roboto"),

    # Application title
    titlePanel("UN Model Life Tables"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            shiny::selectInput("family",
                        "Model family",
                        choices = families),
            sliderInput("e0", 
                        "Life expectancy at birth",
                        min = 20, max = 100, step = 1, round = TRUE, value = 60),
            
            HTML("<p>Produced by freerangestats.info. Read the original 
                 <a href='http://freerangestats.info/blog/2023/08/06/model-life-tables'>blog post</a>.</p>")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("life_exp")
        )
    )
)
)