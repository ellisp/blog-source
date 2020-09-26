

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Visualising a Dirichlet distribution"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            p("A Dirichlet distribution is for a random variable of multiple probabilities that add up to one.
              The chart on the right illustrates this with the three probabilities p1, p2 and p3."),
            p("The parameters in the vector 'alpha' determine the likelihood of various combinations of 
              values for p1, p2 and p3. Use the sliders below to experiment with different values of 
              alpha. For example, values below 1 for all elements of alpha make it likely that just
              one of p1, p2 or p3 will be close to 1 and the other two will be close to zero."),
            
            sliderInput("a1",
                        "Alpha 1:",
                        min = 0.5,
                        max = 5,
                        value = 2),
            sliderInput("a2",
                        "Alpha 2:",
                        min = 0.5,
                        max = 5,
                        value = 2),
            sliderInput("a3",
                        "Alpha 3:",
                        min = 0.5,
                        max = 5,
                        value = 2),
            
            HTML("<p>Read more about <a href='https://en.wikipedia.org/wiki/Dirichlet_distribution'>
                 the Dirichlet distribution on Wikipedia</a></p>.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("d_plot")
        )
    )
))
