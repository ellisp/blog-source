

# Define UI for application that draws a ternary density plot for a Dirichlet distribution
# Peter Ellis 26 September 2020
shinyUI(fluidPage(

    # Application title
    titlePanel("Visualising a Dirichlet distribution"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            p("A Dirichlet distribution is the distribution of a random variable of multiple 
            probabilities that add up to one.
              The chart on the right illustrates this with the three probabilities p1, p2 and p3.
              Darker parts of the chart indicate combinations of p1, p2 and p3 that are more likely,
              given a certain set of values for the parameter 'alpha'."),
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
                 the Dirichlet distribution on Wikipedia</a>.</p>"),
            HTML("<p>Check out the source code 
            <a href='https://github.com/ellisp/blog-source/tree/master/_working/0194-dirichlet'>
                 for this web application on GitHub</a>.</p>")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("d_plot", height = "550px"),
            HTML("<p>This is a <a href='https://en.wikipedia.org/wiki/Ternary_plot'>ternary plot</a>,
                 used to show composition of a total amount that must sum up to a constant (in this case,
                 the three probabilities p1, p2 and p3 must add up to one). If it is hard to read for you,
                 don't worry, you aren't alone. I find this very hard to interpret myself other than in the
                 broadest intuitive sense (eg the corners are clearly extreme values where one of the three
                 variables is close to 1 and the other two are close to 0).<p>")
        )
    )
))
