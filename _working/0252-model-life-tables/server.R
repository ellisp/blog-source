
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  d <- reactive({
    d <- mlt_raw |>
      filter(family == input$family & e0 == input$e0)
    
    return(d)
  })
  
  output$life_exp <- renderPlot({
    p <- d() |>
      ggplot(aes(x = age, y = mx1, colour = sex)) +
      geom_line() +
      scale_colour_manual(values = pal) +
      labs(x = "Age",
           y = "Death rate",
           colour = "",
           title = glue("Model life table family = {input$family}"),
           subtitle = glue("Life expectancy = {input$e0}")) +
      theme(legend.position = c(0.2, 0.8))
    
    if(input$yscale == "Logarithmic"){
      p <- p + scale_y_log10()
    }

    return(p)
    
    })
}
