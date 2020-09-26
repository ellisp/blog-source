# Define server logic for application that draws a ternary density plot for a Dirichlet distribution
# Peter Ellis 26 September 2020
shinyServer(function(input, output) {
    
    the_data <- reactive({
        alpha = c(input$a1, input$a2, input$a3) 
        d$dens <- gtools::ddirichlet(d[, 1:3], alpha = alpha)
        return(d)
    })
    
    output$d_plot <- renderPlot({
        
        p <- ggplot(the_data(), aes(x = p1, y = p2, z = p3, weight = dens, fill = ..level..)) +
            coord_tern() +
            theme_custom(tern.plot.background = "white", tern.panel.background = "white") +
            theme_hidetitles() +
            theme_showarrows() +
            theme(legend.position = "none",
                  tern.panel.grid.major = element_line(linetype = 1, size = 0.1),
                  tern.panel.grid.major.T = element_line(colour = "steelblue"),
                  tern.panel.grid.major.R = element_line(colour = "steelblue"),
                  tern.panel.grid.major.L = element_line(colour = "steelblue")) +
            stat_density_tern(geom = 'polygon', contour = TRUE, alpha = 0.5) +
            scale_fill_gradient2(low = "white", high = "black") +
            scale_T_continuous(labels = 0:5 * .20) +
            scale_L_continuous(labels = 0:5 * .20) +
            scale_R_continuous(labels = 0:5 * .20) +
            theme(axis.title.x = element_blank())
        
        print(p)

    })

})
