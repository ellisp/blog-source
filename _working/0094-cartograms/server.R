library(shiny)
library(leaflet)
library(leaflet.extras)
library(nzcensus)

camel_to_english <- function(camelCase){
   return(gsub("([a-z])([A-Z])", "\\1 \\L\\2", camelCase, perl = TRUE))
}

load("comb_data_reg.rda")
load("comb_data_ta.rda")
load("comb_data_au.rda")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   map_data <- reactive({
      if(input$area == "Regional Council"){
         tmp <- reg_cart
      } else {
         if(input$area == "Territorial Authority"){
            tmp <- ta_cart   
         } else {
            tmp <- au_cart
         }
         
      }
      return(tmp)
   })
   
   numeric_data <- reactive({
      if(input$area == "Regional Council"){
         tmp <- comb_data_reg
      } else {
         if(input$area == "Territorial Authority"){
            tmp <- comb_data_ta
         } else {
            tmp <- comb_data_au  
         }
      } 
      return(tmp)
   })
   
   var <- reactive({
      var_name <- paste0("Prop", input$variable, "2013")
      value <- numeric_data()[ , var_name]
      return(value)
   })
   
   the_data <- reactive({
      tmp <- map_data()
      tmp@data[ ,"value"] <- var()
      tmp@data[ , "label"] <-paste0(tmp@data$Name, " ", round(tmp@data$value * 100, 1), "%")
      return(tmp)
   })   
   
   colour_reverse <- reactive({
      return(input$colour_order == "Reverse")
   })
   
   map <- reactive({
      the_data() %>%
      leaflet() %>%
      addPolygons(color = "#444444", weight = 1,
         fillOpacity = 1,
         fillColor = ~colorNumeric(palette = input$colour_scheme, 
                                   domain = range(var(), na.rm = TRUE), 
                                   reverse = colour_reverse())(value),
         label = ~label,
         highlightOptions = highlightOptions(color = "black", weight = 2,
                                             bringToFront = TRUE)) %>%
         addLegend(pal = colorNumeric(input$colour_scheme, range(var(), na.rm = TRUE), reverse = colour_reverse()), 
                   values = range(var(), na.rm = TRUE), bins = 5, opacity = 1, 
                   title = "Percentage",
                   labFormat = labelFormat(
                      transform = function(x){x * 100}, 
                      suffix = "%")
         )  %>%
         addFullscreenControl(pseudoFullscreen = TRUE)
      }) 
   output$myMap <- renderLeaflet(map())
   
   the_title <- reactive({
      tmp <- paste("Proportion", tolower(camel_to_english(input$variable)), "in 2013 Census")
      tmp <- gsub("_", " ", tmp)
      return(tmp)
   })
   
   output$the_title <- renderText(the_title())
})

