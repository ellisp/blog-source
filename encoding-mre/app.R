
library(shiny)


m <- "M\U0101ori"

eth_list <- list(
  Polynesian = c(m, "Tongan"),
  Asian = c("Chinese", "Japanese")
)

ui <- fluidPage(
  pickerInput("ethnicity", "Choose an ethnicity", choices = c("Chinese", m)),
  pickerInput("ethnicity", "Choose an ethnicity", choices = eth_list)  

)


server <- function(input, output) {
   
}

# Run the application 
shinyApp(ui = ui, server = server)

