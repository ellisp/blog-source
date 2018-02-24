library(shiny)
library(ggvis)
library(scales)
library(ranger)
library(dplyr)
library(nnet)

load("models.rda")
load("nzes_skeleton.rda")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   the_probs <- reactive({
      
      x <- nzes_skeleton
      
      x$dage <- input$dage
      x$NotEuropean <- 1 - 1 * input$European
      x$Maori <- 1 * input$Maori
      x$HHIncome <- input$HHIncome
      x$OwnHouseOrFlat <- 1 * input$OwnHouseOrFlat
      x$HHMemberTradeUnion <- 1 * input$HHMemberTradeUnion
      x$HHMemberProfAssoc <- 1 * input$HHMemberProfAssoc
      x$NZBorn <- 1 * input$NZBorn
      x$Religion <- input$Religion
      x$Marital <- 1 * input$Marital
      x$HighestQual <- input$HighestQual
      x$IdentifyWorkingClass <- 1 * input$IdentifyWorkingClass
      x$WorkStatus <- input$WorkStatus
      x$Student <- 1 * input$Student
      x$SuperviseAnyone <- 1 * input$SuperviseAnyone
      x$City <- 1 * input$City
      x$Male <- 1 * input$Male
      
      
      tmp1 <- as.vector(predict(mod_rr, data = x))$predictions
      tmp2 <- predict(mod_mn, newdata = x, type = "probs")
      
      tmp3 <- data.frame(
         party = colnames(tmp1),
         prob1 = as.numeric(tmp1),
         prob2 = as.numeric(tmp2)
      ) %>%
         mutate(prob = (prob1 + prob2) / 2)
      return(tmp3)
   })
   
   tool_func <- function(x){
      paste0("<p>", x$party, " ", round(x[[4]] * 100), "%</p>")
   }
   
   # for some reason the scale_ordinal doesn't work with shiny, only in interactive mode
   the_probs %>%
      ggvis(x = ~party, y = ~prob, fill = ~party) %>%
      layer_bars(opacity := 0.5) %>%
      add_tooltip(tool_func) %>%
      scale_ordinal('fill', 
                    range = c("lightgrey", "green", "red",  "blue", "black", "lightgrey")) %>%
      hide_legend('fill') %>%
      add_axis('x', title = "") %>%
      add_axis('y', title = "Modelled probability of voting", title_offset = 50) %>%
      bind_shiny("distPlot")

})
