# Source code for the web tool at XXXXXX
# prep is in the ./_working/0108-nzes-prep.R script, one folder up from where this server.R resides.

library(shiny)
library(DT)
library(dplyr)
library(tidyr)

load("nzes.rda")
load("vars.rda")


shinyServer(function(input, output, session) {
  the_weight_var <- reactive({
    if(input$weight_type == "Original NZES weights"){
      tmp <- "dwtfin"
    } else {
      tmp <- "calibrated_weight"
    }
    return(tmp)
  })
  
  the_var <- reactive({
    names(vars)[as.character(vars) == input$variable]
  }) 
  
  row_var <- reactive({
    ifelse(input$hide_n, "partyvote", "partyvote_n")
  })
  
  the_value_var <- reactive({
    if(input$value == "Sample size"){
      tmp <- "unweighted"
    } else if(input$value != "Margin of error"){
      tmp <- "weighted"
    } else {
      tmp <- "rme"
    }
    return(tmp)
  })
  
   my_table <- reactive({
     tab <- nzes %>%
       mutate_("myweight" = the_weight_var()) %>%
       group_by_(the_var(), row_var()) %>%
       summarise(weighted = sum(myweight),
                 unweighted = n()) %>%
       ungroup()
     
     if(input$value == "Percentage"){
       if(input$percent_type == "Columns"){
         tab <- tab %>%
           group_by_(the_var()) %>%
           mutate(weighted = weighted / sum(weighted) * 100)
           
       } else {
         tab <- tab %>%
           group_by_(row_var()) %>%
           mutate(weighted = weighted / sum(weighted) * 100)
       }
     }
     
     if(input$value == "Margin of error"){
       tab <- tab %>%
         ungroup() %>%
         mutate(p = pmax(0.05, weighted / sum(weighted)),
                rme = round(1.96 * sqrt(p * (1 - p) / pmax(1, unweighted)) / p * 100))
         
     }
     
     tab <- tab %>%
       mutate(weighted = round(weighted)) %>%
       select_(the_var(), row_var(), the_value_var()) %>%
       spread_(the_var(), the_value_var(), fill = 0) %>%
       rename_("Party vote" = row_var())
     
     # Finally, if they want Pearson residuals, we turn this table of people counts into something else:
     if(input$value == "Pearson residuals"){
       mat <- as.matrix(tab[ ,-1])
       tab[ ,-1] <- round(chisq.test(mat)$residuals, 1)
     
     }
     
     return(tab)
   })
   
   colours <- reactive({
     n <- length(breaks()) + 1
     if(input$value %in% c("Pearson residuals", "Percentage")){
       col_m <- colorRamp(c("orange", "white", "cyan"))(1:n / n)  
     } else {
       col_m <- colorRamp(c("white", "yellowgreen"))(1:n / n)  
     }
     
     cols <- apply(round(col_m), 1, function(x){paste0("rgb(", paste(x, collapse = ","), ")")})
     return(cols)
   })
   
   breaks <- reactive({
     if(input$value == "Pearson residuals"){
       brks <- quantile(-5:5, probs = seq(.05, .95, .05))
       } else {
            brks <- quantile(as.data.frame(my_table())[ ,-1], probs = seq(.05, .95, .05), na.rm = TRUE) 
         }
       
     
     return(brks)
   })
  
   my_dt <- reactive({
     datatable(my_table(), options = list(dom = 't')) %>%
       formatStyle(names(my_table())[-1], backgroundColor = styleInterval(breaks(), colours()))
     
   })
   
   output$the_table <- renderDataTable(my_dt())
   
   output$the_heading <- renderText(paste0("<h3>", input$variable, "</h3>"))

})
