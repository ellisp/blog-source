# Source code for the web tool at XXXXXX
# prep is in the ./_working/0108a-nzes-prep.R script, one folder up from where this server.R resides.


  
  shinyServer(function(input, output, session) {
  #------------Reactive things that change depending on the election year--------------------
  the_data <- reactive({
    if(input$year == 2014){
      tmp <- nzes14
      updatePickerInput(session, "variable", choices = vars14_list)
    } else {
      tmp <- nzes17
      updatePickerInput(session, "variable", choices = vars17_list)
    }
    return(tmp)
  })
  

  #--------------Identify weights, variable for columns, and value for cells---
  the_weight_var <- reactive({
    if(input$weight_type == "Original NZES weights"){
      tmp <- "dwtfin"
    } else {
      tmp <- "calibrated_weight"
    }
    return(tmp)
  })
  
  the_var <- reactive({
    if(input$year == 2014){
      tmp <- names(vars14)[as.character(vars14) == input$variable]
    }
    if(input$year == 2017){
      tmp <- names(vars17)[as.character(vars17) == input$variable]
    }
    return(tmp)
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
  
  #-------------dynamic explanatory text------------------------
  now_showing <- reactive({
    tmp <- paste0("<p>Currently showing <b>",
          input$value,
          "</b> in each cell of the table. ")
    if(input$value == "Percentage"){
      tmp <- paste0(tmp,
                    "The percentages add up to 100 in each ",
                    tolower(str_sub(input$percent_type, end = -2)),
                    ". This is effective for seeing, for a given "
      )
      if(input$percent_type == "Columns"){
        tmp <- paste0(tmp, " answer to '", 
                      input$variable,
                      "', what proportion of people voted for each party.")
      } else {
        tmp <- paste0(tmp, " set of party voters, what proportion answered differently to '", 
                      input$variable,
                      "'.")
      }
      
    }
    
    tmp <- paste0(tmp, "</p>")
    return(tmp)
  })
  
  output$now_showing <- renderText(now_showing())
  
  pop_text <- reactive({
    tmp <- paste0("<p>The survey was drawn from the population of XXXX people on the electoral
                  roll at the time of the ", input$year, " election</p>")
    if(input$year == 2017){
      tmp <- gsub("XXXX", "4,244,355", tmp)
    }
    if(input$year == 2014){
      tmp <- gsub("XXXX", "3,140,417", tmp)
    }
    return(tmp)
  })
  output$pop_text <- renderText(pop_text())
  
  output$app_heading <- renderText(paste0("<h1>Party vote characteristics at the New Zealand General Election ",
                                          input$year, "</h1>"))
  output$table_heading <- renderText(paste0("<h3>", input$variable, "</h3>"))
  
  #-------------------Create the actual table-------------------
   my_table <- reactive({
     validate(need(row_var() %in% names(the_data()) , "Waiting"))
     validate(need(the_var() %in% names(the_data()) , "Waiting"))
     
     tab <- the_data() %>%
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
     validate(need(nrow(my_table()) > 0 , "Waiting"))
     
     tmp <- datatable(my_table(), options = list(dom = 't'), rownames = FALSE) %>%
       formatStyle(names(my_table())[-1], backgroundColor = styleInterval(breaks(), colours()))
     
     return(tmp)
     
   })
   
   output$the_table <- DT::renderDataTable(my_dt())
   
   

})
