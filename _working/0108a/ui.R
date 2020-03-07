
shinyUI(fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  
  use_googlefont("Prosto One"),
  use_googlefont("Sarala"),
  
  # Application title
  htmlOutput('app_heading'),
  
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      radioButtons("year", "Election year", choices = c(2014, 2017), selected = 2017, inline = TRUE),
      pickerInput("variable", 
                  "Choose a variable", 
                  choices = vars17_list,
                  options = list(
                    `live-search` = TRUE
                  )),
      radioButtons("value", "Choose to show",
                  c("Number (thousands of people)", "Percentage", "Pearson residuals", "Sample size"),
                  selected = "Percentage"),
      conditionalPanel("input.value == 'Percentage'",
                       radioButtons("percent_type", "Show percentages that add up to 100 for:",
                                   c("Rows", "Columns"),
                                   selected = "Rows")),
      checkboxInput("hide_n", "Hide sample size from party names", value = TRUE),
      radioButtons("weight_type", "Choose which survey weights to use",
                  c("Calibrated to party vote totals", "Original NZES weights"),
                  select = "Calibrated to party vote totals"),
      
      
      htmlOutput("pop_text"),
      
      
    
    HTML("<p>This cross-tab tool was built by <a href='http://freerangestats.info'>Free Range Statistics</a> with
data from the <a href='http://www.nzes.org/'>New Zealand Election Study</a> but is not affiliated with that
Study.</p><p>Use at your own risk.</p>
<p><a href='https://github.com/ellisp/blog-source/tree/master/_working/0108a'>Source code is on GitHub.</a></p>
<P>This webpage is only a side project and might lack a little polish. My day job is as Chief Data Scientist at 
         <a href='http://nousgroup.com.au'>Nous Group</a>,
         Australia's largest home-grown management consultancy firm.</p>")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       htmlOutput('table_heading'),
       DT::dataTableOutput('the_table'),
       hr(),
       htmlOutput('now_showing'),
       conditionalPanel("input.value == 'Pearson residuals'",
                        HTML("<p>The <b>Pearson residual</b> is a statistical construct giving an indication of how
<i>suprising</i> the value in a particular cell is.  Values over 2 (or under -2) can be interpreted as unusually high (or low) for that
combination of party vote and the other variable, compared to what would be expected if there were no relationship 
between the two questions.  It is defined as (observed - expected) / sqrt(expected).<p>"))
    )
  )
))
