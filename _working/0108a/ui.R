library(shiny)
library(DT)
load("vars.rda")
load("vars_list.rda")

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Party vote characteristics at the New Zealand General Election 2014"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectizeInput("variable", 
                     "Choose a variable", 
                     choices = vars_list,
                     selected = sample(as.character(vars), 1),
                     multiple = FALSE),
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
      p("The population the survey was drawn from was the 3,140,417 people on the electoral roll 
at the time of the 2014 election."),
      
      conditionalPanel("input.value == 'Margin of error'",
                       HTML("<p>The <b>margin of error</b> indicates the uncertainty of a particular number in the table.
Technically, it shows the half width of a 95% confidence interval for the value
in <i>that particular cell</i> of the table, expressed as a percentage of the original value.  They can exceed 100% 
particularly for small estimates, which have more 'upwards' uncertainty than downwards (as zero is the bottom limit). 
In general, the margins of error are very high, showing you should be uncertain
about any particular value.  More sophisticated analysis and collapsing of categories would
yield more useful results for individual values. In the absence of that approach, more attention should be
paid to the broad patterns than to any individual values.</p>")
                       ),
      
      conditionalPanel("input.value == 'Pearson residuals'",
                       HTML("<p>The <b>Pearson residual</b> is a statistical construct giving an indication of how
<i>suprising</i> the value in a particular cell is.  Values over 2 (or under -2) can be interpreted as unusually high (or low) for that
combination of party vote and the other variable, compared to what would be expected if there were no relationship 
between the two questions.  It is defined as (observed - expected) / sqrt(expected).<p>")),
    
    HTML("<p>This cross-tab tool was built by <a href='http://ellisp.github.io'>Peter's Stats Stuff</a> with
data from the <a href='http://www.nzes.org/'>New Zealand Election Study</a> but is not affiliated with that
Study.</p><p>Use at your own risk.</p>
<p><a href='https://github.com/ellisp/ellisp.github.io/tree/source/_working/0108'>Source code is on GitHub.</a></p>
<P>For a more complex multivariate exploration of the probability of voting for different parties
for people with different <i>combinations</i> of variables, check out this web app on <a href ='https://ellisp.shinyapps.io/individual-vote-nzes/'>
         Modelled individual party vote</a>.")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       htmlOutput('the_heading'),
       dataTableOutput('the_table')
    )
  )
))
