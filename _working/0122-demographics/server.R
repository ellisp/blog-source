library(tidyverse)
library(scales)
library(gridExtra)    
library(showtext)
library(shinyjs)
library(shiny)
library(frs)
library(scales)

font_add_google("Roboto", "main_font")

showtext.auto()
res <- 150
showtext.opts(dpi = res)



theme_set(theme_grey(8, base_family = "main_font"))
linesize <- 0.6
update_geom_defaults("line", list(size = linesize))
load("french_death_rates_2015.rda")

fertile_ages <- 12:50
n_fertile_ages <- length(fertile_ages)

#' calculate the multiplier of a given set of death rates, other than IMR, needed to get a new set of death
#' rates for a target life expectancy
le_opt <- function(x, age, rate, target){
  n <- length(rate)
  rate[-c(1, n)] <- pmax(pmin(1000, rate[-c(1, n)] * x), 0)
  return(abs(life_expectancy(age, rate) - target))
}

shinyServer(function(input, output, session) {
  observeEvent(input$reset_all, {
    reset("inputs")
  })
  
  sim <- reactive({
    age <- french_death_rates_2015$age
    
    dr <- approx(age, french_death_rates_2015$male, xout = 0:150)
    death_probs_m <- data_frame(age = dr$x, 
                                prob_death = dr$y / 1000,
                                sex = "male")
    # arbitrarily set a male infant death rate:
    death_probs_m[1, "prob_death"] <- input$mimr / 1000
    
    death_probs_m[-c(1, nrow(death_probs_m)), "prob_death"] <- pmin(1, input$death_mult * 
      death_probs_m[-c(1, nrow(death_probs_m)), ]$prob_death)
    
    
    dr <- approx(age, french_death_rates_2015$female, xout = 0:150)
    death_probs_f <- data_frame(age = dr$x, 
                                prob_death = dr$y / 1000,
                                sex = "female")
    
    # arbitrarily set a female infant death rate:
    death_probs_f[1, "prob_death"] <- input$fimr / 1000
  
    death_probs_f[-c(1, nrow(death_probs_f)), "prob_death"] <- pmin(1, input$death_mult * 
      death_probs_f[-c(1, nrow(death_probs_f)), ]$prob_death)
  
    death_probs <- rbind(death_probs_m, death_probs_f) 
    death_probs[death_probs$age == 150, "prob_death"] <- 1

   
    # start an object to store all the life expectancy estimates in it
    all_expectancies <- data_frame(male = life_expectancy(death_probs_m$age, death_probs_m$prob_death * 1000),
                                   female = life_expectancy(death_probs_f$age, death_probs_f$prob_death * 1000))
    
    
    #--------------------number of couples starting out-------------------------
    men <- round(runif(input$starting_n, 0, input$mean_starting_age_m * 2))
    men[men < 0] <- 0
    women <- round(runif(input$starting_n, 0, input$mean_starting_age_f * 2))
    women[women < 0] <- 0
    
    currently_alive = data_frame(age = c(men, women),
                                 sex = rep(c("male", "female"), each = input$starting_n)) %>%
      group_by(age, sex) %>%
      summarise(population = n()) %>%
      mutate(birth_year = 0 - age) %>%
      ungroup()
    
    
    
    all_deaths <- data_frame(age = numeric(), 
                             sex = character(), 
                             birth_year=numeric(), 
                             deaths = integer(), 
                             death_year = numeric(),
                             population = numeric())
    
    all_borns <- data_frame(sex = character(),
                            population = numeric(),
                            birth_year = numeric())
    
    alive_at_end_of_year <- currently_alive %>%
      mutate(year = 0)
    
    ave_child_rate <- input$first_ave_child_rate
    if(is.na(ave_child_rate)){ave_child_rate <- 2}
    
    for(this_year in 1:input$number_years){
      #------------------births------------------------------
      ave_child_rate <- ave_child_rate * rnorm(1, 1 + input$birth_rate_perturb / 100, 
                                               abs(input$birth_rate_perturb / 100))
      
      
      # recalculate the birth rates
      birth_probs = data_frame(age = fertile_ages,
                               prob_birth = dbeta((fertile_ages - 12) / n_fertile_ages, 2, 4) * 
                                 ave_child_rate / n_fertile_ages,
                               # mother's sex, for later joining to other tables:
                               sex = "female") 
      
      
      new_borns <- currently_alive %>%
        inner_join(birth_probs, by = c("age", "sex")) %>%
        mutate(births = rbinom(n(), population, prob_birth),
               # see https://www.scientificamerican.com/article/is-a-pregnant-womans-chan/:
               male = rbinom(n(), births, input$ssr / (input$ssr + 100)),
               female = births - male) %>%
        select(-age, -sex, -population, -birth_year, -prob_birth, -births) %>%
        gather(sex, population) %>%
        group_by(sex) %>%
        summarise(population = sum(population)) %>%
        mutate(age = 0, birth_year = this_year)
      
      all_borns <- new_borns %>%
        select(sex, population, birth_year) %>%
        rbind(all_borns)
      #----------------------deaths----------------------------
      
      death_probs <- death_probs %>%
        mutate(prob_death = pmin(1, prob_death * rnorm(1, 1 + input$death_rate_perturb / 100, 
                                               abs(input$death_rate_perturb / 100))))
      
      death_probs[death_probs$age == 150, "prob_death"] <- 1
      
      new_deaths <- currently_alive %>%
        inner_join(death_probs, by = c("age", "sex")) %>%
        mutate(deaths = rbinom(n(), population, prob_death),
               death_year = this_year) 
      # at this point, new_deaths has all the information on every cohort, including a row for those with no deaths
      pd1 <- death_probs[death_probs$sex == "male", ]$prob_death
      pd2 <- death_probs[death_probs$sex == "female", ]$prob_death
      
      all_expectancies <- rbind(all_expectancies, data_frame(
        male = life_expectancy(age = dr$x, pd1 * 1000),
        female = life_expectancy(age = dr$x, pd2 * 1000)))
      
      #------------------new state-------------------
      currently_alive <- new_deaths %>%
        mutate(population = population - deaths,
               age = age + 1) %>%
        select(-prob_death, -deaths, -death_year) %>%
        rbind(new_borns) 
      
      all_deaths <- new_deaths %>%
        filter(deaths > 0) %>%
        select(age, sex, birth_year, deaths, death_year, population) %>%
        rbind(all_deaths)
      
      alive_at_end_of_year <- currently_alive %>%
        mutate(year = this_year) %>%
        rbind(alive_at_end_of_year) %>%
        filter(population > 0)
      
      alive_f <- sum(currently_alive[currently_alive$sex == "female", "population"])
      alive_m <- sum(currently_alive[currently_alive$sex == "male", "population"])
      if(alive_f < 1 | alive_m < 1){
        break()
      }
      
    }
    
    
    return(list(alive_at_end_of_year = alive_at_end_of_year,
                all_deaths = all_deaths,
                all_borns = all_borns,
                ave_child_rate = ave_child_rate,
                all_expectancies = all_expectancies))
  })
  
  #=====================Presenting results================
  the_plots <- reactive({
    p1 <- sim()$alive_at_end_of_year %>%
      group_by(year, sex) %>%
      summarise(population = sum(population)) %>%
      ggplot(aes(x = year, y = population, colour = sex)) +
      geom_line() +
      scale_y_continuous(label = comma) +
      ggtitle("Population size over time",
              paste0("Starting and finishing children per woman: ", round(input$first_ave_child_rate, 1),
                     " and ", round(sim()$ave_child_rate, 1), ".",
                     "\nDeath rate tendency: ", input$death_rate_perturb, "%")) +
      labs(x = "Year since simulation began", colour = "") +
      theme(legend.position = "none")
    
    p2 <- sim()$alive_at_end_of_year %>%
      filter(year %in% c(0, 20, 40, 75, 120, 180, 250, 350, 500)) %>%
      mutate(year_lab = paste("Year:", year),
             year_lab = fct_reorder(year_lab, year)) %>%
      ggplot(aes(x = age, y = population, colour = sex)) +
      geom_line() +
      facet_wrap(~year_lab, scales = "free_y") +
      ggtitle("Population age and sex distribution over time") +
      labs(x = "Age of population", colour = "") +
      scale_y_continuous("Number of people\n(note that scale varies)", label = comma)
    
    year_breaks <- case_when(
      input$number_years > 500 ~ c(0, 20, 40, 75, 120, 180, 250, 500, 1000),
      input$number_years > 250 ~ c(0, 20, 40, 75, 120, 180, 250, 350, 500),
      TRUE                     ~ c(0, 10, 20, 40, 75, 110, 170, 240, 380)
      
    )
    
    p2ad <- sim()$alive_at_end_of_year %>%
      filter(year %in% year_breaks) %>%
      mutate(year_lab = paste("Year:", year),
             year_lab = fct_reorder(year_lab, year)) %>%
      mutate(age_c = fct_drop(cut(age, breaks = 0:16 * 10, right = FALSE)) ) %>%
      group_by(age_c, year_lab, sex) %>%
      summarise(population = sum(population))
    
    p2a <- p2ad %>%
      ggplot(aes(x = as.numeric(age_c), y = population, fill = sex, colour = sex)) +
      geom_point() +
      geom_area(position= "identity", alpha = 0.3) +
      facet_wrap(~year_lab, scales = "free_y") +
      ggtitle("Population age and sex distribution over time") +
      labs(x = "Age of population", colour = "", fill = "") +
      scale_y_continuous("Number of people per ten year age bracket\n(note that scale varies)") +
      scale_x_continuous(breaks = unique(as.numeric(p2ad$age_c)), 
                         labels = levels(p2ad$age_c)) +
      theme(axis.text.x = element_text(angle = 90))
    
    
    total_pop <- sim()$alive_at_end_of_year %>%
      group_by(year) %>%
      summarise(population = sum(population)) 
    
    total_pop_sex <- sim()$alive_at_end_of_year %>%
      group_by(year, sex) %>%
      summarise(population = sum(population)) 
    
    
    cdr <- sim()$all_deaths %>%
      select(deaths, death_year) %>%
      rename(year = death_year) %>%
      group_by(year) %>%
      summarise(deaths = sum(deaths)) %>%
      left_join(total_pop, by = "year") %>%
      group_by(year) %>%
      summarise(crude_death_rate = sum(deaths) / sum(population) * 1000) 
    
    
    cbr <- sim()$all_borns %>%
      rename(born = population, year = birth_year) %>%
      group_by(year) %>%
      summarise(born = sum(born)) %>%
      left_join(total_pop, by = "year") %>%
      mutate(crude_birth_rate = born / population * 1000)
    
    p3 <- cbr %>%
      select(year, crude_birth_rate) %>%
      left_join(cdr, by = "year") %>%
      gather(variable, value, -year) %>%
      mutate(variable = ifelse(variable == "crude_death_rate", "Deaths", "Births")) %>%
      ggplot(aes(x = year, y = value, colour = variable)) +
      geom_line(size = 0.1) +
      scale_y_continuous("Births and deaths\nper 1,000 population", label = comma) +
      ggtitle("Crude birth and death rates") +
      scale_colour_manual(values = c("darkgreen", "orange")) +
      labs(x = "Year since simulation began", colour = "") +
      theme(legend.position = "right") 
    
    
    ae <- sim()$all_expectancies
    le_heading <- paste0("Starting life expectancy: ", paste(round(head(ae, 1)), collapse = ", "), ";\n",
                         "Finishing life expectancy: ", paste(round(tail(ae, 1)), collapse = ", "), ".")
    
    p4 <- sim()$alive_at_end_of_year %>%
      group_by(year, sex) %>%
      summarise(ave_age = sum(age * population) / sum(population)) %>%
      ggplot(aes(x = year, y = ave_age, colour = sex)) +
      geom_line() +
      labs(y = "Average age", x = "Year since simulation began", colour = "") +
      ggtitle("Average age over time",
              le_heading) +
      theme(legend.position = "none") +
      ggtitle("Average age over time")
    
    
    p5 <- sim()$all_deaths %>%
      group_by(birth_year, sex) %>%
      summarise(average_observed_death_age = sum(age * population) / sum(population)) %>%
      ggplot(aes(x = birth_year, y = average_observed_death_age, colour = sex)) +
      geom_line() +
      labs( x = "Birth year (relative to Year Zero for the simulation)", colour = "") +
      ggtitle("Average age at death",
              "Note that this is not the same as life expectancy at birth except in the middle of the data,\nas recent observations are biased to young deaths; and many deaths before year zero were not observed.")
    
    p6 <- sim()$all_expectancies %>%
      mutate(year = 0:(n() -1)) %>%
      gather(sex, value, -year) %>%
      ggplot(aes(x = year, y = value, colour = sex)) +
      geom_line() +
      labs(x = "Year since simulation began",
           y = "Life expectancy",
           colour = "") +
      ggtitle("Life expectancy at birth")
    
    return(list(p1= p1,
                p2 = p2,
                p2a = p2a,
                p3 = p3,
                p4 = p4,
                p5 = p5,
                p6 = p6
           ))
  })  
  
  output$pyramids <- renderImage({
    # This could be done with just renderPlot() but that doesn't work for fonts.
    # See https://stackoverflow.com/questions/31859911/r-shiny-server-not-rendering-correct-ggplot-font-family
    # So unfortunately we need all this palava
    
    # Read myImage's width and height. These are reactive values, so this
    # expression will re-run whenever they change.
    width  <- session$clientData$output_pyramids_width
    height <- session$clientData$output_pyramids_height
    
    # For high-res displays, this will be greater than 1
    pixelratio <- session$clientData$pixelratio
    
    # A temp file to save the output.
    outfile <- tempfile(fileext='.png')
    
    # Generate the image file
    png(outfile, width = width * pixelratio, height = height * pixelratio,
        res = res * pixelratio)
    
    print(the_plots()$p2a)
    
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         width = width,
         height = height
    )
  }, deleteFile = TRUE)

  
  output$fourplots <- renderImage({
    # This could be done with just renderPlot() but that doesn't work for fonts.
    # See https://stackoverflow.com/questions/31859911/r-shiny-server-not-rendering-correct-ggplot-font-family
    # So unfortunately we need all this palava
    
    # Read myImage's width and height. These are reactive values, so this
    # expression will re-run whenever they change.
    width  <- session$clientData$output_fourplots_width
    height <- session$clientData$output_fourplots_height
    
    # For high-res displays, this will be greater than 1
    pixelratio <- session$clientData$pixelratio
    
    # A temp file to save the output.
    outfile <- tempfile(fileext='.png')
    
    # Generate the image file
    png(outfile, width = width * pixelratio, height = height * pixelratio,
        res = res * pixelratio)
    
    grid.arrange(the_plots()$p1, the_plots()$p3, the_plots()$p4, the_plots()$p6)
    
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         width = width,
         height = height
    )
  }, deleteFile = TRUE)
    
})
