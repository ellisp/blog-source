library(tidyverse)
library(nlme)
library(growthrates)
library(Cairo)
library(patchwork)

#-------Our world in data version - country level data only----------
download.file("https://covid.ourworldindata.org/data/ecdc/full_data.csv",
              destfile = "covid_full_data.csv")


covid_country_wid <- read_csv("covid_full_data.csv", col_type = cols())  %>%
  filter(location != "World")

#-----------data prep--------------

deaths <- covid_country_wid %>%
  rename(country = location) %>%
  group_by(country, date) %>%
  summarise(cases = sum(new_deaths),
            c_cases = max(total_deaths)) %>%
  group_by(country) %>%
  arrange(date) %>%
  mutate(growth = cases / lag(c_cases),
         days_since = as.numeric(date - min(date[c_cases >= 10]))) %>%
  filter(days_since >=0 ) %>%
  ungroup()

the_data <- deaths %>%
  filter(country == "United States") %>%
  mutate(lc_cases = log(c_cases),
         dlc_cases = lc_cases - lag(lc_cases))


mod_nls <- try(gnls(lc_cases ~ SSlogis(days_since, Asym, xmid, scal), 
                    data = the_data, correlation = corAR1()))

# asymptote expected between 78k and 230k
ci <- exp(confint(mod_nls, level = 0.8))



#-----------generalized logistic growth---------

# mumax is r in equation 9 of section 4.1
# http://modelosysistemas.azc.uam.mx/texts/sa/logisticmodels.pdf

# Generalized Logistic growth
p <- c(y0 = min(the_data$lc_cases),
       mumax = 0.2, 
       K = max(the_data$lc_cases),
       alpha = 1, beta = 1, gamma = 1)

# caution - these need to be in the same order as p!
lower <- c(y0 = 0, mumax = 0.01, K = max(the_data$lc_cases) - 1, alpha = 0.1, beta = 0.1, gamma = 0.1)
upper <- c(y0 = 5, mumax = 0.5, K = 20, alpha = 1.5, beta = 1.5, gamma = 1.5)

mod_glg <-  fit_growthmodel(FUN = grow_genlogistic, p = p, 
                            time = the_data$days_since, 
                            y = the_data$lc_cases, lower = lower, upper = upper)


mod_glg@fit$par




pred_data <- tibble(days_since = 1:120,
                    lc_cases = c(the_data$lc_cases, rep(NA, 120 - nrow(the_data))))

pred_data <- pred_data %>%
  mutate(pred = exp(predict(mod_nls, newdata = pred_data)),
         new_cases_pred = pred - lag(pred),
         new_cases_actual = exp(lc_cases) - lag(exp(lc_cases))) %>%
  mutate(glg_pred_c = exp(predict(mod_glg, newdata = rename(pred_data, time = days_since))[, 2]),
         glg_pred_d = glg_pred_c - lag(glg_pred_c))

ggplot(pred_data, aes(x = days_since))    +
  geom_line(aes(y = new_cases_pred)) +
  geom_line(aes(y = glg_pred_d), colour = "red") +
  geom_point(aes(y = new_cases_actual)) +
  scale_y_continuous(label = comma) +
  labs(y = "Deaths per day")

ggplot(pred_data, aes(x = days_since, y = pred))    +
  geom_line() +
  geom_line(aes(y = glg_pred_c), colour = "red") +
  geom_point(aes(y = exp(lc_cases))) +
  scale_y_continuous(label = comma) +
  labs(y = "total deaths")

#===========================by state=======================

nyt_states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

nyt_states %>%
  ggplot(aes(x = date, y = deaths, colour = state)) +  geom_line()

nyt_states <- nyt_states %>%
  group_by(state) %>%
  arrange(date) %>%
  mutate(new_deaths = deaths - lag(deaths)) %>%
  mutate(growth = new_deaths / lag(deaths),
         days_since = as.numeric(date - min(date[deaths >= 10]))) %>%
  filter(days_since >=0 )

nyt_states %>%
  ggplot(aes(x = days_since, y = deaths, colour = state)) +
  geom_line() +
  geom_text(data = filter(nyt_states, days_since == max(days_since)),
            aes(label = state), hjust = 0) +
  theme(legend.position = "none") +
  scale_y_log10()

all_states <- unique(nyt_states$state)


the_state <- "New York"

CairoPDF("../img/0175-states.pdf", 11, 8)
for(i in 1:length(all_states)){
  the_state <- all_states[i]

  the_data <- nyt_states %>%
    filter(state == the_state) %>%
    mutate(lc_deaths = log(deaths),
           dlc_deaths = lc_deaths - lag(lc_deaths))
  
  if(nrow(the_data) <= 10){break()}
  
  
  try({
  
    mod_nls <- gnls(lc_deaths ~ SSlogis(days_since, Asym, xmid, scal), 
                        data = the_data, correlation = corAR1())
    
    #-----------generalized logistic growth---------
    
    # mumax is r in equation 9 of section 4.1
    # http://modelosysistemas.azc.uam.mx/texts/sa/logisticmodels.pdf
    
    # Generalized Logistic growth
    p <- c(y0 = min(the_data$lc_deaths),
           mumax = 0.2, 
           K = max(the_data$lc_deaths),
           alpha = 1, beta = 1, gamma = 1)
    
    # caution - these need to be in the same order as p!
    lower <- c(y0 = 0, mumax = 0.01, K = max(the_data$lc_deaths) - 1, alpha = 0.1, beta = 0.1, gamma = 0.5)
    upper <- c(y0 = 5, mumax = 0.5, K = log(2e5), alpha = 5, beta = 5, gamma = 2)
    
    mod_glg <-  fit_growthmodel(FUN = grow_genlogistic, p = p, 
                                time = the_data$days_since, 
                                y = the_data$lc_deaths, lower = lower, upper = upper)
    
    
    mod_glg@fit$par
    
    
    
    
    pred_data <- tibble(days_since = 1:120,
                        lc_deaths = c(the_data$lc_deaths, rep(NA, 120 - nrow(the_data)))) %>%
      mutate(nls_pred_c = exp(predict(mod_nls, newdata = pred_data)),
             nls_pred_d = nls_pred_c - lag(nls_pred_c),
             new_deaths_actual = exp(lc_deaths) - lag(exp(lc_deaths)))%>%
      mutate(glg_pred_c = exp(predict(mod_glg, newdata = rename(pred_data, time = days_since))[, 2])) %>%
      mutate(glg_pred_c = ifelse(is.nan(glg_pred_c), NA, glg_pred_c)) %>%
      tidyr::fill(glg_pred_c) %>%
      mutate(glg_pred_d = glg_pred_c - lag(glg_pred_c))
    
    p1 <- ggplot(pred_data, aes(x = days_since))    +
      geom_line(aes(y = nls_pred_d)) +
      geom_line(aes(y = glg_pred_d), colour = "red") +
      geom_point(aes(y = new_deaths_actual)) +
      scale_y_continuous(label = comma) +
      labs(y = "Deaths per day",
           title = the_state)
    
    p2 <- ggplot(pred_data, aes(x = days_since))    +
      geom_line(aes(y = nls_pred_c)) +
      geom_line(aes(y = glg_pred_c), colour = "red") +
      geom_point(aes(y = exp(lc_deaths))) +
      scale_y_continuous(label = comma) +
      labs(y = "Cumulative total deaths",
           title = the_state)
    
    print(p1 + p2 + 
            p1 + scale_y_log10() + labs(title = "log scale") +
            p2 + scale_y_log10() + labs(title = "log scale"))
  })
}
dev.off()

#=========================world==========================


the_data <- covid_country_wid %>%
  group_by(date) %>%
  summarise(deaths = sum(total_deaths, na.rm = TRUE)) %>%
  mutate(lc_deaths = log(deaths),
         dlc_deaths = lc_deaths - lag(lc_deaths)) %>%
  filter(deaths >= 10) %>%
  mutate(days_since = as.numeric(date - min(date))) %>%
  filter(date < max(date))

mod_nls <- gnls(lc_deaths ~ SSlogis(days_since, Asym, xmid, scal), 
                data = the_data, correlation = corAR1())

# asymptote expected between 135,000 and 1.25m
ci <- exp(confint(mod_nls, level = 0.8))

pred_data <- tibble(days_since = 1:120,
                    lc_deaths = c(the_data$lc_deaths, rep(NA, 120 - nrow(the_data))))

#-----------generalized logistic growth---------

# mumax is r in equation 9 of section 4.1
# http://modelosysistemas.azc.uam.mx/texts/sa/logisticmodels.pdf

# Generalized Logistic growth
p <- c(y0 = min(the_data$lc_deaths),
       mumax = 0.2, 
       K = max(the_data$lc_deaths),
       alpha = 1, beta = 1, gamma = 1)

# caution - these need to be in the same order as p!
lower <- c(y0 = 0, mumax = 0.01, K = max(the_data$lc_deaths) - 1, alpha = 0.1, beta = 0.1, gamma = 0)
upper <- c(y0 = 5, mumax = 0.5, K = 20, alpha = 2, beta = 2, gamma = 2)

mod_glg <-  fit_growthmodel(FUN = grow_genlogistic, p = p, 
                            time = the_data$days_since, 
                            y = the_data$lc_deaths, lower = lower, upper = upper)


mod_glg@fit$par



pred_data <- pred_data %>%
  mutate(nls_pred_c = exp(predict(mod_nls, newdata = pred_data)),
         nls_pred_d = nls_pred_c - lag(nls_pred_c),
         new_deaths_actual = exp(lc_deaths) - lag(exp(lc_deaths)))%>%
  mutate(glg_pred_c = exp(predict(mod_glg, newdata = rename(pred_data, time = days_since))[, 2]),
         glg_pred_d = glg_pred_c - lag(glg_pred_c))

p1 <- ggplot(pred_data, aes(x = days_since))    +
  geom_line(aes(y = nls_pred_d)) +
  geom_line(aes(y = glg_pred_d), colour = "red") +
  geom_point(aes(y = new_deaths_actual)) +
  scale_y_continuous(label = comma) +
  labs(y = "Deaths per day")

p2 <- ggplot(pred_data, aes(x = days_since))    +
  geom_line(aes(y = nls_pred_d)) +
  geom_line(aes(y = glg_pred_c), colour = "red") +
  geom_point(aes(y = exp(lc_deaths))) +
  scale_y_continuous(label = comma) +
  labs(y = "Cumulative total deaths")

print(p1 + p2)
  
