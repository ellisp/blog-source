library(tidyverse)
library(coronavirus)
library(nlme)
library(mgcv)
library(AICcmodavg) # for predictSE to work with gls
library(forecast)
library(growthrates)

fr <- function(x, scale = "log"){
  if(scale == "log"){
    x <- exp(x)
  }
  if(x > 1e8){
    tmp <- "everyone"
  } else {
    tmp <- format(signif(x, 3), big.mark = ",", scientific = FALSE)
  }
  return(tmp)
}



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
            cum_cases = max(total_deaths)) %>%
  group_by(country) %>%
  arrange(date) %>%
  mutate(growth = cases / lag(cum_cases),
         days_since = date - min(date[cum_cases >= 10])) %>%
  filter(days_since >=0 )
  
confirmed <- covid_country_wid %>%
  rename(country = location) %>%
  group_by(country, date) %>%
  summarise(cases = sum(new_cases),
            cum_cases = max(total_cases)) %>%
  group_by(country) %>%
  arrange(date) %>%
  mutate(growth = cases / lag(cum_cases),
         days_since = date - min(date[cum_cases >= 100])) %>%
  filter(days_since >=0 )

  
# that famous chart...  
deaths %>%
  ggplot(aes(x = days_since, y = cum_cases, colour = country)) +
  geom_line() +
  geom_text(data = filter(deaths, date == max(date) & 
                            (country == "Australia" | days_since >= 10)), 
            aes(label = country), hjust = 0) +
  scale_x_continuous() +
  scale_y_log10(label = comma) +
  labs(x = "Days since ten deaths",
       y = "Cumulative number of deaths") +
  theme(legend.position = "none")


confirmed %>%
  ggplot(aes(x = days_since, y = cum_cases, colour = country)) +
  geom_line() +
  geom_text(data = filter(confirmed, date == max(date) & 
                            (country == "Australia" | days_since >= 20)), 
            aes(label = country), hjust = 0) +
  scale_x_continuous() +
  scale_y_log10(label = comma) +
  labs(x = "Days since 100 confirmed cases",
       y = "Cumulative number of cases") +
  theme(legend.position = "none")

confirmed %>%
  filter(country != "Diamond Princess") %>%
  ggplot(aes(x = days_since, y = growth, colour = country)) +
  geom_point() +
  geom_text(data = filter(confirmed, date == max(date) & 
                            country != "Diamond Princess" &
                            (country == "Australia" | days_since >= 20)), 
            aes(label = country), hjust = 0) +
  geom_smooth(method = "loess", aes(group = NULL, colour = NULL)) +
  scale_x_continuous() +
  scale_y_log10(label = percent) +
  labs(x = "Days since 100 confirmed cases",
       y = "Growth rate in cases") +
  theme(legend.position = "none")

deaths %>%
  filter(country != "Diamond Princess") %>%
  ggplot(aes(x = days_since, y = growth, colour = country)) +
  geom_point() +
  geom_text(data = filter(confirmed, date == max(date) & 
                            country != "Diamond Princess" &
                            (country == "Australia" | days_since >= 20)), 
            aes(label = country), hjust = 0) +
  geom_smooth(method = "lm", aes(group = NULL, colour = NULL)) +
  scale_x_continuous() +
  scale_y_log10(label = percent) +
  labs(x = "Days since 10 confirmed deaths",
       y = "Growth rate in deaths") +
  theme(legend.position = "none")

#=========================cases==================
#----------------Comparing different growth models----------

all_countries <- sort(unique(confirmed$country))

CairoPDF("../img/0173-all-countries-cases.pdf", 11, 8)
for(the_country in all_countries){
  the_data <- filter(confirmed, country == the_country) %>% 
    ungroup() %>%
    mutate(days_since = as.numeric(days_since))
  
  if(nrow(the_data) >= 10){
    
    # Logistic
    mod_nls <- try(gnls(log(cum_cases) ~ SSlogis(days_since, Asym, xmid, scal), 
                    data = the_data, correlation = corAR1(), 
                    start = list(Asym = log(max(the_data$cum_cases)), xmid = 6, scal = 5)))
    if("try-error" %in% class(mod_nls)){
      mod_nls <- try(nls(log(cum_cases) ~ SSlogis(days_since, Asym, xmid, scal), 
                     data = the_data, start = list(Asym = log(max(the_data$cum_cases)), xmid = 6, scal = 5),
                       control = nls.control(
                       maxiter = 100, warnOnly = TRUE)))
    }
    
    
    
    
    # Exponential
    mod_exp <- gls(log(cum_cases) ~ days_since, data = the_data, correlation = corAR1())
    
    # ARIMA
    y_ts <- with(the_data, ts(cum_cases))
    best_lambda <- BoxCox.lambda(y_ts)
    mod_aa <- auto.arima(y_ts, lambda = best_lambda)
    
    # Generalized Logistic growth
    p <- c(y0 = min(log(the_data$cum_cases)),
           mumax = 0.2, 
           K = max(log(the_data$cum_cases)),
           alpha = 1, beta = 1, gamma = 1)
    
    # caution - these need to be in the same order as p!
    lower <- c(y0 = 0, mumax = 0.05, K = max(log(the_data$cum_cases)) - 1, alpha = 0.5, beta = 0.5, gamma = 0.5)
    upper <- c(y0 = 10, mumax = 0.5, K = 18, alpha = 1.4, beta = 1.4, gamma = 1.4)
    
    mod_glg <-  fit_growthmodel(FUN = grow_genlogistic, p = p, 
                                time = the_data$days_since, 
                                y = log(the_data$cum_cases), lower = lower, upper = upper)
    
    
    
    two_weeks_out <- max(the_data$days_since) + 14
    
    confint_nls <- try(confint(mod_nls, level = 0.8))
    pred_exp <- try(predictSE(mod_exp, newdata = data.frame(days_since = two_weeks_out)))
    
    if(!"try-error" %in% class(confint_nls)){
      
      ci1 <- pmax(max(log(the_data$cum_cases)), 
                  confint_nls[1, ])
      
      ci2 <- pmax(max(log(the_data$cum_cases)), 
                  pred_exp$fit + c(qnorm(0.1), -qnorm(0.1)) * sqrt((pred_exp$se.fit ^ 2 + mod_exp$sigma ^ 2)))
      
      f_aa <- forecast(mod_aa, h = 14, lambda = best_lambda, biasadj = TRUE)
      ci3 <- c(f_aa$lower[14, 1], f_aa$upper[14, 1])
      
      # limit to one billion at most:
      ci1 <- pmin(ci1, log(1e9)) %>% replace_na(log(1e9))
      ci2 <- pmin(ci2, log(1e9)) %>% replace_na(log(1e9))
      ci3 <- pmin(ci3, 1e9) %>% replace_na(log(1e9))
      
      # Must be at least as much as the current cases:
      ci3 <- pmax(max(the_data$cum_cases), ci3)
          
      the_data$predicted_nls <- exp(predict(mod_nls))
      the_data$predicted_exp <- exp(predict(mod_exp))
      the_data$predicted_aa <- fitted(mod_aa)
      the_data$predicted_glg <- exp(log(the_data$cum_cases) + residuals(mod_glg))
      
      preds <- the_data %>%
        select(days_since, 
               `Exponential` = predicted_exp, 
               `Logistic` = predicted_nls, 
               `ARIMA` = predicted_aa,
               `Generalized Logistic` = predicted_glg)  %>%
        gather(growth_type, cum_cases, -days_since)
      
      st0 <- paste0("Estimated eventual cases of ", fr(mod_glg@par[["K"]]), 
                    " with generalized logistic growth model.")
      st1 <- paste0("Estimated eventual cases between ", fr(ci1[1]), " and ", fr(ci1[2]), 
                    " with logistic growth model.")
      st2 <- paste0("Estimated cases in two more weeks (day ", two_weeks_out, ") between ", 
                    fr(ci2[1]), " and ", fr(ci2[2]), " with exponential growth model.")
      st3 <- paste0("Estimated cases in two more weeks (day ", two_weeks_out, ") between ", 
                    fr(ci3[1], "original"), " and ", fr(ci3[2], scale = "original"), 
                    " with ARIMA model.")
      
      p <- ggplot(the_data, aes(x = days_since, y = cum_cases)) +
        geom_point() +
        geom_line(data = preds, aes(colour = growth_type)) +
        scale_y_continuous(label = comma) +
        scale_x_continuous() +
        labs(title = paste("Growth in cumulative cases of COVID-19 in", the_country),
             subtitle = paste(st0, st1, st3, st2, sep = "\n"),
             colour = "Growth model:",
             y = "Cumulative confirmed cases",
             x = "Days since 100 confirmed cases") +
        scale_colour_brewer(palette = "Set1")
      
      print(p)
    }
  }
}
dev.off()




#=====================deaths===============



CairoPDF("../img/0173-all-countries-deaths.pdf", 11, 8)
for(the_country in all_countries){
  print(the_country)
  the_data <- filter(deaths, country == the_country) %>% 
    ungroup() %>%
    mutate(days_since = as.numeric(days_since))
  
  if(nrow(the_data) >= 10){
  
    # Logistic growth
    mod_nls <- try(gnls(log(cum_cases) ~ SSlogis(days_since, Asym, xmid, scal), 
                        data = the_data, correlation = corAR1(), 
                   start = list(Asym = log(max(the_data$cum_cases)), xmid = 15, scal = 5))
                   )
    if("try-error" %in% class(mod_nls)){
      mod_nls <- try(nls(log(cum_cases) ~ SSlogis(days_since, Asym, xmid, scal), 
                         data = the_data, 
                         start = list(Asym = log(max(the_data$cum_cases)), xmid = 15, scal = 5)))
    }
    
    
    # Exponential
    mod_exp <- gls(log(cum_cases) ~ days_since, data = the_data, correlation = corAR1())
    
    # ARIMA
    y_ts <- with(the_data, ts(cum_cases))
    best_lambda <- BoxCox.lambda(y_ts)
    mod_aa <- auto.arima(y_ts, lambda = best_lambda)
    
    
    # Generalized Logistic growth
    p <- c(y0 = min(log(the_data$cum_cases)),
           mumax = 0.2, 
           K = max(log(the_data$cum_cases)),
           alpha = 1, beta = 1, gamma = 1)
    
    # caution - these need to be in the same order as p!
    lower <- c(y0 = 0, mumax = 0.05, K = max(log(the_data$cum_cases)) - 1, alpha = 0.5, beta = 0.5, gamma = 0.5)
    upper <- c(y0 = 10, mumax = 0.5, K = 20, alpha = 1.5, beta = 1.5, gamma = 1.5)
    
    mod_glg <-  fit_growthmodel(FUN = grow_genlogistic, p = p, 
                               time = the_data$days_since, 
                                  y = log(the_data$cum_cases), lower = lower, upper = upper)
    
    two_weeks_out <- max(the_data$days_since) + 14
    
    confint_nls <- try(confint(mod_nls, level = 0.8))
    pred_exp <- try(predictSE(mod_exp, newdata = data.frame(days_since = two_weeks_out)))
    
    if(!"try-error" %in% class(confint_nls)){
      
      
      ci1 <- pmax(max(log(the_data$cum_cases)), 
                  confint_nls[1, ])
      
      ci2 <- pmax(max(log(the_data$cum_cases)), 
                  pred_exp$fit + c(qnorm(0.1), -qnorm(0.1)) * sqrt((pred_exp$se.fit ^ 2 + mod_exp$sigma ^ 2)))
      
      f_aa <- forecast(mod_aa, h = 14, lambda = best_lambda, biasadj = TRUE)
      ci3 <- c(f_aa$lower[14, 1], f_aa$upper[14, 1])
      
      # limit to one billion at most:
      ci1 <- pmin(ci1, log(1e9))
      ci2 <- pmin(ci2, log(1e9))
      ci3 <- pmin(ci3, 1e9)
      
      # Must be at least as much as the current cases:
      ci3 <- pmax(max(the_data$cum_cases), ci3)
      
      the_data$predicted_nls <- exp(predict(mod_nls))
      the_data$predicted_exp <- exp(predict(mod_exp))
      the_data$predicted_aa <- fitted(mod_aa)
      the_data$predicted_glg <- exp(log(the_data$cum_cases) + residuals(mod_glg))
      
      preds <- the_data %>%
        select(days_since, 
               `Exponential` = predicted_exp, 
               `Logistic` = predicted_nls, 
               `ARIMA` = predicted_aa,
               `Generalized Logistic` = predicted_glg)  %>%
        gather(growth_type, cum_cases, -days_since)
      
      st0 <- paste0("Estimated eventual deaths of ", fr(mod_glg@par[["K"]]), 
                    " with generalized logistic growth model.")
      
      st1 <- paste0("Estimated eventual deaths between ", fr(ci1[1]), " and ", fr(ci1[2]), 
                    " with logistic growth model.")
      st2 <- paste0("Estimated deaths in two more weeks (day ", two_weeks_out, ") between ", 
                    fr(ci2[1]), " and ", fr(ci2[2]), " with exponential growth model.")
      st3 <- paste0("Estimated deaths in two more weeks (day ", two_weeks_out, ") between ", 
                    fr(ci3[1], "original"), " and ", fr(ci3[2], scale = "original"), 
                    " with ARIMA model.")
      
      p <- ggplot(the_data, aes(x = days_since, y = cum_cases)) +
        geom_point() +
        geom_line(data = preds, aes(colour = growth_type)) +
        scale_y_continuous(label = comma) +
        scale_x_continuous() +
        labs(title = paste("Growth in cumulative deaths of COVID-19 in", the_country),
             subtitle = paste(st0, st1, st3, st2, sep = "\n"),
             colour = "Growth model:",
             y = "Cumulative deaths",
             x = "Days since 10 deaths") +
        scale_colour_brewer(palette = "Set1")
      
      print(p)
    }
  }
}
dev.off()
