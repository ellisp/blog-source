library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(showtext) # for fonts
library(forecast)

font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(base_family = "myfont"))

#-------------------how good is auto.arima at picking order of the ARMA process------------
arima_fit_sim <- function(model, 
                          correct_params, 
                          n = 2 ^ (1:10) * 10, 
                          reps = 10,
                          stepwise = FALSE,
                          verbose = TRUE){
   
   # function that creates @reps number of simulated ARIMA models of type @model and 
   # length @n.  @correct_params should be a vector of the correct parameters eg
   # c("ar1", "ma1", "ma2") for ARMA(1,2)
   # peter.ellis2013nz at gmail com 25 September 2015
   
   require(forecast)
   if(verbose){require(dplyr)}
   
   results <- data.frame(n = rep(n, each = reps),
                      correct = FALSE,
                      fitted = "",
                      stringsAsFactors = FALSE)
   counter <- 0

   for(j in 1:length(n)){
      if(verbose){message(n[j])}
      for(i in 1:reps){
         counter <- counter + 1
         true_series <- arima.sim(model, n = n[j])
         fit <- auto.arima(true_series, stepwise = stepwise)
         results[counter, "fitted"] <- paste(names(fit$coef), collapse = " ")
         if(length(coef(fit)) == length(correct_params) && sum(names(fit$coef) %in% correct_params) == length(correct_params) ){
            results[counter, "correct"] <- TRUE
            
         }   else {
            results[counter, "correct"] <- FALSE
         }
         
      }
   }
   
   if(verbose){
      print(
         results %>%
            group_by(n) %>%
            summarise(correct = sum(correct) / length(correct) * 100)
         )
   }

   return(results)
   }


our_reps <- 500

results_ar1 <- arima_fit_sim(model = list(ar = c(0.5)),
                             correct_params = c("ar1"), 
                             reps = our_reps)

results_ma1 <- arima_fit_sim(model = list(ma = c(0.5)),
                             correct_params = c("ma1"), 
                             reps = our_reps)


results_arma22 <- arima_fit_sim(model = list(ar = c(0.5, -0.2), ma = c(-0.3, 0.2)),
                                correct_params = c("ar1", "ar2", "ma1", "ma2"), 
                                reps = our_reps)

results_arma33 <- arima_fit_sim(model = list(ar = c(0.5, -0.2, 0.3), ma = c(-0.3, 0.2, 0.4)),
                                correct_params = c("ar1", "ar2", "ar3", "ma1", "ma2", "ma3"), 
                                reps = our_reps)


save(results_ar1, results_ma1, results_arma22, results_arma33, file = "_output/0012_sim_results.rda")


#================summary plot====================

r1 <- results_ar1 %>%
   mutate(model = "AR(1)")

r2 <- results_ma1 %>%
   mutate(model = "MA(1)")

r3 <- results_arma22 %>%
   mutate(model = "ARMA(2, 2)")

r4 <- results_arma33 %>%
   mutate(model = "ARMA(3, 3)")

r_all <- rbind(r1, r2, r3, r4) %>%
   mutate(model = factor(model, 
                         levels = c("AR(1)", "MA(1)", "ARMA(2, 2)", "ARMA(3, 3)")))

p1 <- r_all %>%
   group_by(model, n) %>%
   summarise(correct = sum(correct) / length(correct)) %>%
   ggplot(aes(x = n, y = correct, colour = model)) +
   geom_point() +
   geom_line() +
   scale_y_continuous("Percent of models correctly identified", label = percent) +
   scale_x_log10("Length of time series", 
                # label = comma, 
                breaks = 2 ^ (1:10) * 10) +
   ggtitle("Success rate of automated ARIMA model selection\nfitting to a known data generating process")

svg("../img/0012-results.svg", 7, 4)
   print(p1)
dev.off()

png("../img/0012-results.png", 700, 400, res = 100)
   print(p1)
dev.off()

svg("../img/0012-results-faceted.svg", 8, 7)
   print(p1 + 
            facet_wrap(~model, scales = "free_y") +
            theme(legend.position = "none"))
dev.off()
#=======================exporting tables to the actual post========
library(xtable)
options(xtable.type = "html")
options(xtable.include.rownames = FALSE)

results_ar1 %>%
   group_by(fitted) %>%
   summarise(count = length(fitted)) %>%
   arrange(-count) %>%
   head(10) %>%
   xtable() %>%
   print(file = "../_tables/0012-tab1.html")

results_ar1 %>%
   group_by(n) %>%
   summarise(correct = sum(correct) / length(correct) * 100) %>%
   xtable(digits = c(0, 0, 1)) %>%
   print(file = "../_tables/0012-tab2.html")

results_ma1 %>%
   group_by(n) %>%
   summarise(correct = sum(correct) / length(correct) * 100) %>%
   xtable(digits = c(0, 0, 1)) %>%
   print(file = "../_tables/0012-tab4.html")

results_arma33 %>%
   group_by(fitted) %>%
   summarise(count = length(fitted)) %>%
   arrange(-count) %>%
   xtable() %>%
   print(file = "../_tables/0012-tab3.html")


ntables <- 3
alltables <- paste0("../_tables/0012-tab", 1:ntables, ".html")

thispost <- "2015-09-30-autoarima-success-rates.html"

# read in the human-edited post from the _knitr directory
tmp0 <- readLines(paste0("../_knitr/", thispost))

for(i in 1:ntables){
   thistable <- alltables[i]
   tmp1 <- paste(readLines(thistable), collapse ="\n")
   tmp0 <- gsub(thistable, tmp1, tmp0, fixed = TRUE)
}
writeLines(tmp0, con =(paste0("../_posts/", thispost)))
