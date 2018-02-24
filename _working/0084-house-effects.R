library(nzelect)
library(mgcv)
library(tidyverse)
library(scales)
library(magrittr)
library(forcats)
library(RColorBrewer)


#=====================data prep=======================

# vector of colours to use in graphics
house_colours <- c("black", brewer.pal(3, "Set1"))
names(house_colours) <-   c("Election result", "Reid Research", "Colmar Brunton", "Roy Morgan")

# vector of just the seven main parties to use
parties <- polls %>%
   filter(ElectionYear == 2017) %>%
   distinct(Party) %>%
   filter(!Party %in% c("Destiny", "Progressive", "Mana", "Conservative")) %$%
   Party


#===============introductory graphic========================
election_dates <- polls %>%
   filter(Pollster == "Election result") %>%
   select(MidDate) %>%
   distinct()

d1 <- polls %>%
   filter(Party %in% parties) %>%
   filter(Pollster %in% c("Reid Research", "Colmar Brunton", "Roy Morgan", "Election result")) %>%
   mutate(Party = fct_reorder(Party, VotingIntention, .desc = TRUE),
          Pollster = fct_relevel(Pollster, "Election result")) 

p1 <- d1 %>%
   ggplot(aes(x = MidDate, y = VotingIntention, colour = Pollster)) +
   geom_vline(xintercept = as.numeric(election_dates$MidDate), colour = "orange") +
   geom_line(alpha = 0.4) +
   geom_smooth(data = filter(d1, Pollster != "Election result"), span = .3, se = FALSE) +
   geom_line(data = filter(d1, Pollster == "Election result"), size = 1, alpha = 0.5) +
   geom_point(data = filter(d1, Pollster == "Election result"), size = 2) +
   scale_y_continuous("Voting intention", label = percent) +
   scale_x_date("") +
   labs( colour = "")   +
   scale_colour_manual(values = house_colours) +
   ggtitle("Survey versus actual performance in New Zealand voting behaviour",
           "New Zealand First seems systematically underestimated; Greens perhaps overestimated.") +
   labs(caption = "Source: polls data collected by Wikipedia, available in the {nzelect} R package")

svg("../img/0084-straight-polls-1.svg", 12, 7)
p1 +
   facet_wrap( ~ Party, scales = "free") +
   theme(legend.position = c(0.7, 0.15)) 
dev.off()

#=============estimate and present house "bias"=============

house_bias <- function(elect_years, pollsters){
   # depends on these objects being in environmenet:
   # polls, house_colours, parties
   
   houses <- expand.grid(elect_years, pollsters)
   names(houses) <- c("ElectionYear", "Pollster")
   
   for(j in 1:length(parties)){
      the_party = parties[j]
      
      # election results:
      results <- polls %>%
         filter(ElectionYear %in% elect_years & ElectionYear != 2002) %>%
         filter(Pollster == "Election result")  %>%
         filter(Party == the_party) 
      
      
      for(i in 1:length(elect_years)){
         
         # Note we include *all* pollsters in the data for fitting the model
         thedata <- polls %>%
            filter(ElectionYear == elect_years[i] & Pollster != "Election result") %>%
            filter(Party == the_party)
         
         mod <- gam(VotingIntention ~ s(as.numeric(MidDate)) + Pollster, 
                    family = "quasibinomial", data = thedata)
         
         # for predicting values, we only take the pollsters we have an interest in:
         preddata <- data.frame(MidDate = as.numeric(results[i, "MidDate"]), Pollster = pollsters)
         
         # house effect is shown by the amount the predicted value from polling
         # is *more* than the actual vote.  So a positive score means the poll
         # overestimated the actual vote:
         houseeffects <- predict(mod, newdata = preddata, type = "response") -
            results[i, "VotingIntention"]
         houses[houses$ElectionYear == elect_years[i], the_party] <- houseeffects
      }
   
   }   
   
   p <- houses %>%
      gather(Party, `Polling overestimate`, -ElectionYear, -Pollster) %>%
      ggplot(aes(x = ElectionYear, y = `Polling overestimate`, colour = Pollster)) +
      geom_hline(yintercept = 0, colour = "black") +
      geom_point() +
      geom_line() +
      facet_wrap(~Party, ncol = 4) +
      scale_colour_manual(values = house_colours) +
      scale_x_continuous("Election year", breaks = c(2005, 2008, 2011, 2014), limits = c(2004, 2015)) +
      scale_y_continuous(label = percent) +
      theme(legend.position = c(0.9, 0.18)) +
      ggtitle("Statistical forecast of election compared to actual result",
              "Forecasts use time series methods based on pollsters' results, are not actual pollsters' forecasts") +
      labs(caption = "Source: polls data collected by Wikipedia, available in the {nzelect} R package")
   
   print(p)
   
   houses_av <- houses %>%
      gather(Party, Bias, -ElectionYear, -Pollster) %>%
      group_by(Party, Pollster) %>%
      summarise(Bias = mean(Bias))
   
   return(houses_av)
}
   
svg("../img/0084-house1.svg", 8, 5)
   hb1 <- house_bias(elect_years = c(2005, 2008, 2011, 2014),
              pollsters   = c("Colmar Brunton", "Roy Morgan"))      
dev.off()

svg("../img/0084-house2.svg", 8, 5)
   hb2 <- house_bias(elect_years = c(2011, 2014),
           pollsters    = c("Reid Research", "Colmar Brunton", "Roy Morgan"))      
dev.off()

# table for blog post:
house_effects <- hb2 %>%
   filter(Pollster == "Reid Research") %>%
   rbind(hb1) %>%
   arrange(Party, Pollster)
   
house_effects %>%
   mutate(Bias = paste0(round(Bias * 100, 1), "%")) %>%
   spread(Pollster, Bias) %>%
   knitr::kable(align = "lrrr")

save(house_effects, file = "../data/house_effects.rda")

#===================how many polls per year==========================
svg("../img/0084-polls-year.svg", 8, 7)
polls %>%
   select(ElectionYear, Pollster, MidDate) %>%
   distinct() %>%
   group_by(ElectionYear, Pollster) %>%
   summarise(Polls = n()) %>%
   ungroup() %>%
   mutate(Pollster = fct_reorder(Pollster, Polls)) %>%
   ggplot(aes(x = Polls, y = Pollster, colour = as.factor(ElectionYear))) +
   geom_point() +
   facet_wrap(~ElectionYear) +
   theme(legend.position = "none")
dev.off()

#============conver to PNGs==============

convert_pngs("0084")
