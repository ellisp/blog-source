

devtools::install_github("ellisp/nzelect/pkg1")
library(nzelect)
library(tidyverse)
library(scales)
library(lubridate)
library(gridExtra)
library(broom)
library(mgcv)
library(gratia)

#-----------------------------polling overview---------------------

p1 <- polls %>%
  filter(MidDate > as.Date("2014-11-20") & !is.na(VotingIntention)) %>%
  filter(Party %in% c("National", "Labour")) %>%
  mutate(Party = fct_reorder(Party, VotingIntention, .desc = TRUE),
         Party = fct_drop(Party)) %>%
  ggplot(aes(x = MidDate, y = VotingIntention, colour = Party, linetype = Pollster)) +
  geom_line(alpha = 0.5) +
  geom_point(aes(shape = Pollster)) +
  geom_smooth(aes(group = Party), se = FALSE, colour = "grey15", span = .4) +
  scale_colour_manual(values = parties_v, guide = "none") +
  scale_y_continuous("Voting intention", label = percent) +
  scale_x_date("") +
  facet_wrap(~Party, scales = "fixed") +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none") 

p2 <- polls %>%
  filter(MidDate > as.Date("2014-11-20") & !is.na(VotingIntention)) %>%
  filter(Party %in% c("Green", "NZ First")) %>%
  mutate(Party = fct_reorder(Party, VotingIntention, .desc = TRUE),
         Party = fct_drop(Party)) %>%
  ggplot(aes(x = MidDate, y = VotingIntention, colour = Party, linetype = Pollster)) +
  geom_line(alpha = 0.5) +
  geom_point(aes(shape = Pollster)) +
  geom_smooth(aes(group = Party), se = FALSE, colour = "grey15", span = .4) +
  scale_colour_manual(values = parties_v, guide = "none") +
  scale_y_continuous("Voting intention", label = percent) +
  scale_x_date("") +
  facet_wrap(~Party, scales = "fixed") +
  theme(panel.grid.minor = element_blank()) 

CairoSVG("../img/0144-four-parties.svg", 8, 6)
grid.arrange(p1, p2)
dev.off()



#----------------long run-------------
elections <- polls %>%
  filter(Pollster == "Election result") %>%
  distinct(MidDate) %>%
  rename(start_date = MidDate) %>%
  mutate(end_date = lead(start_date, default = as.Date("2020-10-01")),
         pm = c("Labour", "Labour", "National", "National", "National", "Labour"))

p3 <- polls %>%
  as_tibble %>%
  filter(!is.na(VotingIntention)) %>%
  filter(Party %in% c("Green", "Labour")) %>%
  select(Party, MidDate, VotingIntention, Pollster) %>%
  spread(Party, VotingIntention) %>%
  mutate(Combined = Labour + Green) %>%
  gather(Party, VotingIntention, -MidDate, -Pollster) %>%
  mutate(Party = fct_relevel(Party, c("Combined", "Labour"))) %>%
  ggplot() +
  geom_rect(data = elections, ymin = -Inf, ymax = Inf, alpha = 0.1, 
            aes(xmin = start_date, xmax = end_date, fill = pm)) +
  geom_line(aes(x = MidDate, y = VotingIntention, colour = Party)) +
  scale_colour_manual(values = c(parties_v, "Combined" = "black")) +
  scale_fill_manual(values = parties_v) +
  labs(x = "Survey date", y = "Voting intention", 
       colour = "Surveyed voting intention:", fill = "Prime Minister's party:") +
  scale_y_continuous(label = percent_format(accuracy = 2)) +
  ggtitle("Voting intention in New Zealand over a longer period than usually presented",
          "Labour, Greens, and combined")
  

CairoSVG("../img/0144-lab-green-comb.svg", 8, 6)
print(p3)
dev.off()

#------------------------seasonality of govt support------------------
# data frame of voting intention for the lead party of the government 
d <- polls %>%
  as_tibble() %>%
  left_join(elections, by = c("MidDate" = "start_date")) %>%
  select(-(WikipediaDates:EndDate)) %>%
  fill(pm, end_date) %>%
  # limit to the party that has the Prime Minister:
  filter(Party == pm) %>%
  mutate(survey_month = as.character(month(MidDate, label = TRUE)),
         survey_month = fct_relevel(survey_month, "Jun"),
         election_month = (month(MidDate) == month(end_date) & year(MidDate) == year(end_date)))

p4 <- ggplot(d) +
  geom_rect(data = elections, ymin = -Inf, ymax = Inf, alpha = 0.1, 
           aes(xmin = start_date, xmax = end_date, fill = pm)) +
  geom_line(aes(x = MidDate, y = VotingIntention, colour = Party, group = end_date)) +
  scale_colour_manual(values = c(parties_v, "Combined" = "black")) +
  scale_fill_manual(values = parties_v) +
  labs(x = "Survey date", y = "Voting intention for PM's party", 
       colour = "Surveyed voting intention:", fill = "Prime Minister's party:") +
  scale_y_continuous(label = percent_format(accuracy = 2)) +
  ggtitle("Voting intention for the Prime Minister's party")
  

CairoSVG("../img/0144-pro-pm.svg", 8, 6)
print(p4)
dev.off()

# model of vote for govt's lead party, controlling for trend (the smooth term) and for which
# party is in government, looking for effects from month of year.
mod <- gam(VotingIntention ~ election_month + survey_month +  Party + s(as.numeric(MidDate)), data = d)
anova(mod)

p5 <- tibble(variable = names(coef(mod)),
       estimate = coef(mod),
       se = summary(mod)$se) %>%
  # very approximate confidence intervals:
  mutate(lower = estimate - se * 1.96,
         upper = estimate + se * 1.96) %>%
  filter(grepl("month", variable)) %>%
  mutate(survey_month = gsub("survey_month", "", variable)) %>%
  mutate(survey_month = factor(survey_month, 
                               levels = c("election_monthTRUE", as.character(month(c(7:12, 1:6), label = TRUE))))) %>%
  ggplot(aes(x = lower, xend = upper, y = survey_month, yend = survey_month)) +
  geom_vline(xintercept = 0, colour = "red") +
  geom_segment(size = 3, colour = "steelblue", alpha = 0.7) +
  geom_point(aes(x = estimate)) +
  scale_x_continuous(label = percent) +
  labs(x = "Estimated impact (in percentage points) on intended vote for PM's party, relative to June",
       y = "Survey month") +
  ggtitle("Seasonality of voting preference for the New Zealand PM's party?",
          "Weak evidence of a very weak pro-government effect from late Spring to early Autumn")


CairoSVG("../img/0144-seasonality.svg", 8, 6)
print(p5)
dev.off()


convert_pngs("0144")
