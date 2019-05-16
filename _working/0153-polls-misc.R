library(ozfedelect)
library(tidyverse)
library(lubridate)

#-------------Has Roy Morgan fixed the pro-ALP overestimate----------------

d <- ozpolls %>%
  filter(firm %in% c("Election result", "Roy Morgan", "Newspoll", "Galaxy", "Ipsos", "Essential")) %>%
  filter(preference_type == "Two-party-preferred") %>%
  filter(party == "ALP") 

p1 <- d %>%
  filter(firm != "Election result") %>%
  ggplot(aes(x = intended_vote, colour = firm)) +
  geom_density() +
  facet_wrap(~election_year) +
  labs(x = "Intended two-party-preferred vote for the ALP") +
  ggtitle("Differences in polling firms' estimates of ALP vote by election year")

svglite("../img/0153-morgan-density.svg", 8, 5)
print(p1)
dev.off()

p2 <- d %>%
  filter(firm != "Election result") %>%
  ggplot(aes(x = mid_date, y = intended_vote, colour = firm)) +
  geom_point() +
  facet_wrap(~firm) +
  ggtitle("Roy Morgan polls for the 2019 election are few and late",
          "We can't conclude the ALP overestimate is fixed from these data points.") +
  labs(x = "Survey mid date",
       y = "Intended two-party-preferred vote for the ALP")


svglite("../img/0153-morgan-2019.svg", 8, 5)
print(p2)
dev.off()

#----------How bad is the underdispersion---------
set.seed(123)

d2 <- d %>%
  filter(year(mid_date) == 2019) %>%
  select(mid_date, 
         observed_polls = intended_vote) 

n <- nrow(d2)
reps <- 100000

sims <- matrix(round(rbinom(n * reps, 1000, mean(d2$observed_polls) / 100) / 10),
               ncol = n, nrow = reps)

d4 <- tibble(standard_deviations = apply(sims, 1, sd)) 

p4 <- d4 %>%
  ggplot(aes(x = standard_deviations)) +
  geom_density(fill = "grey", alpha = 0.5) +
  geom_vline(xintercept = sd(d2$observed_polls), colour = "red") +
  annotate("text", x = 0.45, y = 0.5, hjust = 0, label = paste0(
    "Only ", round(mean(d4 < sd(d2$observed_polls)), 3),
    " of simulated poll\nsequences have a standard\ndeviation less than has been\nobserved in the ",
    n, " polls in 2019."
  )) +
  annotate("text", x = 1, y = 1.5, colour = "red", label = "Observed", hjust = 1) +
  labs(x = paste0("Standard deviations of simulated sequences of ", 
                  n, 
                  " surveys with 1,000 respondents, with survey results rounded")) +
  ggtitle("The polls published in 2019 vary somewhat less than they should if random",
          "The amount of variance is surprisingly low, but not impossibly so.")

svglite("../img/0153-herding.svg", 8, 4)
print(p4)
dev.off()

#=========wrap up===============
svgs <- list.files("../img/", pattern = "0153.+\\.svg", full.names = TRUE)
lapply(svgs, svg_googlefonts)

convert_pngs("0153")

thankr::shoulders() %>% 
  mutate(maintainer = str_squish(gsub("<.+>", "", maintainer)),
         maintainer = ifelse(maintainer == "R-core", "R Core Team", maintainer)) %>%
  group_by(maintainer) %>%
  summarise(`Number packages` = sum(no_packages),
            packages = paste(packages, collapse = ", ")) %>%
  arrange(desc(`Number packages`)) %>%
  knitr::kable() %>% 
  clipr::write_clip()

