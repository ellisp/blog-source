library(ozfedelect)
library(tidyverse)
library(lubridate)

#----------How bad is the underdispersion---------

d <- ozpolls %>%
  filter(firm %in% c("Election result", "Roy Morgan", "Newspoll", "Galaxy", "Ipsos", "Essential")) %>%
  filter(preference_type == "Two-party-preferred") %>%
  filter(party == "ALP") 



set.seed(123)

d2 <- d %>%
  filter(mid_date < as.Date("2016-07-02") & mid_date > as.Date("2016-06-02")) %>%
  #filter(mid_date < as.Date("2013-09-07") & mid_date > as.Date("2013-08-07")) %>%
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
    n, " polls before the election."
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

