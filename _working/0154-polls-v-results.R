library(ozfedelect)
library(tidyverse)
library(scales)

elections <- ozpolls %>%
  filter(firm == "Election result" &
           preference_type == "Two-party-preferred" &
           party == "ALP") %>%
  mutate(election_year = election_year - 3) %>%
  select(election_year, 
         election_date = mid_date, 
         actual_vote = intended_vote) %>%
  rbind(tibble(
    election_year = 2019, 
    election_date = as.Date("2019-05-18"),
    actual_vote = 49.11
  ))


p <- ozpolls %>%
  filter(firm != "Election result"  &
           preference_type == "Two-party-preferred" &
           party == "ALP") %>%
  left_join(elections, by = "election_year") %>%
  mutate(days_away_election = as.numeric(election_date - mid_date),
         alp_overestimate = intended_vote - actual_vote) %>%
  filter(days_away_election < 28 & days_away_election > 0) %>% 
  ggplot(aes(x = mid_date, y = alp_overestimate)) +
  theme(panel.grid = element_blank()) +
  geom_hline(yintercept = 0, colour = "grey") +
  geom_smooth(se = FALSE, method = "gam") +
  geom_point(aes(colour = firm)) +
  facet_wrap(~election_year, scales = "free_x", nrow = 1) +
  ggtitle("2019 polls overestimated the ALP result somewhat more than usual",
          "Showing polls in the 28 days up to the election. 2019 result is provisional, at 16 hours.") +
  labs(x = "Polling date",
       y = "Survey voting 2pp intention minus result on the day",
       colour = "",
       caption = "freerangestats.info")

svglite("../img/0154-polls.svg", 8, 5)
print(p)
dev.off()

#=========wrap up===============
svgs <- list.files("../img/", pattern = "0154.+\\.svg", full.names = TRUE)
lapply(svgs, svg_googlefonts)

convert_pngs("0154")
  