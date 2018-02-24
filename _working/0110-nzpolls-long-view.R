# devtools::install_github("ellisp/nzelect/pkg1")

library(tidyverse)
library(scales)
library(nzelect)
library(forcats)
library(RColorBrewer)
library(viridis)

pollsters <- unique(polls$Pollster)
pollsters <- pollsters[!grepl("Election result", pollsters)]

palette <- c("black", hue_pal()(15))
names(palette) <- c("Election result", pollsters)

p <- polls %>%
  filter(Party %in% c("Labour", "National", "NZ First", "Green")) %>%
  mutate(Party = fct_reorder(Party, -VotingIntention),
         Pollster = fct_reorder(Pollster, VotingIntention, fun = length, .desc = TRUE)) %>%
  ggplot(aes(x = MidDate, y = VotingIntention, colour = Pollster)) +
  facet_wrap(~Party, scales = "free_y") +
  geom_line() +
  geom_text(aes(label = ifelse(Pollster == "Election result", "O", "")), 
            size = 8, colour = "black") +
  scale_y_continuous(label = percent) +
  labs(x = "Polling date",
       y = "Voting intention",
       caption = "Peter's Stats Stuff, http://ellisp.github.io") +
  scale_colour_manual(values = palette) +
  ggtitle("Fifteen years of opinion polls in New Zealand",
          "Intended party vote for the next election (election results shown in black)\nPolling results taken from Wikipedia, compiled in the nzelect R package")

svg("../img/0110-polls.svg", 9, 6)
print(p)
dev.off()

convert_pngs("0110")
