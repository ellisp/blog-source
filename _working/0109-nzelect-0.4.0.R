install.packages("nzelect")

library(nzelect)
library(tidyverse)
library(ggplot2)
library(scales)
library(ggrepel)
library(forcats)

  
palette <- c(parties_v, Other = "pink2", `Informal Votes` = "grey")

svg("../img/0109-special.svg", 9, 6)
nzge %>%
  filter(voting_type == "Party") %>%
  mutate(party = fct_lump(party, 5)) %>%
  mutate(dummy = grepl("special", voting_place, ignore.case = TRUE)) %>%
  group_by(electorate, party, election_year) %>%
  summarise(prop_before = sum(votes[dummy]) / sum(votes),
            total_votes = sum(votes)) %>%
  ungroup() %>%
  mutate(party = gsub(" Party", "", party),
         party = gsub("ACT New Zealand", "ACT", party),
         party = gsub("New Zealand First", "NZ First", party)) %>%
  mutate(party = fct_reorder(party, prop_before)) %>%
  ggplot(aes(x = prop_before, y = party, size = total_votes, colour = party)) +
  facet_wrap(~election_year) +
  geom_point(alpha = 0.1) +
  ggtitle("'Special' votes proportion by party, electorate and year",
          "Each point represents the proportion of a party's vote in each electorate that came from special votes") +
  labs(caption = "Source: www.electionresults.govt.nz, collated in the nzelect R package",
       y = "") +
  scale_size_area("Total party votes", label = comma) +
  scale_x_continuous("\nPercentage of party's votes that were 'special'", label = percent) +
  scale_colour_manual(values = palette, guide = FALSE)
dev.off()

svg("../img/0109-before.svg", 9, 6)
nzge %>%
  filter(voting_type == "Party") %>%
  mutate(party = fct_lump(party, 5)) %>%
  mutate(dummy = grepl("before", voting_place, ignore.case = TRUE)) %>%
  group_by(electorate, party, election_year) %>%
  summarise(prop_before = sum(votes[dummy]) / sum(votes),
            total_votes = sum(votes)) %>%
  ungroup() %>%
  mutate(party = gsub(" Party", "", party),
         party = gsub("ACT New Zealand", "ACT", party),
         party = gsub("New Zealand First", "NZ First", party)) %>%
  mutate(party = fct_reorder(party, prop_before)) %>%
  ggplot(aes(x = prop_before, y = party, size = total_votes, colour = party)) +
  facet_wrap(~election_year) +
  geom_point(alpha = 0.1) +
  ggtitle("'Before' votes proportion by party, electorate and year",
          "Each point represents the proportion of a party's vote in each electorate that were cast before election day") +
  labs(caption = "Source: www.electionresults.govt.nz, collated in the nzelect R package",
       y = "") +
  scale_size_area("Total party votes", label = comma) +
  scale_x_continuous("\nPercentage of party's votes that were before election day", label = percent) +
  scale_colour_manual(values = palette, guide = FALSE)
dev.off()

svg("../img/0109-party-cand.svg", 8, 8)
nzge %>%
  group_by(voting_type, party) %>%
  summarise(votes = sum(votes)) %>%
  spread(voting_type, votes) %>%
  ggplot(aes(x = Candidate, y = Party, label = party)) +
  geom_abline(intercept = 0, slope = 1, colour = "grey50") +
  geom_point() +
  geom_text_repel(colour = "steelblue") +
  scale_x_log10("Total 'candidate' votes", label = comma, breaks = c(1, 10, 100, 1000) * 1000) +
  scale_y_log10("Total 'party' votes", label = comma, breaks = c(1, 10, 100, 1000) * 1000) +
  ggtitle("Lots of political parties: total votes from 2002 to 2014",
          "New Zealand general elections") +
  labs(caption = "Source: www.electionresults.govt.nz, collated in the nzelect R package") +
  coord_equal()
dev.off()

convert_pngs("0109")
