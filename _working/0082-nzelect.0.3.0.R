library(tidyverse)
library(scales)
library(nzelect)
library(forcats)
library(stringr)
library(rvest)
library(magrittr)

#================2014 theshold scenarios=========
# Party vote results from the 2014 election
results <- GE2014 %>%
   mutate(VotingType = paste0(VotingType, "Vote")) %>%
   group_by(Party, VotingType) %>%
   summarise(Votes = sum(Votes, na.rm = TRUE)) %>%
   spread(VotingType, Votes) %>%
   select(Party, PartyVote) %>%
   ungroup() %>%
   filter(!is.na(PartyVote)) %>%
   filter(Party != "Informal Party Votes") %>%
   arrange(desc(PartyVote)) %>%
   mutate(Party = gsub(" Party", "", Party),
          Party = gsub("New Zealand First", "NZ First", Party),
          Party = gsub("Internet MANA", "Mana", Party)) %>%
   left_join(parties_df[, c("Shortname", "Colour")], by = c("Party" = "Shortname"))

# Electorate seats from the 2014 election
electorate = c(41, 27, 0, 
               0, 0, 0, 
               1, 1, 0,
               1, 0, 0,
               0,0,0)

# Seats awarded under different scenarios:
res2014 <- data_frame(
   party = results$Party,
   `5% threshold\n(actual result)` = 
      allocate_seats(results$PartyVote, electorate = electorate)$seats_v,
   `3.25% threshold like Israel\n(hypothetical result)` = 
      allocate_seats(results$PartyVote, electorate = electorate, threshold = 0.0325)$seats_v,
   `No threshold\n(hypothetical result)` = 
      allocate_seats(results$PartyVote, electorate = electorate, threshold = 0)$seats_v
) %>%
   gather(scenario, seats, -party) %>%
   mutate(party = str_wrap(party, 15),
          party = fct_reorder(party, -seats, fun = mean),
          scenario = fct_relevel(scenario, "5% threshold\n(actual result)"),
          party = fct_other(party, keep = levels(party)[1:10])) %>%
   group_by(party, scenario) %>%
   summarise(seats = sum(seats)) 
   

svg("../img/0082-scenarios1.svg", 10, 6.5)
res2014 %>%
   ggplot(aes(x = seats, y = scenario, label = seats))  +
   facet_wrap(~party, ncol = 4) +
   geom_segment(xend = 0, aes(yend = scenario, colour = party), size = 3, alpha = 0.3) +
   scale_colour_manual(values = parties_v) +
   scale_y_discrete("", limits =  levels(res2014$scenario)[3:1]) +
   geom_text(size = 3) +
   theme(legend.position = "none") +
   ggtitle("Impact of changing minimum threshold for seats in Parliament",
           "New Zealand 2014 election seat allocation") +
   labs(x = "Seats", 
        caption = "Analysis using the `nzelect` R package")
dev.off()


#================2011 theshold scenarios=========
res2011 <- "http://archive.electionresults.govt.nz/electionresults_2011/partystatus.html" %>%
   read_html() %>%
   # extract all the tables:
   html_nodes(css = "table") %>%
   # we only want the fourth table:
   extract2(4) %>%
   # turn it into a data frame:
   html_table() %>%
   # drop the first four rows:
   slice(-(1:4))

# give it some real names:
names(res2011) <- c("Party", "PartyVote", "PropVote", "ElectorateSeats", "ListSeats", "TotalSeats")

res2011 <- res2011 %>%
   # turn into numbers:
   mutate(PartyVote = as.numeric(gsub(",", "", PartyVote)),
          ElectorateSeats = as.numeric(ElectorateSeats)) %>%
   # remove the "total seats" row:
   filter(!is.na(PartyVote)) %>%
   # estimate seats
   mutate(
      `5% threshold\n(actual result)` = 
         allocate_seats(PartyVote, electorate = ElectorateSeats)$seats_v,
      `3.25% threshold like Israel\n(hypothetical result)` = 
         allocate_seats(PartyVote, electorate = ElectorateSeats, threshold = 0.0325)$seats_v,
      `No threshold\n(hypothetical result)` = 
         allocate_seats(PartyVote, electorate = ElectorateSeats, threshold = 0)$seats_v
   )


svg("../img/0082-scenarios2.svg", 10, 6.5)
res2011 %>%
   select(-(PartyVote:TotalSeats)) %>%
   gather(scenario, seats, -Party) %>%
   mutate(Party = gsub(" Party", "", Party),
          Party = gsub("New Zealand First", "NZ First", Party)) %>%
   left_join(parties_df[, c("Shortname", "Colour")], by = c("Party" = "Shortname")) %>%
   mutate(Party = fct_reorder(Party, -seats),
          Party = fct_other(Party, keep = levels(Party)[1:11])) %>%
   ggplot(aes(x = seats, y = scenario, label = seats))  +
   facet_wrap(~Party, ncol = 4) +
   geom_segment(xend = 0, aes(yend = scenario, colour = Party), size = 3, alpha = 0.3) +
   scale_colour_manual(values = parties_v) +
   scale_y_discrete("", limits =  levels(res2014$scenario)[3:1]) +
   geom_text(size = 3) +
   theme(legend.position = "none") +
   ggtitle("Impact of changing minimum threshold for seats in Parliament",
           "New Zealand 2011 election seat allocation") +
   labs(x = "Seats", y = "",
        caption = "Analysis using the `nzelect` R package")
dev.off()

#=========polls demo=========
election_dates <- polls %>%
   filter(Pollster == "Election result") %>%
   select(MidDate) %>%
   distinct()

svg("../img/0082-polls.svg", 10, 7)
polls %>%
   filter(!Party %in% c("Destiny", "Progressive")) %>%
   mutate(Party = gsub("M.ori", "Maori", Party)) %>%
   mutate(Party = fct_reorder(Party, VotingIntention, .desc = TRUE),
          Pollster = fct_relevel(Pollster, "Election result")) %>%
   ggplot(aes(x = MidDate, y = VotingIntention, colour = Pollster)) +
   geom_line(alpha = 0.5) +
   geom_point(size = 0.7) +
   geom_smooth(aes(group = Party), se = FALSE, colour = "grey15", span = .20) +
   scale_y_continuous("Voting intention", label = percent) +
   scale_x_date("") +
   facet_wrap(~Party, scales = "free_y") +
   geom_vline(xintercept = as.numeric(election_dates$MidDate), colour = "grey80") +
   ggtitle("15 years of voting intention opinion polls in New Zealand") +
   labs(caption = "Source: nzelect #Rstats package on CRAN")
dev.off()

convert_pngs("0082")
