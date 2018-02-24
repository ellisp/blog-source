#-------------functionality and data----------------

library(tidyverse)
library(scales)
library(nzelect)
library(grid)
library(forcats)
library(testthat)

# load up the last prediction data, which is a data frame of 4,800 rows
# (one for each simulated result) and 10 columns of party results

download.file("https://github.com/ellisp/ellisp.github.io/raw/source/data/ellis-final-nz-election-forecasts-2017.rda",
              destfile = "tmp.rda", mode = "wb")
load("tmp.rda")
unlink("tmp.rda")


#------------electorate seats that matter---------
# probability of Labour win in each of the seven Maori seats:
maori_probs <- data.frame(Labour = c(0.49, 0.52, 0.55, 0.58, 0.48, 0.64, 0.3)) %>%
  mutate(Other = 1 - Labour)

n <- nrow(sims) # number of simulations ie 4800

filler <- data.frame(
  party = c("Conservative", "Green", "NZ First", "United Future"),
  seats = c(0, 0, 1,0),
  sim = rep(1:n, each = 4)
)

# probability of ACT win in Epsom
epsom <- 0.8

# simulate electorate seat results:
electorate_sims <- data_frame(
  epsom = sample(c("ACT", "National"), prob = c(epsom, 1 - epsom), size = n, replace = TRUE),
  m1 = sample(c("Labour", "Maori"), prob = maori_probs[1, 1:2], size = n, replace = TRUE),
  m2 = sample(c("Labour", "Maori"), prob = maori_probs[2, 1:2], size = n, replace = TRUE),
  m3 = sample(c("Labour", "Maori"), prob = maori_probs[3, 1:2], size = n, replace = TRUE),
  m4 = sample(c("Labour", "Maori"), prob = maori_probs[4, 1:2], size = n, replace = TRUE),
  m5 = sample(c("Labour", "Mana"),  prob = maori_probs[5, 1:2], size = n, replace = TRUE),
  m6 = sample(c("Labour", "Maori"), prob = maori_probs[6, 1:2], size = n, replace = TRUE),
  m7 = sample(c("Labour", "Maori"), prob = maori_probs[7, 1:2], size = n, replace = TRUE)
) %>%
    mutate(sim = 1:n()) %>%
    gather(seat, party, -sim) %>%
    group_by(party, sim) %>%
    summarise(seats = n()) %>%
    ungroup() %>%
    rbind(filler) %>%
    spread(party, seats, fill = 0)

#-------------convert to total seats-------------------------
seats <- t(sapply(1:n, function(i){
  allocate_seats(votes      = as.numeric(sims[i, 1:9]), 
                 electorate = as.numeric(electorate_sims[i, -1]),
                 parties    = gsub("M.ori", "Maori", names(sims)[1:9]))$seats_v
})) %>%
    as_tibble()

#-------------compare to actual results----------------------

actual_results <- data_frame(
  party = c("ACT", "Green", "Labour", "Mana", "Maori", "National", "NZ First"),
  final_seats = c(1, 8, 46, 0, 0, 56, 9)
)
expect_equal(sum(actual_results$final_seats), 120)

d <- seats %>%
  gather(party, n_seats) %>%
  filter(!party %in% c("Conservative", "United Future", "Mana")) %>%
  left_join(actual_results) %>%
  mutate(success = ifelse(n_seats == final_seats, "Actual result", "Probability of other results")) %>%
  mutate(party = fct_reorder(party, desc(n_seats)))


# see https://stackoverflow.com/questions/4646020/ggplot2-axis-transformation-by-constant-factor
# for this idea to do a linear transformation of the y axis so it is probabilities rather than
# counts of the 4800 simulations:
formatter <- function(x, n = 4800){ 
  format(signif(x / n , 2), digits = 3)
}

levels(d$party)[levels(d$party) == "Maori"] <- "M\U0101ori"

svg("../img/0111-histograms.svg", 9,5)
d %>%
  ggplot(aes(x = n_seats, fill = party, alpha = success)) +
  facet_wrap(~party, scales = "free") +
  geom_histogram(colour = "white", binwidth = 1) +
  geom_histogram(data = filter(d, success == "Actual result"), colour = "white", fill = "grey10", binwidth = 1) +
  scale_alpha_manual("", values = c(`Actual result` = 0.9, `Probability of other results` = 0.3)) +
  labs(x = "Number of seats") +
  scale_y_continuous("Probability of outcome\n", labels = formatter) +
  ggtitle("Comparison of forecast and actual number of seats in the 2017 New Zealand election",
          "Forecasts are the combination of Peter's Stats' Stuff Models A and B") +
  scale_fill_manual(values = parties_v, guide = FALSE) 
dev.off()

convert_pngs("0111")

