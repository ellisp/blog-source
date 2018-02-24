library(tidyverse)
library(forcats)
library(riverplot)
library(sankeyD3)
library(foreign)
library(testthat)
library(mice)
library(grid)
library(survey)

# Caution networkD3 has its own sankeyD3 with less features.  Make sure you know which one you're using!
# Don't load up networkD3 in the same session

#============import data======================
# See previous blog posts for where this comes from:
unzip("D:/Downloads/NZES2014GeneralReleaseApril16.sav.zip")

nzes_orig <- read.spss("NZES2014GeneralReleaseApril16.sav", 
                       to.data.frame = TRUE, trim.factor.names = TRUE)

# Five versions of each row, so when we do imputation later on we
# in effect doing multiple imputation:
nzes <- nzes_orig[rep(1:nrow(nzes_orig), each = 5), ] %>%
   # lump up party vote in 2014:
   mutate(partyvote2014 = ifelse(is.na(dpartyvote), "Did not vote", as.character(dpartyvote)),
          partyvote2014 = gsub("M.ori", "Maori", partyvote2014),
          partyvote2014 = gsub("net.Man", "net-Man", partyvote2014),
          partyvote2014 = fct_infreq(partyvote2014)) %>%
   mutate(partyvote2014 = fct_lump(partyvote2014, 5)) 

# party vote in 2011, and a smaller set of columns to do the imputation:
nzes2 <- nzes %>%
   mutate(partyvote2011 = as.factor(ifelse(dlastpvote == "Don't know", NA, as.character(dlastpvote)))) %>%
   # select a smaller number of variables so imputation is feasible:
   select(dwtfin, partyvote2014, partyvote2011, dethnicity_m, dage, dhhincome, dtradeunion, dprofassoc,
          dhousing, dclass, dedcons, dwkpt)

# impute the missing values.  Although we are imputing only a single set of values,
# because we are doing it with each row repeated five times this has the same impact,
# for the simple analysis we do later on, as multiple imputation:
nzes3 <- complete(mice(nzes2, m = 1))

# Lump up the 2011 vote, tidy labels, and work out who was too young to vote:
nzes4 <- nzes3 %>%
   mutate(partyvote2011 = fct_lump(partyvote2011, 5),
          partyvote2011 = ifelse(grepl("I did not vote", partyvote2011), "Did not vote", as.character(partyvote2011)),
          partyvote2011 = ifelse(dage < 21, "Too young to vote", partyvote2011),
          partyvote2014 = as.character(partyvote2014))

#===============re-weighting to match actual votes in 2011 and 2014===================

# This makes a big difference, for both years, but more for 2011. "Did not vote" is only 16% in 2011
# if we only calibrate to 2014 voting outcomes, but in reality was 25.8%.  We calibrate for both
# 2011 and 2014 so the better proportions for both elections.

#------------2014 actual outcomes----------------
# http://www.elections.org.nz/news-media/new-zealand-2014-general-election-official-results
actual_vote_2014 <- data_frame(
   partyvote2014 = c("National", "Labour", "Green", "NZ First", "Other", "Did not vote"),
   Freq = c(1131501, 604534, 257356, 208300, 
            31850 + 16689 + 5286 + 95598 + 34095 + 10961 + 5113 + 1730 + 1096 + 872 + 639,
            NA)
)

# calculate the did not vote, from the 77.9 percent turnout
actual_vote_2014[6, 2] <- (100 / 77.9 - 1) * sum(actual_vote_2014[1:5, 2])

# check I did the turnout sums right:
expect_equal(0.779 * sum(actual_vote_2014[ ,2]), sum(actual_vote_2014[1:5, 2]))

#---------------2011 actual outcomes---------------------
# http://www.elections.org.nz/events/past-events-0/2011-general-election/2011-general-election-official-results
actual_vote_2011 <- data_frame(
   partyvote2011 = c("National", "Labour", "Green", "NZ First", "Other", "Did not vote", "Too young to vote"),
   Freq = c(1058636, 614937, 247372, 147511, 
            31982 + 24168 + 23889 + 13443 + 59237 + 11738 + 1714 + 1595 + 1209,
            NA, NA)
)
# calculate the did not vote, from the 74.21 percent turnout at 
# http://www.elections.org.nz/events/past-events/2011-general-election
actual_vote_2011[6, 2] <- (100 / 74.21 - 1) * sum(actual_vote_2011[1:5, 2])

# check I did the turnout sums right:
expect_equal(0.7421 * sum(actual_vote_2011[1:6 ,2]), sum(actual_vote_2011[1:5, 2]))

# from the below, we conclude 4.8% of the 2014 population (as estimated by NZES)
# were too young to vote in 2011:
nzes_orig %>%
   mutate(tooyoung = dage < 21) %>%
   group_by(tooyoung) %>%
   summarise(pop = sum(dwtfin),
             n = n()) %>%
   ungroup() %>%
   mutate(prop = pop / sum(pop))

# this is pretty approximate but will do for our purposes.
actual_vote_2011[7, 2] <- 0.048 * sum(actual_vote_2011[1:6, 2])

# Force the 2011 numbers to match the 2014 population
actual_vote_2011$Freq <- actual_vote_2011$Freq / sum(actual_vote_2011$Freq) * sum(actual_vote_2014$Freq)

#------------create new weights--------------------
# set up survey design with the original weights:
nzes_svy <- svydesign(~1, data = nzes4, weights = ~dwtfin)

# calibrate weights to the known total party votes in 2011 and 2014:
nzes_cal <- calibrate(nzes_svy, 
                      list(~partyvote2014, ~partyvote2011),
                      list(actual_vote_2014, actual_vote_2011))

# extract those weights for use in straight R (not just "survey")
nzes5 <- nzes4 %>%
   mutate(weight = weights(nzes_cal))

# See impact of calibrated weights for the 2014 vote:
prop.table(svytable(~partyvote2014, nzes_svy)) # original weights
prop.table(svytable(~partyvote2014, nzes_cal)) # recalibrated weights

# See impact of calibrated weights for the 2011 vote:
prop.table(svytable(~partyvote2011, nzes_svy)) # original weights
prop.table(svytable(~partyvote2011, nzes_cal)) # recalibrated weights


#=======================previous years vote riverplot version============

the_data <- nzes5 %>%
   mutate(
       # add two spaces to ensure the partyvote2011 does not have any
       # names that exactly match the party vote in 2014
       partyvote2011 = paste(partyvote2011, "  "),
       partyvote2011 = gsub("M.ori", "Maori", partyvote2011)) %>%
   group_by(partyvote2011, partyvote2014) %>%
   summarise(vote_prop = sum(weight)) %>%
   ungroup() 

# change names to the names wanted by makeRiver
names(the_data) <- c("col1", "col2", "Value")

# node ID need to be characters I think
c1 <- unique(the_data$col1)
c2 <- unique(the_data$col2)
nodes_ref <- data_frame(fullname = c(c1, c2)) %>%
   mutate(position = rep(c(1, 2), times = c(length(c1), length(c2)))) %>%
   mutate(ID = LETTERS[1:n()])

edges <- 
   the_data %>%
   left_join(nodes_ref[ , c("fullname", "ID")], by = c("col1" = "fullname")) %>%
   rename(N1 = ID) %>%
   left_join(nodes_ref[ , c("fullname", "ID")], by = c("col2" = "fullname")) %>%
   rename(N2 = ID) %>%
   as.data.frame(stringsAsFactors = FALSE)

rp <- makeRiver(nodes = as.vector(nodes_ref$ID), edges = edges,
                node_labels = nodes_ref$fullname,
                # manual vertical positioning by parties.  Look at
                # nodes_ref to see the order in which positions are set.  
                # This is a pain, so I just let it stay with the defaults:
                #  node_ypos = c(4, 1, 1.8, 3, 6, 7, 5, 4, 1, 1.8, 3, 6, 7),
                node_xpos = nodes_ref$position,
                # set party colours; all based on those in nzelect::parties_v:
                node_styles = list(C = list(col = "#d82a20"), # red labour
                                   D = list(col = "#00529F"), # blue national
                                   E= list(col = "black"),   # black NZFirst
                                   B = list(col = "#098137"), # green
                                   J = list(col = "#d82a20"), # labour
                                   I = list(col = "#098137"), # green
                                   K = list(col = "#00529F"), # national
                                   L = list(col = "black")))  # NZ First

# Turn the text horizontal, and pale grey
ds <- default.style()
ds$srt <- 0
ds$textcol <- "grey95"

mygp <- gpar(fontfamily = "myfont", col = "grey75")
# using PNG rather than SVG as vertical lines appear in the SVG version

png("../img/0098-vote.png", 8 * 600, 6 * 600, res = 600)
par(bg = "grey40", family = "myfont")

# note the plot_area argument - for some reason, defaults to using only half the
# vertical space available, so set this to higher than 0.5!:
plot(rp, default_style = ds, plot_area = 0.9)

title(main = "Self-reported party vote in 2011 compared to 2014", 
      col.main = "white", font.main = 1)

grid.text(x = 0.15, y = 0.1, label = "2011 party vote", gp = mygp)
grid.text(x = 0.85, y = 0.1, label = "2014 party vote", gp = mygp)
grid.text(x = 0.95, y = 0.03, 
          gp = gpar(fontfamily = "myfont", fontsize = 7, col = "grey75"), just = "right",
          label = "Source: New Zealand Election Study, analysed at https://ellisp.github.io")
dev.off()


#=======================sankeyD3 version====================

nodes_ref2 <- nodes_ref %>%
   mutate(ID = as.numeric(as.factor(ID)) - 1) %>%
   as.data.frame()

edges2 <-    the_data %>%
   ungroup() %>%
   left_join(nodes_ref2[ , c("fullname", "ID")], by = c("col1" = "fullname")) %>%
   rename(N1 = ID) %>%
   left_join(nodes_ref2[ , c("fullname", "ID")], by = c("col2" = "fullname")) %>%
   rename(N2 = ID) %>%
   as.data.frame(stringsAsFactors = FALSE) %>%
   mutate(Value = Value / sum(Value) * 100)



# not sure how .domain([]) works, so have had to set the colours by hand...
# .domain(["Did not vote", "Green",   "Labour",  "National", "NZ First", "Other",   "Labour  ", "Other  ", "Don\'t know  ", "Green  "])


pal <- 'd3.scaleOrdinal()
         .range(["#DCDCDC", "#098137", "#d82a20", "#00529F",  "#000000",  "#DCDCDC", 
                 "#DCDCDC", "#098137", "#d82a20", "#00529F", "#000000", "#DCDCDC"]);'

sankeyNetwork(Links = edges2, Nodes = nodes_ref2, 
              Source = "N1", Target = "N2", Value = "Value",
              NodeID = "fullname",
              NodeGroup = "fullname",
              LinkGroup = "col2",
              fontSize = 12, nodeWidth = 30,
              colourScale = JS(pal),
              numberFormat = JS('d3.format(".1f")'),
              fontFamily = "Calibri", units = "%", 
              nodeShadow = TRUE,
              showNodeValues = FALSE,
              width = 700, height = 500) %>% 
   # note; important to not load library(networkD3) as then there
   # is a clash with sankeyD3, so we call the saveNetwork() function
   # directly from the networkD3 namespace rather than loading the
   # whole package:
   networkD3::saveNetwork(file = '../img/0098-sankey.html')


#=======other by the by analysis==================
# Age density plot by party vote

# Remember to weight by the survey weights - in this case it controls for
# the under or over sampling by age in the original design.
svg("../img/0098-age-densities.svg", 8, 5)
nzes5 %>%
   ggplot(aes(x = dage, fill = partyvote2014, weight = weight / sum(nzes5$weight))) +
   geom_density(alpha = 0.3) +
   facet_wrap(~partyvote2014, scales = "free_y") +
   scale_fill_manual(values = parties_v) +
   theme(legend.position = "none") +
   labs(x = "Age at time of 2014 election",
        caption = "Source: New Zealand Election Study") +
   ggtitle("Age distribution by Party Vote in the 2014 New Zealand General Election")
dev.off()

