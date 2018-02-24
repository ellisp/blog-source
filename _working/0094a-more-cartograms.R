# devtools::install_github("ellisp/nzelect/pkg2", force = TRUE)

library(nzcensus)
library(tidyverse)
library(viridis)

# we need make_legend() and colour_scale() as defined in
# http://ellisp.github.io/blog/2017/04/23/cartograms

comb_data_ta <- ta_cart@data %>%
   left_join(TA2013, by = c("Name" = "TA2013_NAM")) %>%
   as_tibble()

svg("../img/0094-ta1.svg", 8, 7)
par(family = "myfont", font.main= 1, fg = "grey75")
plot(ta_cart,
     col = colour_scale(comb_data_ta$PropNoQualification2013))
title(main = "People with no qualification; areas sized by usual resident population")
make_legend(comb_data_ta$PropNoQualification2013, 
            title = "Percentage of all individuals\nwith no qualification",
            location = "left", cex = 0.8)
dev.off()

svg("../img/0094-ta2.svg", 8, 7)
par(family = "myfont", font.main= 1, fg = "grey75")
plot(ta_cart,
     col = colour_scale(comb_data_ta$PropLabourers2013))
title(main = "Labourers as a percentage of those with occupation;\nareas sized by usual resident population")
make_legend(comb_data_ta$PropLabourers2013, 
            title = "Percentage of all individuals\nwho are labourers",
            location = "left", cex = 0.8)
dev.off()

#==========prepare for shiny app==============
comb_data_reg <- reg_cart@data %>%
   left_join(REGC2013, by = c("Name" = "REGC2013_N")) 

comb_data_au <- au_cart@data %>%
   left_join(AreaUnits2013, by = c("Name" = "AU_NAM")) 



save(comb_data_au, file = "0094-cartograms/comb_data_au.rda")
save(comb_data_ta, file = "0094-cartograms/comb_data_ta.rda")
save(comb_data_reg, file = "0094-cartograms/comb_data_reg.rda")
variables <- names(comb_data_reg)[grepl("^Prop", names(comb_data_reg))]
variables <- gsub("^Prop", "", variables)
variables <- gsub("2013", "", variables)
save(variables, file = "0094-cartograms/variables.rda")

# rsconnect::deployApp("0094-cartograms", appName = "nzcensus-cartograms", account = "ellisp")

convert_pngs("0094")
