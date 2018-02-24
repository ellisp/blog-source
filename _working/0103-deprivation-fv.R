
# Install nzcensus package, if necessary:
devtools::install_github("ellisp/nzelect/pkg2")

library(nzcensus)
library(tidyverse)
library(scales)
library(forcats)
library(testthat)
library(gridExtra)

# Download the New Zealand Deprivation Index by area unit from Massey University
nzdep <- read.csv("http://cphronline.massey.ac.nz/data/csv?inline=true&viewId=96&viewName=New+Zealand+Index+of+Deprivation+Atlas&geoId=15&subsetId=&instances=11701",
                  stringsAsFactors = FALSE)
names(nzdep) <- c("AU", "AU_NAM", "Dep", "X")


# check we have matches for all the area units:
expect_equal(sum(nzdep$AU_NAM %in% AreaUnits2013$AU_NAM),
             nrow(nzdep))

# merge the deprivation index with the Area Unit data from the nzcensus package:
d <- nzdep %>%
  select(-X) %>%
  left_join(AreaUnits2013, by = "AU_NAM") %>%
  mutate(Dep5 = ceiling(Dep / 2)) %>%
  mutate(denominator = ResidentPop2013,
         Maori = PropMaori2013 * denominator,
         Maori = ifelse(is.na(Maori), 0, Maori),
         Pacific = PropPacific2013 * denominator,
         Pacific = ifelse(is.na(Pacific), 0, Pacific),
         Other = denominator - Maori - Pacific) %>%
  as_tibble()

#-----------------exploratory graphics----------------------
svg("../img/0103-deprivation-ethnicity-barchart.svg", 8, 4)
d %>%
  group_by(Dep) %>%
  summarise(Maori = sum(Maori),
            Pacific = sum(Pacific),
            Other = sum(Other)) %>%
  gather(Ethnicity, Value, -Dep) %>%
  mutate(Ethnicity = fct_relevel(Ethnicity, "Other")) %>%
  ggplot(aes(x = as.factor(Dep), weight = Value, fill = Ethnicity)) +
  geom_bar(position = "stack") +
  scale_y_continuous("Usual resident population", label = comma) +
  labs(x = ("Massey Uni deprivation decile\n(higher numbers indicates living in a more deprived area)"),
       fill = "")
dev.off()

#' convenience function, just for this script, which draws
#' area unit box plots of a census variable by deprivation index
bp <- function(variable, ylab = NULL, label = percent, title = "",
               xlab = "Massey Uni deprivation decile\n(higher numbers indicates a more deprived area)"){
  if(is.null(ylab)){
    ylab <- gsub("Prop", "Proportion ", variable)
    ylab <- gsub("2013", "", ylab)
  }
  d %>%
    mutate(DepF = as.factor(Dep)) %>%
    ggplot(aes_string(x = "DepF", y = variable)) +
    geom_boxplot() +
    scale_y_continuous(ylab, label = label) +
    labs(x = xlab) +
    ggtitle(title)
  }

svg("../img/0103-boxplots.svg", 10, 9)
grid.arrange(
  bp("PropPacific2013", xlab = "", title = "Data by area unit from the New Zealand census 2013"),
  bp("PropSmoker2013", xlab = ""),
  bp("PropPartnered2013"),
  bp("MedianIncome2013", "Median Income", dollar)
)
dev.off()

#==================family violence deaths==================
# Data from Figure 3, page 21 of  "Family Violence Death Review Committee" Report 5

fv <- data_frame(
  Dep5 = rep(1:5, 2),
  Ethnicity = rep(c("Maori", "NonMaori"), each = 5),
  Offenders = c(0,  1,  4,  17, 46, 
                13, 14, 22, 18, 37)
)

# check if got roughly equal to what it should be
expect_equal(sum(fv$Offenders), 195 - 9 - 5 - 9) 

fv %>%
  group_by(Ethnicity) %>%
  summarise(Offenders = sum(Offenders))

d2 <- d %>%
  mutate(NonMaori = Pacific + Other) %>%
  select(NonMaori, Maori, Dep5) %>%
  gather(Ethnicity, Population, -Dep5) %>%
  group_by(Dep5, Ethnicity) %>%
  summarise(Population = sum(Population)) %>%
  left_join(fv) %>%
  mutate(NonOffenders = round(Population - Offenders),
         Proportion = Offenders / Population,
         Ethnicity = fct_relevel(Ethnicity, "NonMaori"))


mod_saturated <- glm(as.matrix(d2[ , c("Offenders", "NonOffenders")]) ~ Ethnicity * Dep5, 
            family = binomial, data = d2)  

mod_simple <- glm(as.matrix(d2[ , c("Offenders", "NonOffenders")]) ~ Ethnicity + Dep5, 
                     family = binomial, data = d2)  

summary(mod_simple)
summary(mod_saturated)
anova(mod_saturated)

svg("../img/0103-interaction.svg", 7, 5)
d2 %>%
  ggplot(aes(x = Dep5, y = Proportion, colour = Ethnicity)) +
  geom_point(aes(size = Population), alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, span = 2, alpha = 0.5) +
  geom_point(aes(size = Population), shape = 1, colour = "black") +
  scale_y_continuous("Proportion of area sub-population that\nwas an offender in family violence deaths",
                     label = percent) +
  scale_size_area(max_size = 10, label = comma) +
  labs(x = "Deprivation quintile (higher means more deprived)",
       colour = "",
       caption = "Source: Stats NZ census 2013 meshblock data, Massey University 2013 NZ Deprivation Index,
Family Violence Death Review Committee Fifth Report; 
analysis at https://ellisp.github.io.") +
  ggtitle("Offenders in family violence deaths New Zealand 2009-15",
          "By deprivation of area and ethnicity (n=172)")
dev.off()  

convert_pngs("0103")



#======================extras================================
kay <- d %>%
  group_by(Dep5) %>%
  summarise(Population = sum(ResidentPop2013),
            Partnered = round(sum(ResidentPop2013 * PropPartnered2013, na.rm = TRUE)),
            Population7 = Population * 7,
            Partnered7 = Partnered * 7,
            UnweightedMeanProportion = mean(PropPartnered2013, na.rm = TRUE),
            OverallProportion = Partnered / Population
            )
write.csv(kay, "partnered.csv", row.names = FALSE)
