# basic functionality
library(WHO)
library(dplyr)
library(ggplot2)
library(scales)
library(showtext)
library(scatterD3) # note - this is the GitHub version so we have ellipses
library(stringr)
library(RColorBrewer)
library(dygraphs)

# ---------------default fonts, themes and colour scale
font.add.google("Poppins", "myfont")
showtext.auto()


mdg_theme <- theme_light(base_family = "myfont") + theme(legend.position = "bottom")
theme_set(mdg_theme)

scale_colour_discrete <- function(...) {
   scale_colour_manual("", values = brewer.pal(6, "Spectral")[c(1,2,5,6)], ...)
}


#------------------data prep-------------
# download all WHO codes
codes <- get_codes()
dim(codes) # 2144 codes


mdg_codes <- codes[grepl("^MDG", codes$label), ]
mdg_codes$number <- as.numeric(str_sub(mdg_codes$label, start = -2))


dim(mdg_codes) # 33 for MDGs
mdg_codes$label

#----------------helper functions---------------
prep <- function(data){
   # remove regional groupings, and make income a factor with levels in correct order
   data %>%
      filter(!worldbankincomegroup %in% c("Global", "NA")) %>%
      filter(!is.na(region)) %>%
      filter(!is.na(country)) %>%
      mutate(worldbankincomegroup = factor(worldbankincomegroup,
                                           levels = c("Low-income", "Lower-middle-income",
                                                      "Upper-middle-income", "High-income")))
}
   

latest <- function(data, nudge = 1){
   # return a cut back data frame of just the latest value, useful for geom_text annotations
   data %>%
      group_by(country) %>%
      filter(year == max(year)) %>%
      mutate(year = year + nudge)
}

#-------------------Infant mortality--------------
mdg1 <- get_data("MDG_0000000001")
# values are a funny mixture of points and intervals, so we extract just the points:
mdg1$value_n <- as.numeric(str_extract(mdg1$value, "[0-9]*\\.[0-9]*"))

mdg1a <- mdg1 %>% prep()


p1 <- mdg1a %>%
   ggplot(aes(x = year, y = value_n, colour = worldbankincomegroup, group = country)) +
   geom_line() + 
   facet_wrap(~region) +
   geom_text(data = latest(mdg1a), aes(label = country), hjust = 0, size = 3) +
   xlim(1990, 2025) +
   labs(x = "Year",
        y = mdg_codes[1, "display"],
        title = "'Reduce child mortality' MDG Indicator 14, infant mortality rate")

svg("../img/0029-mdg1.svg", 10, 9)
print(p1)
dev.off()

#--------------Adolescent fertility----------------
mdg3 <- get_data("MDG_0000000003")
# This isn't listed at http://www.unmillenniumproject.org/goals/gti.htm as an MDG indicator,
# but according to http://www.wikigender.org/wiki/adolescent-birth-rate/ it's part of
# measuring Goal 5 "Improve Maternal Health"

svg("../img/0029-mdg3.svg", 10, 9)
mdg3 %>%
   prep() %>%
   ggplot(aes(x = year, y = value, label = country, colour = worldbankincomegroup)) +
   labs(x = "Year of latest data",
        y = mdg_codes[2, "display"],
        title = "'Improve maternal health' MDG un-numbered indicator, adolescent fertility") +
   geom_text() +
   facet_wrap(~region) +
   scale_colour_manual("", values = brewer.pal(6, "Spectral")[c(1,2,5,6)]) +
   theme(legend.position = "bottom")
dev.off()

#----------contraceptive prevalence----------
mdg5 <- get_data("MDG_0000000005")

# for some odd reason there's no income group so we make it up:
mdg5a <- prep(mdg5) %>%
   select(-worldbankincomegroup) %>%
   left_join(unique(mdg1a[ , c("country", "worldbankincomegroup")]))

p5 <- mdg5a %>%
   prep() %>%
   ggplot(aes(x = year, y = value / 100, group = country, colour = worldbankincomegroup)) +
   facet_wrap(~region) +
   scale_y_continuous(mdg_codes[3, "display"], label = percent) +
   labs(x = "Year of latest data",
        title = "'Combat HIV/AIDS, malaria and other diseases' MDG indicator 19c, contraception prevalence") +
   xlim(1990, 2020) +
   geom_point() +
   geom_line() +
   geom_text(data = latest(mdg5a, 0.3), aes(label = country), hjust = 0, size = 3)
   
svg("../img/0029-mdg5.svg", 10, 9)
print(p5)
dev.off()



#==================scatter plot==================
# combine the three datasets into just one and knock out countries with missing values
comb <- latest(mdg3) %>%
   select(country, region, worldbankincomegroup, value) %>%
   rename(AdolFert = value) %>%
   left_join(latest(mdg5)[, c("country", "value")], by = "country") %>%
   rename(Contra = value) %>%
   left_join(latest(mdg1)[ , c("country", "value_n")], by = "country") %>%
   rename(InfantMortality = value_n) %>%
   filter(!is.na(country)) %>%
   filter(!is.na(Contra))

# draw scatter plot
scatterD3(x = comb$Contra, y = comb$AdolFert, lab = comb$country,
          size_var = comb$InfantMortality,
          col_var=comb$worldbankincomegroup, symbol_var = comb$region,
          xlab = "Access to contraception (%)", 
          ylab = "Adolescent fertility rate (per 1000 girls aged 15-19)", 
          col_lab = "",
          symbol_lab = "",
          size_lab = "Infant Mortality per 1000",
          ellipses = TRUE, ellipses_level = 0.75)
# note - next step to get into a web page requires manually saving it.


