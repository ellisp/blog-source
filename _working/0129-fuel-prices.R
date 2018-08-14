library(tidyverse)
library(scales)
library(openxlsx)

# download...


sn <- getSheetNames("fuel price data.xlsx")
sn <- sn[sn != "Source"]

fuel_orig <- list()

for(i in 1:length(sn)){
  tmp <- read.xlsx("fuel price data.xlsx", sheet = sn[i], cols = 1:7, 
                   detectDates = TRUE, na.strings = c("NA", "n/a"))
  tmp[ , "region"] <- sn[i]
  fuel_orig[[i]] <- tmp
}

fuel_df <- do.call("rbind", fuel_orig)
summary(fuel_df)

fuel_tidy <- fuel_df %>%
  select(-LPG) %>%
  gather(fueltype, value, -Company, -Date, -region) %>%
  filter(!is.na(value)) 

south_island <- c("Canterbury", "Nelson", "Otago", "Southland", "West Coast")



p91 <- fuel_tidy %>%
  filter(fueltype == "91") %>%
  group_by(region, Date) %>%
  summarise(value = mean(value, tr = 0.2)) %>%
  mutate(island = ifelse(region %in% south_island, "South", "North")) %>%
  ungroup() %>%
  mutate(region = fct_reorder(region, as.numeric(as.factor(island))))

p91_rel <- p91 %>%
  group_by(Date) %>%
  mutate(Auckland = value[region == "Auckland"]) %>%
  filter(! region %in% c("Auckland", "Wairarapa")) %>%
  mutate(perc_of_auck = value / Auckland)



svg("../img/0129-petrol-orig.svg", 10, 6)
ggplot() +
  geom_rect(data = data.frame("hello world"), 
            xmin = as.Date("2018-07-01"), xmax = Inf, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) +
  geom_line(data = filter(p91, region != "Wairarapa"), aes(x= Date, y = value, colour = island)) +
  facet_wrap(~region) +
  scale_y_continuous("Price per litre of 91 grade petrol\n", label = dollar)
dev.off()



svg("../img/0129-petrol-perc-auckland.svg", 10, 6)
ggplot() +
  geom_hline(yintercept = 1, colour = "grey50") +
  geom_rect(data = data.frame("hello world"), 
            xmin = as.Date("2018-07-01"), xmax = Inf, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) +
  geom_line(data = p91_rel, aes(x= Date, y = perc_of_auck, colour = island)) +
  facet_wrap(~region, ncol = 3) +
  scale_y_continuous("Price of 91 fuel as a percentage of in Auckland\n", label = percent) +
  labs(x = "2018")
dev.off()


svg("../img/0129-petrol-comp-auckland.svg", 10, 6)
ggplot() +
  geom_rect(data = data.frame("hello world"), 
            xmin = as.Date("2018-07-01"), xmax = Inf, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) +
  geom_ribbon(data = p91_rel, aes(x = Date, ymin = Auckland, ymax = value), fill = "grey", alpha = 0.5) +
  geom_line(data = p91_rel, aes(x = Date, y = Auckland), colour = "grey50") +
  geom_line(data = p91_rel, aes(x= Date, y = value, colour = island), size = 1.2) +
  facet_wrap(~region, ncol = 3) +
  scale_y_continuous("Price of 91 fuel compared to in Auckland\n", label = dollar) +
  labs(x = "2018; grey line shows Auckland")
dev.off()

svg("../img/0129-auck-minus-rest.svg", 8, 4)
fuel_tidy %>%
  filter(fueltype == "91") %>%
  mutate(island = ifelse(region %in% south_island, "South", "North")) %>%
  group_by(Date) %>%
  summarise(auck_v_rest = 
              mean(value[region == "Auckland"], tr = 0.2) - 
              mean(value[region != "Auckland"], tr = 0.2),
            auck_v_si = 
              mean(value[region == "Auckland"], tr = 0.2) - 
              mean(value[island != "South"], tr = 0.2)) %>%
  mutate(post_tax = as.integer(Date >= as.Date("2018-07-01"))) %>%
  gather(comparison, value, -Date, -post_tax) %>%
  mutate(comparison = ifelse(comparison == "auck_v_si", "South Island", "All NZ except Auckland")) %>%
  ggplot(aes(x = Date, y = value)) +
  facet_wrap(~comparison, ncol = 2) +
  geom_line() +
  geom_smooth(aes(group = post_tax), method = "lm") +
  scale_y_continuous("Average price in Auckland minus\naverage price in comparison area",
                     label = dollar)
dev.off()  
  

#=======modelling=============

library(nlme)
library(forcats)
head(fuel_tidy)

tidy_91 <- fuel_tidy %>%
  filter(fueltype == 91) %>%
  filter(region != "Wairarapa") %>%
  group_by(Date, Company, region) %>%
  mutate(value = mean(value)) %>%
  mutate(post_tax = as.integer(Date >= as.Date("2018-07-01"))) %>%
  ungroup() %>%
  mutate(Company = fct_relevel(Company, "Z ENERGY"),
         auckland_post_tax = as.integer(region == "Auckland" & post_tax == 1))

mod <- lme(value ~ Date * post_tax + Company + auckland_post_tax, 
           random = ~ post_tax | region, 
           correlation = corAR1(),
           data = tidy_91)


summary(mod)
anova(mod)

rf <- ranef(mod)
plot(rf$post_tax)
apply(rf, 2, mean)
coef(mod)

ints <- intervals(mod, which = "fixed")$fixed
ints <- round(ints, 2)
ints
#----------with differences--------------
# not sure this makes sense
diff_91 <- fuel_tidy %>%
  group_by(Date, region) %>%
  mutate(value = value - lag(value)) %>%
  filter(!is.na(value)) %>%
  mutate(post_tax = as.integer(Date >= as.Date("2018-07-01")))

mod2 <- lme(value ~ poly(Date, 2) + Company, data = diff_91, random = ~ post_tax | region, correlation = corAR1())

summary(mod2)
anova(mod2)

rf <- ranef(mod2)


dim(diff_91)
