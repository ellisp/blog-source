library(tidyverse)
library(scales)
library(openxlsx)
library(forecast)
library(nlme)

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

south_island <- c("Canterbury", "Nelson", "Otago", "Southland", "West Coast")
big_four <- c("CALTEX", "Z ENERGY", "BP", "MOBIL")

fuel_tidy <- fuel_df %>%
  select(-LPG) %>%
  gather(fueltype, value, -Company, -Date, -region) %>%
  filter(!is.na(value)) %>%
  mutate(island = ifelse(region %in% south_island, "South", "North"),
         company_type = ifelse(Company %in% big_four, "Big Four", "Smaller")) %>%
  mutate(region = fct_reorder(region, as.numeric(as.factor(island))),
         Company = fct_reorder(Company, value))
  
png("../img/0129-overview.png", 17 * 600, 10 * 600, res = 600)
fuel_tidy %>%
  ggplot(aes(x = Date, y = value, colour = Company)) +
  facet_grid(fueltype~region, scales = "free_y") +
  geom_point(size = 0.8) +
  scale_y_continuous("Price per litre at the pump", label = dollar) +
  labs(x = "Date in 2018", 
       caption = "Source: pricewatch.co.nz, collated by @Economissive") +
  ggtitle("Petrol prices in New Zealand over several months in mid 2018") + 
  guides(colour = guide_legend(override.aes = list(size=5))) +
  scale_colour_brewer(palette = "Set1")
dev.off()

p91 <- fuel_tidy %>%
  filter(fueltype == "91") %>%
  group_by(region, island, Date) %>%
  summarise(value = mean(value, tr = 0.2)) %>%
  ungroup() 

p91_rel <- p91 %>%
  group_by(Date) %>%
  mutate(Auckland = value[region == "Auckland"]) %>%
  filter(! region %in% c("Auckland", "Wairarapa")) %>%
  mutate(perc_of_auck = value / Auckland)


# probably won't use this one:
svg("../img/0129-petrol-orig.svg", 10, 6)
ggplot() +
  geom_rect(data = data.frame("hello world"), 
            xmin = as.Date("2018-07-01"), xmax = Inf, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) +
  geom_line(data = filter(p91, region != "Wairarapa"), aes(x= Date, y = value, colour = island)) +
  facet_wrap(~region) +
  scale_y_continuous("Price per litre of 91 grade petrol\n", label = dollar) +
  labs(caption = "Source: pricewatch.co.nz, collated by @Economissive")
dev.off()



svg("../img/0129-petrol-perc-auckland.svg", 10, 6)
ggplot() +
  geom_hline(yintercept = 1, colour = "grey50") +
  geom_rect(data = data.frame("hello world"), 
            xmin = as.Date("2018-07-01"), xmax = Inf, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) +
  geom_line(data = p91_rel, aes(x= Date, y = perc_of_auck, colour = island)) +
  facet_wrap(~region, ncol = 3) +
  scale_y_continuous("Price of 91 octane petrol as a percentage of in Auckland\n", label = percent) +
  labs(x = "2018",
       caption = "Source: pricewatch.co.nz, collated by @Economissive")
dev.off()


svg("../img/0129-petrol-comp-auckland.svg", 10, 8)
ggplot() +
  geom_rect(data = data.frame("hello world"), 
            xmin = as.Date("2018-07-01"), xmax = Inf, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) +
  geom_ribbon(data = p91_rel, aes(x = Date, ymin = Auckland, ymax = value), fill = "grey", alpha = 0.5) +
  geom_line(data = p91_rel, aes(x = Date, y = Auckland), colour = "grey50") +
  geom_line(data = p91_rel, aes(x= Date, y = value, colour = island), size = 1.2) +
  facet_wrap(~region, ncol = 3) +
  scale_y_continuous("Price of 91 octane petrol compared to in Auckland\n", label = dollar) +
  labs(x = "2018; grey line shows Auckland",
       caption = "Source: pricewatch.co.nz, collated by @Economissive")
dev.off()

# Data on the difference between Auckland's average price and those in other areas:
diff_data <- fuel_tidy %>%
  filter(fueltype == "91" & company_type == "Big Four") %>%
  group_by(Date) %>%
  summarise(auck_v_rest = 
              mean(value[region == "Auckland"]) - 
              mean(value[region != "Auckland"]),
            auck_v_si = 
              mean(value[region == "Auckland"]) - 
              mean(value[island == "South"]),
            auck_v_ni = 
              mean(value[region == "Auckland"]) - 
              mean(value[island == "North" & region != "Auckland"]),
            ) %>%
  mutate(post_tax = as.integer(Date >= as.Date("2018-07-01"))) %>%
  gather(comparison, value, -Date, -post_tax) %>%
  mutate(comparison = case_when(
         comparison == "auck_v_si"   ~ "Compared to South Island",
         comparison == "auck_v_ni"   ~ "Compared to rest of North island",
         comparison == "auck_v_rest" ~ "Compared to all NZ except Auckland"))

svg("../img/0129-auck-minus-rest.svg", 9, 5)
ggplot(diff_data, aes(x = Date, y = value)) +
  facet_wrap(~comparison, ncol = 3) +
  geom_line() +
  geom_smooth(aes(group = post_tax), method = "lm") +
  scale_y_continuous("Average price of 91 octane petrol in Auckland\nminus average price in comparison area",
                     label = dollar) +
  labs(x = "Date in 2018\nAverage prices have not been weighted by population or sales",
       caption = "Source: pricewatch.co.nz, collated by @Economissive") +
  ggtitle("Fuel prices in Auckland compared to two other comparison areas",
          "Restricted to prices from BP, Caltex, Mobil and Z Energy")
dev.off()  

# Modelling
# first, make a convenient subset of the data (useful later in various tests and diagnositcs):
D <- subset(diff_data, comparison == "Compared to all NZ except Auckland")

# Fit model, taking care to specify time series residuals, which aren't as useful for inference
# as i.i.d. residuals and hence lead to more conservative inference:
model <- gls(value ~ Date * post_tax, 
            data = subset(diff_data, comparison == "Compared to all NZ except Auckland"),
            cor = corARMA(p = 1, q = 1))

# print-friendly summary of coefficieints
stargazer::stargazer(model, type = "html")

# more comprehensive summary (not shown in blog):
summary(model)  

plot(model)
convert_pngs("0129")

# Wouldn't it be better to model all the original microdata as a multilevel model rather than aggregating it? 
# Yes, but that gets much more complicated very quickly, and also badly loses in interpretability.


model2 <- auto.arima(residuals(model))

diff_data_ts <- ts(D$value)

model2 <- auto.arima(diff_data_ts, xreg = D$post_tax)
model2

library(knitr)
fuel_tidy %>%
  group_by(region, Company) %>%
  summarise(freq = n()) %>%
  spread(Company, freq, fill = 0) %>%
  arrange(desc(GULL)) %>%
  kable

