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
         company_type = ifelse(Company %in% big_four, "Big Four", "Smaller"))
  

p91 <- fuel_tidy %>%
  filter(fueltype == "91") %>%
  group_by(region, island, Date) %>%
  summarise(value = mean(value, tr = 0.2)) %>%
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

diff_data <- fuel_tidy %>%
  filter(fueltype == "91" & company_type == "Big Four") %>%
  group_by(Date) %>%
  summarise(auck_v_rest = 
              mean(value[region == "Auckland"], tr = 0) - 
              mean(value[region != "Auckland"], tr = 0),
            auck_v_si = 
              mean(value[region == "Auckland"], tr = 0) - 
              mean(value[island != "South"], tr = 0)) %>%
  mutate(post_tax = as.integer(Date >= as.Date("2018-07-01"))) %>%
  gather(comparison, value, -Date, -post_tax) %>%
  mutate(comparison = ifelse(comparison == "auck_v_si", "Compared to South Island", 
                             "Compared to all NZ except Auckland"))

svg("../img/0129-auck-minus-rest.svg", 8, 4)
ggplot(diff_data, aes(x = Date, y = value)) +
  facet_wrap(~comparison, ncol = 2) +
  geom_line() +
  geom_smooth(aes(group = post_tax), method = "lm") +
  scale_y_continuous("Average price of 91 petrol in Auckland minus\naverage price in comparison area",
                     label = dollar) +
  labs(x = "Date in 2018.",
       caption = "Source: pricewatch.co.nz, collated by @Economissive") +
  ggtitle("Fuel prices in Auckland compared to two other comparison areas",
          "Restricted to prices from BP, Caltex, Mobil and Z Energy")
dev.off()  

D <- subset(diff_data, comparison == "Compared to all NZ except Auckland")

diff_data_ts <- ts(D$value)

forecast::auto.arima(diff_data_ts, xreg = D$post_tax)

model <- gls(value ~ Date * post_tax, 
            data = subset(diff_data, comparison == "Compared to all NZ except Auckland"),
            cor = corARMA(p = 1, q = 1))

auto.arima(residuals(model))

summary(model)  
plot(model)
convert_pngs("0129")

fuel_tidy %>%
  filter(Date == "2018-06-30" & region == "Wellington")

# Wouldn't it be better to model all the original microdata as a multilevel model rather than aggregating it? 
# Yes, but that gets much more complicated very quickly, and also badly loses in interpretability.
