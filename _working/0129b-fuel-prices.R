library(tidyverse)
library(scales)
library(openxlsx)
library(forecast)
library(nlme)

# download manually from https://www.dropbox.com/s/i75ha9n1jc0vm2c/fuel%20prices.xlsx?dl=0
# edit the "Central Plateau" sheet by setting the whole "Date" column to be in date format

fn <- "fuel prices data 2.xlsx"
sn <- getSheetNames(fn)
sn <- sn[which(sn == "Auckland"):which(sn == "Fiordland")]
fuel_orig <- list()

for(i in 1:length(sn)){
  tmp <- read.xlsx(fn, sheet = sn[i], cols = 1:7, 
                   detectDates = TRUE, na.strings = c("NA", "n/a"))
  tmp[ , "region"] <- sn[i]
  # for some reason heading of column 1 is missing for the Marlborough and Fiordland sheets so we fix"
  names(tmp)[1] <- "Company"
  fuel_orig[[i]] <- tmp
}

fuel_df <- do.call("rbind", fuel_orig)
summary(fuel_df)

south_island <- c("Canterbury", "Nelson", "Otago", "Southland", "West Coast", "Marlborough", "Fiordland")
big_four <- c("CALTEX", "Z ENERGY", "BP", "MOBIL")

fuel_tidy <- fuel_df %>%
  select(-LPG) %>%
  gather(fueltype, value, -Company, -Date, -region) %>%
  filter(!is.na(value)) %>%
  mutate(island = ifelse(region %in% south_island, "South", "North"),
         company_type = ifelse(Company %in% big_four, "Big Four", "Smaller")) %>%
  mutate(region = fct_reorder(region, as.numeric(as.factor(island))),
         Company = fct_reorder(Company, value))
  
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


svg("../img/0129b-petrol-comp-auckland.svg", 10, 8)
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

svg("../img/0129b-auck-minus-rest.svg", 9, 5)
ggplot(diff_data, aes(x = Date, y = value)) +
  facet_wrap(~comparison, ncol = 3) +
  geom_line() +
  geom_smooth(aes(group = post_tax), method = "lm") +
  scale_y_continuous("Average price of 91 octane petrol in Auckland\nminus average price in comparison area",
                     label = dollar) +
  labs(x = "Date in 2018\nAverage prices have not been weighted by population or sales",
       caption = "Source: pricewatch.co.nz, collated by @Economissive") +
  ggtitle("Fuel prices in Auckland compared to three other comparison areas",
          "Restricted to prices from BP, Caltex, Mobil and Z Energy")
dev.off()  

convert_pngs("0129b")
