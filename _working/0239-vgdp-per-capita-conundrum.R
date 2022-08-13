library(WDI)



WDIsearch("GDP per capita")


d <- WDI(c("GBR", "NZL"), 
         indicator = c("NY.GDP.PCAP.CD",
                       "NY.GDP.PCAP.KD")) |>
  as_tibble() |>
  rename(Nominal = NY.GDP.PCAP.CD,
         Real = NY.GDP.PCAP.KD)

# How can NZ be more than UK in nominal but not in real?

d |>
  ggplot(aes(x = year, y = Nominal, colour = country)) +
  geom_line()


d |>
  ggplot(aes(x = year, y = Real, colour = country)) +
  geom_line()
