library(WDI)
WDIsearch("gdp per capita") |> View()
WDIsearch("gni per capita") |> View()
WDIsearch("poverty") |> View()

# GDP per capita, constant 2015 USD
gdppc <- WDI(country = "KIR", indicator = "NY.GDP.PCAP.KD") |>
  as_tibble()

gdppc  |>
  ggplot(aes(x = year, y = NY.GDP.PCAP.KD)) +
  geom_line()


# GBI per capita, constant 2015 USD
gnipc <- WDI(country = "KIR", indicator = "NY.GNP.PCAP.KD") |>
  as_tibble()

gnipc |>
  ggplot(aes(x = year, y = NY.GNP.PCAP.KD)) +
  geom_line()


p <- gdppc |>
  select(year, `GDP per capita` = NY.GDP.PCAP.KD) |>
  full_join(gnipc, by = "year") |>
  rename(`GNI per capita` = NY.GNP.PCAP.KD) |>
  select(-(country:iso3c)) |>
  gather(variable, value, -year) |>
  ggplot(aes(x = year, y = value, colour = variable)) +
  geom_line() +
  labs(colour = "", 
       y = "Constant (2015) US dollars per person",
       x = "",
       caption = "Source: World Development Indicators NY.GDP.PCAP.KD and NY.GNP.PCAP.KD",
       title = "Gross national income and domestic product in Kiribati",
       subtitle = "Fisheries license features in national income but not as domestic production") +
  scale_y_continuous(label = dollar, limits = c(0, 3500))

svg_png(p, "../img/kiribati-gni-and-gdp", w = 8, h = 6)


# Poverty head count ratio estimates total doesn't work, no data
povhcr <- WDI(country = "KIR", indicator = "IN.POV.HCR.EST.TOTL") |>
  as_tibble()

# there should be only one observation of htis, for 2019...
filter(pov4, !is.na(IN.POV.HCR.EST.TOTL))
