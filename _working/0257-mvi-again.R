
source("0255-mvi-pacific.R")

library(WDI)

WDIsearch("GDP") |> View()

# 	GDP per capita, PPP (constant 2017 international $)
gdp_ppp_pc <- WDI(indicator = "NY.GDP.PCAP.PP.KD", start = 2000) |>
  drop_na() |>
  group_by(iso3c) |>
  arrange(desc(year)) |>
  slice(1) |>
  ungroup() |>
  select(iso3c,
          gdp_year = year,
         gdp_pc = NY.GDP.PCAP.PP.KD)

v20 <- c("Afghanistan", "Bangladesh", "Barbados", "Benin", "Bhutan", "Burkina Faso",
"Cambodia", "Chad", "Colombia", "Comoros", "Costa Rica", "Côte D'Ivoire", 
"Democratic Republic of the Congo", "Dominica", "Dominican Republic",
"Eswatini", "Ethiopia", "Fiji", "Gambia", "Ghana", "Grenada", "Guatemala",
"Guinea", "Guyana", "Haiti", "Honduras", "Jordan", "Kenya", "Kiribati",
"Kyrgyzstan", "Lebanon", "Liberia", "Madagascar", "Malawi", "Maldives",
"Marshall Islands", "Mongolia", "Morocco", "Mozambique", "Namibia", "Nepal",
"Nicaragua", "Niger", "Pakistan", "Palau", "Palestine**", "Papua New Guinea",
"Paraguay", "Philippines", "Rwanda", "Saint Lucia", "Samoa", "Senegal",
"Sierra Leone", "South Sudan", "Sri Lanka", "Sudan", "United Republic of Tanzania",
"Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Tuvalu",
"Uganda", "Vanuatu", "Viet Nam", "Yemen")

# any non-matches? Yes, Palestine:
v20[!v20 %in% mvi$Country]
# Palestine not in the mvi.

mvi2 <- mvi |>
  mutate(type = ifelse(Country %in% v20, "Member of V-20", "Not a member of V-20")) |>
  left_join(gdp_ppp_pc, by = c("ISO" = "iso3c"))

# note there are seven countries with no GDP PPP per capita values
filter(mvi2, is.na(gdp_pc))

mvi2 |>
  group_by(type) |>
  summarise(median(svi),
            median(lsri),
            median(gdp_pc, na.rm = TRUE),
            median(`MVI - Score`))

pal <- c("Member of V-20" = "blue","Not a member of V-20" = "darkred")

mvi2 |>
  ggplot(aes(x = svi, y = lsri, colour = type)) +
  geom_point() +
  scale_colour_manual(values = pal)

mvi2 |>
  ggplot(aes(x = gdp_pc, y = `MVI - Score`, colour = type)) +
#  geom_smooth(method = "lm", se = TRUE) +
  geom_point() +
  geom_text_repel(data = filter(mvi2, `MVI - Score` < 40 |
                                  `MVI - Score` > 65 |
                                  gdp_pc > 50000 |
                                  gdp_pc < 2000),
                  aes(label = Country)) +
  scale_x_log10(label = dollar) +
  scale_colour_manual(values = pal)



mvi2 |>
  ggplot(aes(x = gdp_pc, y = `MVI - Score`, colour = type)) +
  #  geom_smooth(method = "lm", se = TRUE) +
  geom_point() +
  geom_text_repel(data = filter(mvi2, (`MVI - Score` < 50 & type == "Member of V-20")|
                                  (`MVI - Score` > 60 & type != "Member of V-20")),
                  aes(label = Country)) +
  annotate("label", x = 2000, y = 37, colour = pal[1], fontface = "italic",
           label.size = 0, fill = "grey90",
           label = "Member of V20 but lower than average vulnerability according to MVI") +
  annotate("label", x = 2000, y = 73, colour = pal[2], fontface = "italic",
           label.size = 0, fill = "grey90",
           label = "Not a member of V20 but high vulnerability according to MVI") +
  scale_x_log10(label = dollar) +
  scale_colour_manual(values = pal)
