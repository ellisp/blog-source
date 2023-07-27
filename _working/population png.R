library(tidyverse)

# an odd thing about PNG numbers is that allegedly Moresby has stayed at 5%
# of the country's population even as the population more than doubled in size
# ie moresby growing at the same rate a sthe rest of the country.

# 1980, 1990 and 2000 taken from https://spccfpstore1.blob.core.windows.net/digitallibrary-docs/files/5f/5f5e4d2ffe6c53ae19cf5f90dc2cf9d0.pdf?sv=2015-12-11&sr=b&sig=eN5jB9ZdCIhbi5M6slfv1yg5O%2FSr3SxeTsnI8ftM2E8%3D&se=2023-12-25T05%3A03%3A38Z&sp=r&rscc=public%2C%20max-age%3D864000%2C%20max-stale%3D86400&rsct=application%2Fpdf&rscd=inline%3B%20filename%3D%22PNG_2000_Census_National_Report.pdf%22
# or you can get them all from https://spccfpstore1.blob.core.windows.net/digitallibrary-docs/files/f2/f29f4484aeb618921dc4511a3b7ee617.pdf?sv=2015-12-11&sr=b&sig=2gL%2FO2P%2FEYbTssywPAem6GyY8BymOSOJmss%2FzcGDPlc%3D&se=2023-12-25T05%3A08%3A18Z&sp=r&rscc=public%2C%20max-age%3D864000%2C%20max-stale%3D86400&rsct=application%2Fpdf&rscd=inline%3B%20filename%3D%22PNG_2011_Census_National_Report.pdf%22 
# the 1980 and 1990 pop figures for NCD are on the map at https://spccfpstore1.blob.core.windows.net/digitallibrary-docs/files/98/98db7955a63f8367b3ec74354862b733.pdf?sv=2015-12-11&sr=b&sig=QtGIpV6hE6xDNdUrslZDyDOHRYuGFIrPXkyOVhVJofA%3D&se=2023-12-25T05%3A13%3A20Z&sp=r&rscc=public%2C%20max-age%3D864000%2C%20max-stale%3D86400&rsct=application%2Fpdf&rscd=inline%3B%20filename%3D%22PNG_1990_Census_Population_Distribution_Map_by_Province.pdf%22
png <- tibble(
  year = c(1980, 1990, 2000, 2011),
  pop_png = c(2978057, 3582333, 5171548, 7254442),
  prop_ncd = c(NA, NA, 4.9, 5) / 100,
  pop_ncd = c(123624, 195570, NA, NA)
) |>
  mutate(pop_ncd = if_else(is.na(pop_ncd), pop_png * prop_ncd, pop_ncd),
         prop_ncd = if_else(is.na(prop_ncd), pop_ncd / pop_png, prop_ncd))
png

png |>
  ggplot(aes(x = year, y = pop_png)) +
  geom_line() +
  scale_y_log10()
