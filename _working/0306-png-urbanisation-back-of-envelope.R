# drawing on the 2024 census report and the 2011 census report


# NCD 2011 361222
# Urban 2011 

library(tidyverse)
conflicts_prefer(dplyr::lag)
# pop per sq km in 2011
 7275324 / 461937

# people in NCD in 2011
0.05 * 7254442

# area in NCD in 2011 - Table 1.3 of the 2011 report ays it is 5%
(0.05 * 7254442 / 1366.3)


tibble(year = c(2024, 2011, 2000, 1990, 1980),
       urban = c(NA, 902891, 675403, 539331,365547),
       # other than for 2024, have to use the people per sq km and reverse engineer the actual number of people...:
       ncd    = c(756754, c(1366.3, 1059.0, 814.9, NA) * (0.05 * 7254442 / 1366.3)),
      total = c(10185363, 7254442, 5171548, 3582333, 2978057)) |> 
  arrange(year)|> 
  mutate(prop_urban = urban / total,
         ncd_prop_total = ncd / total,
         growth_total = (total / lag(total)) ^ (1 / (year - lag(year))) - 1,
         growth_ncd = (ncd / lag(ncd)) ^ (1 / (year - lag(year))) - 1) 


# if urban grew 2011 to 2024 same as NCD did, what would be the % urban?
902891 * 1.0743 ^ 13 / 10185363


