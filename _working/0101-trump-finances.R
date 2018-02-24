library(tidyverse)
library(stringr)
library(tabulizer)

tf <- tempfile()
download.file("https://assets.documentcloud.org/documents/3867112/Trump-Financial-Disclosure-2017.pdf",
              mode = "wb", destfile = tf)

# takes some minutes to run:
tabs <- extract_tables(tf)
str(tabs)

length(tabs) # list with 135 elements
tabs[[1]]
tabs[[3]]
tabs[[4]]
tabs[[5]]

# finances start at page 14 (of 51)
# By trial and error we find this starts with element 27 and finish at element 58
View(tabs[[27]])
tabs[[59]]
tabs[[58]]
