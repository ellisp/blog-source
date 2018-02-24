# http://www.mbie.govt.nz/info-services/business/business-growth-agenda/regions

download.file("http://www.mbie.govt.nz/info-services/business/business-growth-agenda/regions/documents-image-library/REAR-Webtool-20160610.zip",
              destfile = "tmp.zip", mode = "wb")
unzip("tmp.zip") # read the "regional-economic-activity-data.txt" file for description

rea <- read.csv("all-data.csv", stringsAsFactors = FALSE)
dim(rea) # 1.1 million rows, 14 columns

head(rea)

# cleanup
unlink("tmp.zip")
unlink("all-data.csv")

unique(rea$indicator)
unique(rea$slice)
unique(rea$area)
unique(rea$feature)

rea %>%
   filter(slice == "unemployment rate, annual average" &
             area == "Lower Hutt")

