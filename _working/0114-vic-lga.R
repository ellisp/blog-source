
# host page is https://data.gov.au/dataset/psma-administrative-boundaries/resource/d4b6833f-a064-4809-ab7f-b0d407d48b36
# actual url of the zipped up shapefiles is http://data.gov.au/dataset/bdcf5b09-89bc-47ec-9281-6b8e9ee147aa/resource/d4b6833f-a064-4809-ab7f-b0d407d48b36/download/local-government-areas-november-2016.zip

library(sf)
fname <- "D:/Peter/Documents/assets/Local Government Areas NOVEMBER 2016/Standard/VIC_LGA_POLYGON_shp.shp"


vic <- st_read(fname)

head(vic)

plot(vic)
