

# used the direct link generator https://sites.google.com/site/gdocs2direct/
# to generate a downloadable link from the google drive share link, which was
#   https://drive.google.com/file/d/1FomfQUt5U1yhxjkYDKW7SbSLp7J-tHM_/view
if(!file.exists("fiji-pop-grid.tif")){
  download.file("https://drive.google.com/uc?export=download&id=1FomfQUt5U1yhxjkYDKW7SbSLp7J-tHM_",
              destfile = "fiji-pop-grid.tif", mode = "wb")
}

if(!file.exists("fiji-provinces.geojson")){
  download.file("https://pacificdata.org/data/dataset/f45cd559-7350-4362-91ad-0c922853fb5c/resource/0d2bdd5b-9c28-47fd-9dd8-2917daeb276d/download/2017_phc_fji_pid_4326.geojson",
                destfile = "fiji-provinces.geojson", mode = "wb")
}

library(terra)
library(tidyverse)
library(RColorBrewer)
library(quadmesh)
library(rayshader)
library(rgl)
library(RColorBrewer)
library(sf)
library(mapview)

fiji_grid <- rast("fiji-pop-grid.tif")
fiji_prov <- st_read("fiji-provinces.geojson")



ps <- unique(fiji_prov$Province)
i <- 10
fiji_prov |>
  filter(Province == ps[i]) |>
  ggplot() +
  geom_sf() +
  labs(title = ps[i])

fiji_prov |>
  filter(!Province %in% ps[c(3,5,7)]) |>
  ggplot() +
  geom_sf(aes(fill = Province), linewidth = 0) +
  theme(legend.position = "right")

#  problems with provinces that go across the 180 degree line
fiji_prov |>
#  filter(!Province %in% ps[c(3,5,7)]) |>
  ggplot() +
  geom_sf() +
  coord_sf(xlim = c(175, 180))

fiji_prov |>
  filter(!Province %in% ps[c(3,5,7)]) |>
  st_centroid()

# 3,5,7 - cross 180 degrees
# 2 - island above viti levu
# 15 way up north
# 4 Kadavu islands on right
# 6 Lomaiviti, islands to right
fiji_prov |>
  filter(Province %in% ps[c(1, 8, 9:13, 14)]) |>
  ggplot() +
  geom_sf(aes(fill = Province), colour = NA) +
  theme(legend.position = "right")
  
fiji_selected <- fiji_prov |>
  filter(Province %in% ps[c(1, 8, 9:13, 14)]) |>
  st_transform(crs = crs(fiji_grid))

# doesn't work
# mapview(fiji_selected)
library(tmap)

fiji_selected |>
  tm_shape() +
  tm_polygons()

d <- crop(fiji_grid, fiji_selected)  


plot(fiji_grid)
attributes(fiji_grid)

head(fiji_grid)
dim(fiji_grid)
summary(fiji_grid)


crs(fiji_grid)

ef <- ext(fiji_grid)

plot(fiji_grid, xlim = c(2.05, 2.1) * 1e6, ylim = c(4.04, 4.08) * 1e6)

x <- tibble(
  orig_col_number = 1:ncol(fiji_grid),
  col_name = paste0("V", orig_col_number),
  xcoord = seq(from = ef$xmin, to = ef$xmax, length.out = ncol(fiji_grid))
)

# convert into a sparse tibble
fiji_grid_tb <- fiji_grid |>
  raster_to_matrix() |>
  t() |>
  as.data.frame() |> 
  mutate(orig_row_number = 1:n(),
         ycoord = seq(from = ef$ymax, to = ef$ymin, length.out = n())) |>
  gather(col_name, value, -ycoord, -orig_row_number) |>
  drop_na() |>
  left_join(x, by = "col_name") |>
  select(xcoord, ycoord, orig_row_number, orig_col_number, value)

nrow(fiji_grid_tb) # only 51224 values

summary(fiji_grid_tb)
count(fiji_grid_tb, value, sort =  TRUE) |>
  slice(1:15)

# where are the denser points
fiji_grid_tb |>
  filter(value > 400)

fiji_grid_tb |>
  arrange(value) |>
  ggplot(aes(x = xcoord, y = ycoord, colour = value)) +
  geom_point(shape = 15, size = 0.2) +
  # note y limits in reverse order as is south of equaotr
  coord_equal(xlim = c(1.84, 1.99) * 1e6, ylim = c(3.84, 3.97) * 1e6) +
  #theme_void() +
  scale_colour_viridis_c()


plot(fiji_grid, xlim = c(1.87, 1.99) * 1e6, ylim = c(3.84, 3.97) * 1e6)


viti_levu <- crop(fiji_grid, ext(c(1.82, 2.02, 3.84, 3.97) * 1e6))

suva <- crop(fiji_grid, ext(c(1.965, 1.98, 3.87, 3.888) * 1e6))
central_suva <- crop(fiji_grid, ext(c(1.964, 1.971, 3.872, 3.881) * 1e6))

pal <- brewer.pal(11, "RdYlBu")[11:1]
library(viridis)
pal <- viridis(100, direction = 1, option = "C")

par(bty = "l")
plot(viti_levu, col = pal, axes = FALSE, background = "white")
plot(suva, col = pal, axes = FALSE, background = "grey80")
plot(central_suva, col = pal, axes = FALSE, background = "grey80")


plot(viti_levu, col = pal, axes = FALSE, background = "grey80", perimeter = FALSE)
inset(central_suva, col = pal, scale = 0.6, perimeter = FALSE, background = "grey90")

library(mapview)
mapview(viti_levu, maxpixels = 2600000)

sm <- raster_to_matrix(suva)
sm[is.na(sm)] <- 0
sm |>
  sphere_shade(texture = "desert") |>
  plot_map(rotate = 0)


sm |>
  sphere_shade(texture = "desert") |>
  plot_3d(heightmap = sm)
render_snapshot(filename = "temp", vignette =TRUE)

csm <- raster_to_matrix(central_suva)
csm[is.na(csm)] <- 0
csm |>
  sphere_shade(texture = "desert") |>
  plot_3d(heightmap = csm, zscale = 5)



sm |>
  sphere_shade(texture = "bw") |>
  plot_3d(heightmap = sm, zscale = 5)



vl <- viti_levu |>
  raster_to_matrix()
vl[is.na(vl)] <- 0
vl |>
  sphere_shade(texture = "desert") |>
  plot_3d(heightmap = vl, zscale = 1)

# so the grids are actually a bit pointillistic. Lots of zeroes and 5s
table(sm)


#------------smoothing--------
# there is a problem that a lot of the NAs are probably meant to be 0s.
# NA should be coast?

# vl3 <- crop(fiji_grid, fiji_selected)  
vl0 <- viti_levu
vl0[is.na(vl0)] <- 0

vl2 = focal(vl0, w = matrix(1, nrow = 5, ncol = 5), fun = mean)

mapview(vl2, maxpixels = 2600000)

tmap_mode("view")

vl2 |>
  tm_shape() +
  tm_raster()

vl2m <-  raster_to_matrix(vl2) 

vl2m |>  sphere_shade(texture = "desert") |>
  plot_3d(heightmap = vl2m, zscale = 1)


vl3m <- vl3 |>  
  raster_to_matrix() 

vl3m |>
  sphere_shade(texture = "desert") |>
  plot_3d(heightmap = vl3m, zscale = 1)

render_camera(theta=10,  phi=35, zoom = 0.6, fov=90)
vl2m |>  
  sphere_shade(texture = "desert") |>
  plot_3d(heightmap = vl2m, zscale = 1)

plot_3d(fiji_selected)

