

# used the direct link generator https://sites.google.com/site/gdocs2direct/
# to generate a downloadable link from the google drive share link, which was
#   https://drive.google.com/file/d/1FomfQUt5U1yhxjkYDKW7SbSLp7J-tHM_/view
download.file("https://drive.google.com/uc?export=download&id=1FomfQUt5U1yhxjkYDKW7SbSLp7J-tHM_",
              destfile = "fiji-pop-grid.tif", mode = "wb")


library(raster)
library(tidyverse)
library(RColorBrewer)
library(quadmesh)
library(rayshader)
library(rgl)
fiji_grid <- raster("fiji-pop-grid.tif")

plot(fiji_grid)
attributes(fiji_grid)

head(fiji_grid)
dim(fiji_grid)
summary(fiji_grid)


# https://gis.stackexchange.com/questions/200417/very-basic-question-on-extracting-data-from-tif-raster-layer-in-r-projection-n
projection(fiji_grid)


plot(fiji_grid, xlim = c(2.05, 2.1) * 1e6, ylim = c(4.04, 4.08) * 1e6)

ext <- attributes(fiji_grid)$extent
attributes(fiji_grid)$ncols
attributes(fiji_grid)$nrows
dim(fiji_grid)

ext@xmin

x <- tibble(
  orig_col_number = 1:ncol(fiji_grid),
  col_name = paste0("V", orig_col_number),
  xcoord = seq(from = ext@xmin, to = ext@xmax, length.out = ncol(fiji_grid))
)

# convert into a sparse tibble
fiji_grid_tb <- fiji_grid |>
  as.matrix() |>
  as.data.frame() |>
  mutate(orig_row_number = 1:n(),
         ycoord = seq(from = ext@ymax, to = ext@ymin, length.out = n())) |>
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


fiji_grid_tb |>
  filter(xcoord > 1.84e6 & xcoord < 1.99e6 & ycoord > 3.84e6 & ycoord < 3.97e6) |>
  summary()

plot(fiji_grid, xlim = c(1.87, 1.99) * 1e6, ylim = c(3.84, 3.97) * 1e6)

library(RColorBrewer)

viti_levu <- crop(fiji_grid, extent(c(1.87, 1.99, 3.84, 3.97) * 1e6))

suva <- crop(fiji_grid, extent(c(1.965, 1.98, 3.87, 3.888) * 1e6))
central_suva <- crop(fiji_grid, extent(c(1.964, 1.971, 3.872, 3.881) * 1e6))

pal <- brewer.pal(11, "RdYlBu")[11:1]
par(bty = "n")
plot(viti_levu, col = pal, xaxt = "n", yaxt = "n")
plot(suva, col = pal, xaxt = "n", yaxt = "n")
plot(central_suva, col = pal, xaxt = "n", yaxt = "n")


viti_levu_qm <- quadmesh(viti_levu)
# doesn't work
# shade3d(viti_levu_qm)
# clear3d(viti_levu_qm)

suva_qm <- quadmesh(suva)
# doesn't work
# shade3d(suva_qm)


# does work
library(rasterVis)
plot3D(viti_levu, col = brewer.pal(11, "RdYlBu")[11:1])

plot3D(suva)

gplot(suva) +
  geom_point(aes(colour = value)) +
  coord_equal()


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
  plot_3d(heightmap = csm)



sm |>
  sphere_shade(texture = "bw") |>
  plot_3d(heightmap = sm, zscale = 5)



vl <- viti_levu |>
  raster_to_matrix()
vl[is.na(vl)] <- 0
vl |>
  sphere_shade(texture = "bw") |>
  plot_3d(heightmap = vl, zscale = 5)

# so the grids are actually a bit pointillistic. Lots of zeroes and 5s
table(sm)


library(mgcv)
smooth_matrix <- function(m, family = quasipoisson, trunc_level = -Inf, 
                          replace_na = NULL, smooth = 0.5, frac = 1, seed = 123,
                          ...){
  
  set.seed(123)
  
  stopifnot(length(dim(m)) == 2)
  stopifnot(smooth >= 0 & smooth <= 1)
  
  if(!is.null(replace_na)){
    m[is.na(m)] <- replace_na
  }
  
  d <- as.data.frame(m)
  names(d) <- paste0(1:ncol(d))
  d$row <- 1:nrow(d)
  ds <- gather(d, col, value, -row)
  ds$col <- as.numeric(ds$col)
  mod <- mgcv::gam(value ~ s(row, col, ...), family = family, 
                   data = sample_frac(ds, size = frac))
  ds$fit <- pmax(trunc_level, predict(mod, newdata = ds))
  ds$fit <- ds$fit * smooth + ds$value * (1 - smooth)
  ds$value <- NULL
  d2 <- spread(ds, col, fit) |>
    arrange(row) |>
    select(-row)
  m2 <- as.matrix(d2)
  
  stopifnot(ncol(m2) == ncol(m))
  stopifnot(nrow(m2) == nrow(m))
  return(m2)
}

# this isn't working visually
sm2 <- smooth_matrix(sm, smooth = 0.99, trunc_level = 0, frac = 0.8)
sm2 |>
  sphere_shade(texture = "bw") |>
  plot_3d(heightmap = sm2, zscale = 0.2)

sm |>
  sphere_shade(texture = "bw") |>
  plot_3d(heightmap = sm, zscale = 5)

vl2 <- viti_levu |>
  raster_to_matrix() |>
  smooth_matrix(replace_na = 0, smooth = 0.5, frac = 0.1)

vl2 |>
  sphere_shade(texture = "desert") |>
  plot_3d(heightmap = vl2)
