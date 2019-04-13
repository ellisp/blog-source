

library(Cairo)
library(svglite)
library(frs)
library(clipr)

png("../img/0150-antialias-none.png", 8 * 600, 4 * 600, res = 600)
plot(1:2, 1:2, type = "l", bty = "n", main = "No anit-alias png() device")
dev.off()

CairoPNG("../img/0150-antialias-cairo.png", 8 * 600, 4 * 600, dpi = 600)
plot(1:2, 1:2, type = "l", bty = "n", main = "Anti-aliased CairoPNG device")
dev.off()

CairoSVG("../img/0150-antialias-cairo.svg", 8, 4)
plot(1:2, 1:2, type = "l", bty = "n", main = "CairoSVG device")
dev.off()

CairoSVG("../img/0150-cairo-svg.svg")
par(family = "Indie Flower")
plot.new()
text(0.5, 0.5, "Hello world", cex = 3)
dev.off()

svg("../img/0150-svg.svg")
par(family = "Indie Flower")
plot.new()
text(0.5, 0.5, "Hello world", cex = 4)
dev.off()

svglite("../img/0150-svglite.svg")
par(family = "Indie Flower")
plot.new()
text(0.5, 0.5, "Hello world", cex = 5)
dev.off()


print_to_screen <- function(fn){
  txt <- readChar(fn, file.info(fn)$size)
  write_clip(txt)
}

print_to_screen("../img/0150-cairo-svg.svg")
print_to_screen("../img/0150-svg.svg")
print_to_screen("../img/0150-svglite.svg")

svg_googlefonts("../img/0150-svglite.svg", "Indie Flower", new_svgfile = "../img/0150-svglite-with-fonts.svg")
print_to_screen("../img/0150-svglite-with-fonts.svg")

convert_pngs("0150")
