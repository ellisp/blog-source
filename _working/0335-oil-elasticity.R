setwd(here::here())
source(here::here(".Rprofile"))

library(tidyverse)
library(plot3D)
library(scales)
library(glue)
library(RColorBrewer)

# total oil use per day, needed
original_total <- 100
original_gulf_exports <- 20

elasticities <- seq(from = 0.1, to = 0.2, length.out = 11)
exports <- 2:original_gulf_exports
length(exports)
length(elasticities)

d <- expand_grid(
    elasticity = elasticities,
    export = exports
) |>
    mutate(
        lost_oil = (original_gulf_exports - export) / original_total,
        price_rise = lost_oil / elasticity * 100
    )

the_caption <- "Re-worked by freerangestats.info based on original analysis by Haver Analytics"
the_title <- "Illustrative impact of Gulf export cuts on oil prices"
#----------------Perspective plot---------------
# this recreates the original

prices_matrix <- d |>
    select(elasticity, export, price_rise) |>
    spread(elasticity, price_rise) |>
    select(-export) |>
    as.matrix()

reds <- colorRampPalette(c("mistyrose", "darkred"))(100)

persp_plot <- function() {
    persp3D(
        x = exports,
        y = elasticities,
        z = prices_matrix,
        col = reds,
        xlab = "\nOil exported from Gulf",
        ylab = "\n\nPrice elasticity of demand",
        zlab = "\nRise in oil prices (%)",
        main = the_title,
        # box type must be one of “b”, “b2”, “f”, “g”, “bl”, “bl2”, “u”, “n”
        bty = "g",
        ticktype = "detailed",
        colkey = FALSE
    )

    mtext(
        the_caption,
        side = 1,
        line = 4,
        cex = 0.8,
        col = "grey30"
    )
}
svg_png(persp_plot, "../img/0335-persp", w = 10, h = 6)

#-----------------Heat map--------------------
elasticities2 <- c(0.1, 0.12, 0.14, 0.16, 0.18, 0.2)
exports2 <- 1:10 * 2

p1 <- d |>
    filter(round(elasticity, 2) %in% elasticities2) |>
    filter(export %in% exports2) |>
    mutate(text_col = ifelse(price_rise >= 100, "Pale", "Dark")) |>
    ggplot(aes(x = export, y = elasticity, fill = price_rise)) +
    geom_tile() +
    geom_text(
        aes(label = percent(price_rise, scale = 1), colour = text_col),
        size = 4
    ) +
    scale_fill_gradientn(colours = reds, label = percent_format(scale = 1)) +
    scale_y_continuous(breaks = elasticities2) +
    scale_x_continuous(breaks = exports2) +
    scale_colour_manual(
        values = c("Pale" = "white", "Dark" = "black"),
        guide = "none"
    ) +
    labs(
        title = the_title,
        subtitle = str_wrap(
            "Illustrative price increase for crude oil depends on the price elasticity of demand and the amount of oil that is exported from the Gulf.",
            100
        ),
        x = "Oil exported from Gulf (in million barrels per day)",
        y = "Price elasticity of demand for crude oil",
        fill = "Price increase for crude oil",
        caption = the_caption
    )

svg_png(p1, "../img/0335-heatmap", w = 9, h = 7)

#-----------------line plot------------------
annot_col <- "grey88"

p2 <- d |>
    mutate(
        uncertainty_elasticity = abs(elasticity - 0.15),
        is_centre_est = ifelse(round(elasticity, 2) == 0.15, "solid", "dashed")
    ) |>
    ggplot(aes(
        x = export,
        y = price_rise,
        group = elasticity,
        colour = uncertainty_elasticity
    )) +
    geom_line(aes(linetype = is_centre_est)) +
    geom_text(
        data = filter(
            d,
            export == min(export),
            round(elasticity, 2) %in% c(0.1, 0.15, 0.2)
        ),
        aes(label = glue("Elasticity = {elasticity}")),
        hjust = 1,
        position = position_nudge(x = -0.1),
        colour = annot_col,
        size = 2.8
    ) +
    scale_linetype_manual(
        values = c("solid" = 1, "dashed" = 5),
        guide = "none"
    ) +
    scale_y_continuous(
        label = percent_format(scale = 1),
        sec.axis = sec_axis(
            ~ 72.5 + .725 * .,
            name = "New  price per barrel",
            label = dollar
        )
    ) +
    annotate(
        "text",
        x = 17,
        y = 0,
        hjust = 1,
        size = 2.5,
        label = "Pre-war situation",
        colour = annot_col
    ) +
    annotate(
        "segment",
        x = 17,
        xend = 19.5,
        y = 0,
        yend = 0,
        arrow = arrow(length = unit(0.15, "cm")),
        colour = annot_col
    ) +
    expand_limits(x = c(0, 20)) +
    scale_colour_gradientn(
        colours = brewer.pal(9, "YlGnBu")[2:8],
        breaks = c(0.01, 0.05)
    ) +
    labs(
        x = "Oil exported from Gulf (in million barrels per day)",
        y = "Price increase for crude oil",
        colour = str_wrap(
            "Deviation of assumed elasticity of demand from mid-point estimate:",
            38
        ),
        title = the_title,
        subtitle = str_wrap(
            "Illustrative price increase for crude oil depends on the price elasticity of demand and the amount of oil that is exported from the Gulf.",
            100
        ),
        caption = the_caption
    ) +
    theme(
        panel.background = element_rect(fill = "grey15"),
        panel.grid.major = element_line(colour = "grey30"),
        panel.grid.minor = element_blank()
    )

svg_png(p2, "../img/0335-lines", w = 8, h = 5)
