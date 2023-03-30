## prepare data
library(gg.layers)
library(ggplot2)
library(rcolors)

data("d_trendPerc")
d_mask <- mutate(d_trendPerc, mask = perc <= 0.99) %>% as_tibble()
n <- nrow(d_mask) * 2
dat <- rbind(cbind(type = "a", d_mask), cbind(type = "b", d_mask)) %>%
  mutate(val = rnorm(n))

brks <- seq(0.9, 1, 0.025)
nbrk <- length(brks) - 1
cols <- get_color(rcolors$amwg256, nbrk)

## option1
# the part of not significant
ggplot(data = dat, aes(x, y)) +
  geom_raster(aes(fill = perc)) +
  layer_barchart(aes(z = val),
    width = unit(0.3, "npc"),
    height = unit(0.3, "npc"),
    brks = brks, cols = cols
  ) +
  facet_wrap(~type)

## option2
func <- function(data, ...) {
  add_barchart(data$z, brks, cols, ...)
}

ggplot(data = dat, aes(x, y)) +
  geom_raster(aes(fill = perc)) +
  geom_annotation_func(aes(z = val), plot.fun = func) +
  facet_wrap(~type)
