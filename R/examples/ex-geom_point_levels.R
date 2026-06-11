library(ggplot2)

# fake gridded data with a continuous variable `z`
set.seed(1)
d <- expand.grid(x = 1:20, y = 1:20)
d$z <- with(d, sin(x / 3) + cos(y / 3) + rnorm(nrow(d), 0, 0.2))

# default breaks: pretty(z, 7)
ggplot(d, aes(x, y, z = z)) +
  geom_point_levels(size = 3)

# custom breaks (the same `brks` style as geom_raster_filled)
brks <- seq(-2, 2, 0.5)
ggplot(d, aes(x, y, z = z)) +
  geom_point_levels(breaks = brks, size = 3) +
  scale_color_viridis_d()
