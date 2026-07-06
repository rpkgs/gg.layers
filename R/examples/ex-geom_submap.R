library(gg.layers)
library(ggplot2)

# shp = "Z:/ShapeFiles/ChinaBound2024/shp/bou1_4l_2024.shp"
data("d_trendPerc")

ggplot(d_trendPerc, aes(x, y)) +
  geom_raster(aes(fill = perc)) +
  geom_submap() + # South China Sea inset at the bottom-right corner
  coord_cartesian()
