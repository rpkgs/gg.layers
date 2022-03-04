library(gg.layers)
library(ggplot2)
data("d_trendPerc")

d_mask <- mutate(d_trendPerc, mask = perc <= 0.99) %>% as.data.frame()

ggplot() +
    geom_raster(data = d_trendPerc, aes(x, y, fill = perc)) +
    # geom_sf(data = shp) +
    geom_signHatch(data = d_mask, aes(x, y, mask = mask), color = "red")
