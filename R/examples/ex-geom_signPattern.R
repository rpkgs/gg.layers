library(gg.layers)
library(ggplot2)
data("d_trendPerc")

d_mask <- mutate(d_trendPerc, mask = perc <= 0.99) # %>% as.data.frame()

# significant part; geom_sf_pattern still has bug unsolved.
ggplot() +
  geom_raster(data = d_trendPerc, aes(x, y, fill = perc)) +
  stat_signPattern(data = d_mask, aes(x, y, mask = mask),
                   fill = "transparent", color = "red",
                   pattern_density = 0.02)

# insignificant
ggplot() +
    geom_raster(data = d_trendPerc, aes(x, y, fill = perc)) +
    stat_signPattern(data = d_mask, aes(x, y, mask = !mask),
                     fill = "transparent", color = "red",
                     pattern_density = 0.02) #-> p
# Ipaper::write_fig(p, "temp.pdf")
