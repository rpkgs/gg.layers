library(gg.layers)
library(ggplot2)
data("d_trendPerc")

d_mask <- mutate(d_trendPerc, mask = perc <= 0.99) # %>% as.data.frame()

# the part of not significant
ggplot(data = d_mask, aes(x, y)) +
  geom_raster(aes(fill = perc)) +
  geom_signPoint(aes(mask = !mask), fact = 2, shape = 4)

# significant
ggplot(data = d_mask, aes(x, y)) +
  geom_raster(aes(fill = perc)) +
  geom_signPoint(aes(mask = mask), fact = 1)
