library(gg.layers)
library(ggplot2)
library(rcolors)

data("d_trendPerc")
d_mask <- mutate(d_trendPerc, mask = perc <= 0.99) # %>% as.data.frame()

brks = seq(0.9, 1, 0.025)
nbrk = length(brks) - 1
cols <- get_color(rcolors$amwg256, nbrk)

# the part of not significant
ggplot(data = d_mask, aes(x, y)) +
  geom_raster(aes(fill = perc)) +
  layer_barchart(aes(z = perc), 
    width = unit(0.3, "npc"), 
    height = unit(0.3, "npc"), 
    brks = brks, cols = cols)
