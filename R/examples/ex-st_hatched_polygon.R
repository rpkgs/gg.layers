library(gg.layers)
library(ggplot2)
data("d_trendPerc")

d = d_trendPerc %>% subset(perc >= 0.99) %>% .[, 1:2]
poly = st_point2poly(d)
hatches = st_hatched_polygon(poly) #
ggplot(hatches) + geom_sf()
