library(rcolors)
library(ggplot2)
library(magrittr)

df = data.frame(x = 1:10, y = 1:10, z = 1:10)

brks <- c(2, 4, 6, 8) %>% c(-Inf, ., Inf) # 这里要包含
nbrk <- length(brks) - 1
cols = get_color(rcolors$amwg256, nbrk)

ggplot(df, aes(x, y, z = x)) +
  stat_cut(aes(color = after_stat(level)), breaks = brks, geom = "point") +
  scale_color_manual(
    values = cols,
    guide = guide_coloursteps2(title = "lgd", barheight = unit(0.8, "npc"))
  )
