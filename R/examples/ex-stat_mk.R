library(ggplot2)
library(rtrend)

ggplot(mpg, aes(displ, hwy, colour = drv)) +
    geom_point() +
    stat_mk(size = 1, slope_FUN = slope_mk) +
    geom_smooth(method = "lm", se = FALSE, linetype = 2)
