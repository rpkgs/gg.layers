library(ggplot2)
library(rtrend)

ggplot(mpg, aes(displ, hwy, colour = drv)) +
  geom_point() +
  stat_mk(linewidth = 1, fun_slope = slope_mk) +
  geom_smooth(method = "lm", se = FALSE, linetype = 2)

ggplot(mpg, aes(displ, hwy, colour = drv)) +
  geom_point() +
  geom_mk(linewidth = 1, fun_slope = slope_mk) +
  geom_smooth(method = "lm", se = FALSE, linetype = 2)
