library(ggplot2)
library(data.table)

dat <- data.table(mtcars)
dat$cyl <- as.factor(dat$cyl)
table(dat$cyl)

ggplot(dat, aes(wt, mpg, color = cyl)) +
    geom_point() +
    stat_reg_gof(data = dat[cyl != 4], y = 1, mar = 0, position = "none") +
    facet_wrap(~cyl)

ggplot(dat, aes(wt, mpg, color = cyl)) +
  geom_point() +
  stat_reg_gof(data = dat[cyl != 4], y = 1,
               position = "dodge",
               height.factor = 1.2,
               unit = "gC m^-2 d^-1")

ggplot(dat, aes(wt, mpg, color = cyl)) +
  geom_point() +
  stat_reg_gof(data = dat[cyl != 4], y = 1,
               position = "none",
               height.factor = 1.2,
               unit = "gC m^-2 d^-1") +
  facet_wrap(~cyl)


ggplot(dat, aes(wt, mpg, color = cyl)) +
    geom_point() +
    stat_gof(x = 0, y = 1) +
    # stat_reg_gof(data = subset(dat, cyl == 4), y = 1, color = "red") +
    # stat_reg_gof(data = subset(dat, cyl == 6), y = 0.8) +
    facet_wrap(~cyl)
