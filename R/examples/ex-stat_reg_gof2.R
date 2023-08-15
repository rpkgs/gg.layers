library(ggplot2)
library(data.table)

dates <- seq(make_date(2010), length.out = 32, by = "day")
dat <- data.table(mtcars) %>% cbind(date = dates, .)
dat$cyl <- as.factor(dat$cyl)
# table(dat$cyl)

ggplot(dat, aes(date, mpg, color = cyl)) +
    stat_gof2(aes(obs = mpg, sim = wt), x = 0, y = 1) +
    geom_point() +
  facet_wrap(~cyl)
