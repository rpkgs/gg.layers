library(ggplot2)
library(data.table)

dates <- seq(as.Date("2010-01-01"), length.out = 32, by = "day")
dat <- data.table(mtcars) %>% cbind(date = dates, .)
dat$cyl <- as.factor(dat$cyl)
# table(dat$cyl)

ggplot(dat, aes(date, mpg, color = cyl)) +
  stat_gof2(aes(obs = mpg, sim = wt), x = 0, y = 1) +
  geom_point() +
  facet_wrap(~cyl)

## Example 2
dat = GPP_US_MMS
ggplot(dat, aes(date, GPP)) +
  geom_line() +
  stat_gof2(aes(obs=GPP, sim=SM), x = 0, y = 1)

## 
dat = data.table(
  obs = c(10, 12, 15, 14, 13, 16, 18, 20, 19, 22),
  sim = c(11, 13, 14, 15, 12, 17, 17, 21, 20, 20),
  sim2 = c(11, 13, 14, 15, 12, 17, 17, 21, 20, 25),
  group = rep(c("A", "B"), each = 5)
)

showtext::showtext_auto()
library(Ipaper)

fmt_gof <- "*NSE* = {str_num(NSE,2)}, *R^2* = {str_num(R2, 2)}"

p = ggplot(dat, aes(obs, sim)) +
  geom_point() +
  stat_gof2(aes(obs = obs, sim = sim), x = 0, y = 1, eval.flood = TRUE, show.bias = FALSE, label.format = fmt_gof, size = 6) + 
  stat_gof2(aes(obs = obs, sim = sim2), x = 0, y = 0.9, eval.flood = TRUE, show.bias = FALSE, label.format = fmt_gof, size = 6)

write_fig(p, "d:/Rplot.pdf", 10, 5)
