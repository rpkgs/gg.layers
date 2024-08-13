library(ggplot2)
library(data.table)

d = data.table(x = 1:10, y = 1:10)
d_span = data.table(xmin = c(1, 4), xmax = c(3, 5), group = 1:2)

ggplot(d, aes(x, y)) + 
  geom_point() + 
  geom_hspan(data = d_span, aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, group = group), 
    alpha = 0.2, fill = "yellow")
