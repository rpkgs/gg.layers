library(ggplot2)
df <- CMIP6_HWD

p <- ggplot(df, aes(year, value, color = prob, fill = prob))

# both line and fill
p + geom_interval(alpha = 0.2, alpha.line = 1, 
  linewidth = 0.7, linetype = 2, # for ensemble line
  param_slope = list(linewidth = 1, alpha = 1, linetype = 1), # slope line
  interval = 0.8, fun_middle = "mean")

# only line
p + geom_interval(
  fill = "NA",
  alpha = 0.4, interval = 0.8, fun_middle = "mean", linewidth = 0.4
) +
  theme(legend.key = element_rect(fill = "NA"))

# only fill
p + geom_interval(
  color = "NA", key_glyph = "polygon2",
  alpha = 0.4, interval = 0.8, fun_middle = "mean", linewidth = 0.4
) +
  theme(legend.key = element_rect(fill = "NA"))
