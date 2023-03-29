## use str_mk in ggplot
library(ggplot2)

d <- data.frame(
  x = 1:3, y = 1:3,
  varname = c("T_min", "T_max", "T_avg")
)

ggplot(d, aes(x, y)) +
  geom_point() +
  geom_richtext(aes(label = varname), x = 2, y = 2) +
  facet_wrap(~varname, labeller = label_mk) +
  theme(
    strip.text = element_markdown(face = "bold")
  )
