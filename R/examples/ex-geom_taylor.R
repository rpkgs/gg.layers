library(ggplot2)
library(dplyr)

data = dummy_model %>%
  group_by(model, variable) %>%
  group_modify(~taylor_data(.$obs, .$mod)) #%>% as.data.table()

mar = 0.01
p = ggplot(data) +
  geom_taylor(aes2(sd.obs, sd.mod, R, color = model), obs.colour = "black", obs.size = 5) +
  # geom_point(aes(color = model), size = 5) +
  facet_wrap(~variable) +
  labs(color = NULL) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(1, 1) - c(1, 1/3)*mar,
    legend.justification = c(1, 1)
  )
p
# p = last_plot()
# Ipaper::write_fig(p, "Rplot.pdf")
