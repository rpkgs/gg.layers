library(ggplot2)

col_prcp = "#3e89be"
col_runoff = "darkorange"

my_theme <-
  theme_dual_axis(col_runoff, col_prcp) +
  theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    # axis.ticks = element_blank(),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0)
  )

## Visualization ---------------------------------------------------------------
dat <- runoff_data
prcp.coef <- guess_prcp_coef(dat$Q, dat$prcp, ratio = 0.5)

ggplot(dat, aes(x = time, Q)) +
  theme_test() +
  geom_prcpRunoff(
    aes(prcp = prcp, color = flood_type),
    params_prcp = list(color = "white", fill = "blue"),
    prcp.coef = prcp.coef, 
    # prcp.qmax = 1200,
    color = col_runoff, linewidth = 0.5
  ) +
  facet_wrap(~flood_type, scales = "free") +
  # scale_y_precipitation(sec.name = "Precipitation (mm)", coef = set_coef) +
  scale_x_datetime(date_labels = "%m/%d") +
  my_theme +
  labs(x = "Date", y = expression("Runoff (m"^"3" * "/s)")) -> p
print(p)
# write_fig(p, "d:/Rplot.pdf", 10, 5)
