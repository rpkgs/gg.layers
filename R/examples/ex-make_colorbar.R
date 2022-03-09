library(rcolors)
library(ggplot2)

brks = c(-Inf, -1, 0, 1, 3, 6, 9, Inf)
# brks = 1:10
nbrk = length(brks) - 1
cols = get_color(rcolors$amwg256, nbrk)

spaces = c("right", "left", "top", "bottom") #%>% set_names(., .)

params <- list(
    at = brks, col = cols, height = 1,
    tck = 0.4,
    # padding.left = unit(2, "points"),
    # padding.right = unit(2, "points"),
    space = spaces[1],
    # legend.line = element_line(size = 0.1, linetype = 1, color = "black"),
    # legend.text = element_text(hjust = 0.5),
    legend.text.location = c(0.2, 0.5),
    # legend.text.just = c(0.5, 0.5),

    title = "Title",
    legend.title = element_text(hjust = 0, vjust = 1)
    # legend.box = element_rect(size = 0.5),
    # legend.line = element_line(size = 1),
    # legend.text = list(fontfamily = "Times", cex = 1.1),
    # hjust = 0.5
)

g <- do.call(make_colorbar, params)

# cowplot::plot_grid(plotlist = lst)
p <- ggplot(mtcars, aes(mpg, disp)) + geom_point() +
  facet_wrap(~cyl)
p + g

p <- ggplot(mtcars, aes(mpg, disp)) + geom_point() +
  facet_wrap(~cyl, nrow = 2)
add_colorbar(p, g)

## Test the bottom
params$space = "bottom"
params$title = ""
g2 <- do.call(make_colorbar, params)
add_colorbar(p, g2, space = "bottom",
             title = "(mm/y)",
             legend.title = element_text(hjust = -5, vjust = -3, family = "Times"))

# Another option
title = element_grob_text(element_text(family = "Times", hjust = 1, vjust = 0, size = 12),
  label = "(mm/y)", x = 0.98, y = 0.09)
add_colorbar(p, g2, space = "bottom") %>% add_grob(title)
