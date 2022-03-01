library(rcolors)
library(ggplot2)

brks = c(-Inf, -1, 0, 1, 3, 6, 9, Inf)
brks = 1:10
nbrk = length(brks) - 1
cols = get_color(rcolors$amwg256, nbrk)

spaces = c("right", "left", "top", "bottom") #%>% set_names(., .)

g <- make_colorbar(
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
    legend.title = element_text(hjust = 0, vjust = 1),
    # legend.box = element_rect(size = 0.5),
    # legend.line = element_line(size = 1),
    # legend.text = list(fontfamily = "Times", cex = 1.1),
    # hjust = 0.5
)

# cowplot::plot_grid(plotlist = lst)

# Ipaper::write_fig({
#     grid.rect()
#     grid.draw(g)
# }, "a.pdf", 0.7, 4)

##
p <- ggplot(mtcars, aes(mpg, disp)) + geom_point()
p + g
# p2 = add_colorbar(p, g, width = 1)
# write_fig(p2, "a.pdf", 7, 4)
