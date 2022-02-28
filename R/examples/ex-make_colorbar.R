library(rcolors)
library(ggplot2)

brks = c(-Inf, -1, 0, 1, 3, 6, 9, Inf)
nbrk = length(brks) - 1
cols = get_color(rcolors$amwg256, nbrk)

g <- make_colorbar(
    at = brks, col = cols, height = 1,
    tck = 0.4,
    padding.left = unit(10, "points"),
    padding.right = unit(0, "points"),
    space = "right",
    legend.text.location = c(0.3, 0.5),
    legend.text.just = c(0.5, 0.5),
    # legend.text = list(fontfamily = "Times", cex = 1.1),
    hjust = 0.5
)

Ipaper::write_fig({
    grid.rect()
    grid.draw(g)
}, "a.pdf", 0.7, 4)

##
p <- ggplot(mtcars, aes(mpg, disp)) + geom_point()
p + g

theme_set(theme_grey())

theme = theme_grey()
calc_element("legend.text", theme)

# p2 = add_colorbar(p, g, width = 1)
# write_fig(p2, "a.pdf", 7, 4)
