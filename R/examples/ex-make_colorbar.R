library(rcolors)
library(ggplot2)

brk = c(-Inf, -1, 0, 1, 3, 6, 9, Inf)
nbrk = length(brk) - 1
cols = get_color(rcolors$amwg256, nbrk)

g <- make_colorbar(
    at = brk, col = cols, height = 1,
    tck = 0.4,
    space = "right",
    legend.text.location = c(0.3, 0.5),
    legend.text.just = c(0.5, 0.5),
    # legend.text = list(fontfamily = "Times", cex = 1.1),
    hjust = 0.05
)

# Ipaper::write_fig({
#     grid.rect()
#     grid.draw(g)
# }, "a.pdf", 2.7, 4)

##
p <- ggplot(mtcars, aes(mpg, disp)) + geom_point()
p + g

# p2 = add_colorbar(p, g, width = 1)
# write_fig(p2, "a.pdf", 7, 4)
