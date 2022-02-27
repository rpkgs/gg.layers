library(grid)
library(rcolors)

{

    devtools::load_all()
    # devtools::load_all("/mnt/i/GitHub/rpkgs/lattice.layers.R")
    write_fig({
            grid.newpage()
            grid.rect()
            grid.circle()
            grid.draw(g)
    }, "a.pdf", 0.58, 5)
}

library(gtable)
library(ggplot2)

p <- ggplot(mtcars, aes(mpg, disp)) + geom_point()

dim(p_table)

# add to the last column


{
    devtools::load_all()
    add_colorbar <- function(p, g, width = 1.4) {
        p_table <- ggplotGrob(p)
        dim = dim(p_table)

        loc = p_table$layout %>% subset(name == "panel")

        width = grid::grobWidth(g) * width
        p2 = p_table %>% gtable_add_cols(width)

        gtable_add_grob(p2, g, l = dim[2] + 1, t = loc$t, b = loc$b)
    }

    



    p2 = add_colorbar(p, g, width = 1.1)
    # write_fig(p2, "a.pdf", 10, 4)
}
