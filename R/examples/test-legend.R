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
# add to the last column
p2 = add_colorbar(p, g, width = 1.1)
# write_fig(p2, "a.pdf", 10, 4)
