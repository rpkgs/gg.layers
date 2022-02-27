test_that("draw.colorkey works", {
    expect_true({
        # library(testthat)
        # library(Ipaper)
        # library(lattice)
        library(grid)
        library(gridExtra)
        brks_SOS  <- c(-Inf, seq(110, 150, 5), Inf)
        space = "right"; heigh = 1

        test_legend <- function(space = "right", heigh = 1) {
            key <- list(
                at = brks_SOS,
                space = space, height = heigh,
                tri.upper = 0.05,  tri.lower = 0.05,
                labels=list(cex=1.2, fontface='bold'),
                rect = list(col = "black", lwd = 0.3),
                unit = "%",
                unit.adj = 0.3
            )
            g <- draw.colorkey2(key, draw = TRUE)
            grid.newpage()
            grid.draw(g)
            g
        }

        p11 <- test_legend('right')
        p12 <- test_legend('left')
        p1  <- arrangeGrob(p11, p12, nrow = 1)

        p21 <- test_legend('top', 0.9)
        p22 <- test_legend('bottom', 0.9)
        p2 <- arrangeGrob(p21, p22, nrow = 2)

        g <- grid.arrange(p1, p2, nrow = 2)
        write_fig(g, "lgd.pdf", 6, 12, show = FALSE)
        write_fig(g, "lgd.pdf", devices = c("png", "tif", "jpg"),6, 12, show = FALSE)
        file.remove(list.files(".", "lgd", full.names = TRUE))
        # }
        TRUE
    })

    ## sp
    expect_true({
        library(sp)
        demo(meuse, ask = FALSE, echo = FALSE)
        # spplot(meuse, c("ffreq"), col.regions= "black",
        #        pch=c(1,2,3), key.space=list(x=0.1,y=.95,corner=c(0,1)))
        p1 <- spplot(meuse.grid)
        p2 <- spplot(meuse.grid, colorkey = list(space = "left"))
        p3 <- spplot(meuse.grid, colorkey = list(space = "top"))
        p4 <- spplot(meuse.grid, colorkey = list(space = "bottom"))
        grid.arrange(p1, p2, p3, p4, nrow = 4)
        TRUE
    })
})
