test_that("geom_boxplot2 works", {
    library(ggplot2)
    
    p <- ggplot(mpg, aes(class, hwy)) 
    p1 <- p + geom_boxplot() + 
        ggtitle("(a) geom_boxplot") + 
        theme(axis.text.x = element_blank(), 
              axis.title.x = element_blank(), 
              axis.ticks.x = element_blank())
    
    p2 <- p + geom_boxplot2(width.errorbar = 0.5) +
        ggtitle("(b) geom_boxplot2")
    expect_silent(print(p1))
    expect_silent(print(p2))
    expect_equal(layer_scales(p2)$y$range$range, c(14, 36))

    # p + geom_boxplot2()
    p3 = p + stat_summary(fun.data = box_qtl, geom = "errorbar", width = 0.5, color = "red") + 
        geom_boxplot2(show.errorbar = FALSE)
    expect_silent(print(p3))
})
