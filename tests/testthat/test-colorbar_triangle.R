test_that("colourbar_triangle works", {
    library(ggplot2)
    
    g <- ggplot(mtcars, aes(mpg, wt)) +
    geom_point(aes(colour = drat))

    p = g + scale_colour_viridis_c(
      limits = c(3, 5), oob = scales::oob_squish,
      guide = colourbar_triangle()
    )
    expect_silent(print(p))
})
