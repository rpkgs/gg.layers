test_that("geom_raster_filled works", {
  library(ggplot2)

  expect_no_error({
    df <- expand.grid(X1 = 1:10, X2 = 1:10)
    df$value <- df$X1 * df$X2

    p = ggplot(df, aes(X1, X2, z = value)) +
      geom_raster_filled(breaks = c(-Inf, 10, 25, 50, Inf))
    print(p)
  })
})

test_that("guide_coloursteps2 works", {
  library(ggplot2)
  expect_no_error({
    df <- expand.grid(X1 = 1:10, X2 = 1:10)
    df$value <- df$X1 * df$X2

    p <- ggplot(df, aes(X1, X2)) +
      geom_tile(aes(fill = value))

    # This can be changed with the `even.steps` argument
    p2 <- p +
      scale_fill_binned(
        breaks = c(10, 15, 25, 50),
        guide = guide_coloursteps2(hjust = -2)) +
      theme(legend.title = element_blank(),
            legend.margin = margin(l = -2))
    # print(p2)
    # Ipaper::write_fig(p2, "a.pdf", 10, 5)
  })
})
