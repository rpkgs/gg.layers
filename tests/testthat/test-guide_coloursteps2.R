test_that("geom_raster_filled works", {
  expect_no_error({
    df <- expand.grid(X1 = 1:10, X2 = 1:10)
    df$value <- df$X1 * df$X2

    ggplot(df, aes(X1, X2, z = value)) +
      geom_raster_filled(breaks = c(-Inf, 10, 25, 50, Inf))
  })
})

test_that("guide_coloursteps2 works", {

  expect_no_error({
    df <- expand.grid(X1 = 1:10, X2 = 1:10)
    df$value <- df$X1 * df$X2

    p <- ggplot(df, aes(X1, X2)) +
      geom_tile(aes(fill = value))

    # This can be changed with the `even.steps` argument
    p2 <- p + scale_fill_binned(
      breaks = c(10, 25, 50),
      guide = guide_coloursteps2(even.steps = TRUE, barheight = unit(0.8, "npc"))
    ) + theme(legend.title = element_blank())
    p2
  })
})
