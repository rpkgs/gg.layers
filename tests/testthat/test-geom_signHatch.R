test_that("geom_signHatch works", {
  library(gg.layers)
  library(ggplot2)
  data("d_trendPerc")

  d_mask = mutate(d_trendPerc, mask = perc <= 0.99) %>% as.data.frame()

  p = ggplot() +
    geom_raster(data = d_trendPerc, aes(x, y, fill = perc)) +
    # geom_sf(data = poly) +
    geom_signHatch(data = d_mask, aes(x, y, mask = mask), color = "red")
  expect_silent(print(p))

  p = ggplot() +
    geom_raster(data = d_trendPerc, aes(x, y, fill = perc)) +
    stat_signHatch(data = d_mask, aes(x, y, mask = mask), color = "red")
  expect_silent(print(p))
  # expect_equal(2 * 2, 4)
})
