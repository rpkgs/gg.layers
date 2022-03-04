test_that("geom_signHatch works", {
  library(gg.layers)
  library(ggplot2)
  data("d_trendPerc")
  d_mask <- mutate(d_trendPerc, mask = perc <= 0.99) # %>% as.data.frame()

  # the part of not significant
  p1 = ggplot(data = d_mask, aes(x, y)) +
    geom_raster(aes(fill = perc)) +
    geom_signPoint(aes(mask = !mask), fact = 1, shape = 4)

  # significant
  p2 = ggplot(data = d_mask, aes(x, y)) +
    geom_raster(aes(fill = perc)) +
    stat_signPoint(aes(mask = mask), fact = 1)

  expect_silent(print(p1))
  expect_silent(print(p2))
})
