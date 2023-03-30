test_that("st_hatched_polygon works", {
  library(gg.layers)
  library(ggplot2)
  data("d_trendPerc")

  # ggplot(hatches) + geom_sf()
  expect_no_error({
    d = d_trendPerc %>% subset(perc >= 0.99) %>% .[, 1:2]
    poly = st_point2poly(d)
    hatches = st_hatched_polygon(poly) #
    print(hatches)
  })
  # expect_true("sfc_MULTILINESTRING" %in% class(hatches$geometry))
})
