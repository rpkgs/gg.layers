test_that("st_hatched_polygon works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  data("d_trendPerc")
  d = d_trendPerc %>% subset(perc >= 0.99) %>% .[, 1:2]
  poly = st_point2poly(d)
  hatches = st_hatched_polygon(poly)

  expect_s3_class(hatches, "sf")
})

test_that("st_hatched_polygon handles mapview data", {
  skip_if_not_installed("mapview")

  franconia = mapview::franconia
  expect_no_error(st_hatched_polygon(franconia, density = 10, angle = 45))
})
