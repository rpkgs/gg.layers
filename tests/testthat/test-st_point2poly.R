test_that("st_point2poly converts a regular point grid", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  xyz = expand.grid(x = 1:3, y = 1:2)
  poly = st_point2poly(xyz, crs = 3857)

  expect_s3_class(poly, "sf")
  expect_equal(nrow(poly), 1L)
  expect_equal(poly$z, 1L)
  expect_equal(sf::st_crs(poly)$epsg, 3857)
  expect_true(all(sf::st_is_valid(poly)))
  expect_equal(as.numeric(sf::st_area(poly)), 6)
})

test_that("st_point2poly preserves values and removes NA cells", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  xyz = expand.grid(x = 1:3, y = 1:2)
  xyz$z = c(1, 1, NA, 1, 2, 2)
  poly = st_point2poly(xyz, crs = 3857)

  expect_equal(sort(poly$z), c(1, 2))
  expect_equal(as.numeric(sum(sf::st_area(poly))), 5)
})

test_that("df2rast validates its input", {
  skip_if_not_installed("terra")

  expect_error(df2rast(data.frame(x = 1)), "at least two columns")
  expect_s4_class(df2rast(data.frame(x = 1:2, y = 1:2)), "SpatRaster")
})
