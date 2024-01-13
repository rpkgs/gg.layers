## https://github.com/statnmap/HatchedPolygons

#' st_hatched_polygon
#'
#' @inheritParams graphics::polygon
#' @param density the density of shading lines, in lines per inch. The default
#' value of NULL means that no shading lines are drawn. A zero value of density
#' means no shading nor filling whereas negative values and NA suppress shading
#' (and so allow color filling).
#' @param x sf polygon object
#'
#' @return An hatched area, sf lines
#'
#' @references
#' 1. <https://github.com/statnmap/HatchedPolygons>
#' 2. <https://statnmap.github.io/HatchedPolygons/>
#' 3. <https://statnmap.github.io/HatchedPolygons/articles/leaflet_shading_polygon.html>
#'
#' @example R/examples/ex-st_hatched_polygon.R
#'
#' @export
st_hatched_polygon <- function(x, density = 2, angle = 45, fillOddEven = FALSE) {
  # if (is(x, "SpatialPolygons")) {
  # } else if (sf::st_is(x, c("POLYGON", "MULTIPOLYGON"))[1]) {
  #   # x <- as(x, "Spatial")
  # } else {
  #   stop("Not a sp::SpatialPolygons or sf::*POLYGON object")
  # }
  geoms = sf::st_geometry(x) # POLYGON, sfg
  n = length(geoms)
  ## convert sfc_MULTIPOLYGON into sfc_POLYGON
  if ("sfc_MULTIPOLYGON" %in% class(geoms)) {
    geoms <- sf::st_cast(geoms, "POLYGON")
  }
  if (length(density) != n) density <- rep(density, n, n)
  if (length(angle) != n) angle <- rep(angle, n, n)

  sf_lines <- list()
  for (j in 1:n) {
    sf_lines[[j]] <- polygonRingHolesLines(geoms[[j]],
      density = density[j], angle = angle[j], ID = j, fillOddEven = fillOddEven)
  }
  do.call(rbind, sf_lines) %>%
    sf::st_set_crs(sf::st_crs(x)) %>% sf::st_make_valid()
}

#' Get lines of one Polygons feature
#'
#' @inheritParams graphics::polygon
#' @param geom An object of class sf POLYGON
#' @param ID Number or string identifying the Polygon inside Polygons
#'
#' @keywords internal
#' @return sf lines object
polygonRingHolesLines <- function(geom, density = 0.5, angle = 45, ID = 1, fillOddEven = FALSE)
{
  if (is.null(density)) return(NULL)
  # if (!is(Sr, "Polygons")) stop("Not an Polygons object")
  # 第一个geometry是有效的，其余是holes

  # Transform polygon as parallel lines
  lines.hatch <- polygon.fullhatch(geom[[1]],
    density = density, angle = angle, fillOddEven = fillOddEven)
  if (is.null(lines.hatch)) return(NULL)

  # Transform to sf
  lines = apply(lines.hatch, 1, \(x) list(cbind(c(x[1], x[3]), c(x[2], x[4])))) %>%
    do.call(c, .)

  sf_line = sf::st_sf(ID=ID, geometry = sf::st_sfc(sf::st_multilinestring(lines)))
  sf::st_agr(sf_line) = "constant"
  sf::st_intersection(sf_line, geom) # line in poly
}
