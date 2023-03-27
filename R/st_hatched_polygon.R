## HatchedPolygons Copyright (C) 2021 Sébastien Rochette
# This program comes with ABSOLUTELY NO WARRANTY; for details type 'show w'.
# This is free software, and you are welcome to redistribute it
# under certain conditions; type 'show c' for details.

## https://github.com/statnmap/HatchedPolygons

# TODO: remove the dependency of `sp`, use `sf` instead

#' st_hatched_polygon
#' 
#' @inheritParams graphics::polygon 
#' @param density the density of shading lines, in lines per inch. The default
#' value of NULL means that no shading lines are drawn. A zero value of density
#' means no shading nor filling whereas negative values and NA suppress shading
#' (and so allow color filling).
#' @param x SpatialPolygons* from library sp
#' @param type Controls the type of the returned object:
#' - `sf`: sf object of `MULTILINESTRING`
#' - `sp`: sp object of [sp::SpatialLinesDataFrame()]
#' 
#' @return An hatched area for assigned `type`
#' 
#' @references
#' 1. <https://github.com/statnmap/HatchedPolygons>
#' 2. <https://statnmap.github.io/HatchedPolygons/>
#' 3. <https://statnmap.github.io/HatchedPolygons/articles/leaflet_shading_polygon.html>
#' 
#' @example R/examples/ex-st_hatched_polygon.R
#' 
#' @import sp
#' @importFrom methods is as slot
#' @export
st_hatched_polygon <- function(x, density = 2, angle = 45, fillOddEven = FALSE, type = "sf") {
  if (is(x, "SpatialPolygons")) {
  } else if (sf::st_is(x, c("POLYGON", "MULTIPOLYGON"))[1]) {
    x <- as(x, "Spatial")
  } else {
    stop("Not a sp::SpatialPolygons or sf::*POLYGON object")
  }

  polys <- x@polygons
  n <- length(polys)
  pO <- x@plotOrder

  if (length(density) != n) density <- rep(density, n, n)
  if (length(angle) != n) angle <- rep(angle, n, n)

  all.Lines <- list()
  all.Lines.ID <- numeric(0)

  for (j in pO) {
    poly = polys[[j]]
    id = poly@ID
    sp_lines <- polygonRingHolesLines(poly, 
      density = density[j], angle = angle[j], ID = id, fillOddEven = fillOddEven)

    if (length(sp_lines) == 0) next()

    ID = rep(id, length(sp_lines))
    all.Lines.ID %<>% c(ID)
    all.Lines %<>% c(sp_lines)
  }
  
  SpatialLinesDF <- SpatialLinesDataFrame(SpatialLines(all.Lines),
    data = data.frame(ID = all.Lines.ID), match.ID = FALSE)

  if (type == "sf") {
    ans = sf::st_as_sf(SpatialLinesDF) %>% sf::st_make_valid()
    sf::st_crs(ans) = 4326
  } else {
    ans = SpatialLinesDF
  }
  ans
}

#' Get SpatialLines of one Polygons feature
#'
#' @inheritParams graphics::polygon
#' @param Sr An object of class Polygons
#' @param ID Number or string identifying the Polygon inside Polygons
#'
#' @import sp
#' @importFrom methods is
#' @importFrom methods slot
#' 
#' @keywords internal
#' @return Spatial `Lines` object
polygonRingHolesLines <- function(Sr, density = 0.5, angle = 45, ID = 1, fillOddEven = FALSE) 
{
  if (!is(Sr, "Polygons")) stop("Not an Polygons object")
  if (is.null(density)) return(list())
  
  pO <- Sr@plotOrder
  polys <- Sr@Polygons

  all.Lines <- list()
  for (i in pO) {
    poly = polys[[i]]
    if (!slot(poly, "hole")) {
      # Transform polygon as parallel lines
      lines.hatch <- polygon.fullhatch(poly@coords, 
        density = density, angle = angle, fillOddEven = fillOddEven)

      if (is.null(lines.hatch)) next()
      # Transform as SpatialLines
      lines <- apply(lines.hatch, 1, function(x) Line(cbind(c(x[1], x[3]), c(x[2], x[4]))))
      Lines.i <- SpatialLines(list(Lines(lines, ID = i)))

      # Clean Lines if over a "hole"
      # TODO: warning at here, use sf instead
      suppressWarnings({
        Lines.i.holes <- rgeos::gIntersection(Lines.i, SpatialPolygons(list(Sr)),
          drop_lower_td = TRUE
        )
      })

      if (!is.null(Lines.i.holes)) {
        lines = Lines.i.holes@lines[[1]]
        lines@ID <- paste0(ID, ".", i)
        all.Lines %<>% c(lines)
      }
    }
  }
  return(all.Lines)
}
