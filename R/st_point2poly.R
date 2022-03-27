#' st_point2poly
#' 
#' @inheritParams raster::rasterFromXYZ
#' @param crs one of (i) character: a string accepted by GDAL, (ii) integer, a
#' valid EPSG value (numeric), or (iii) an object of class crs.
#' 
#' @seealso [raster::rasterFromXYZ()]
#' 
#' @references
#' 1. https://gis.stackexchange.com/questions/192771/how-to-speed-up-raster-to-polygon-conversion-in-r
#' @export
st_point2poly <- function(xyz, crs = 4326) {
    r = df2rast(xyz)
    rast2poly(r, crs) #%>% st_dissolve(by = "z")
}

#' @rdname st_point2poly
#' @export 
df2rast <- function(xyz) {
    # first three columns
    if (ncol(xyz) == 2) {
        xyz %<>% cbind(z = 1L)
    }
    colnames(xyz) = c("x", "y", "z")
    raster::rasterFromXYZ(xyz)
}

#' @rdname st_point2poly
#' @export 
rast2poly <- function(r, crs = 4326) {
    sf_poly = suppressWarnings({
        sf::st_as_sf(stars::st_as_stars(r),
            as_points = FALSE, merge = TRUE
        ) %>% sf::st_make_valid() # %>% sf::as_Spatial()
    })
    sf::st_crs(sf_poly) = crs
    sf_poly
}

#' @rdname st_point2poly
#' @export
st_dissolve <- function (x, by = NULL, ...) {
  if (is.null(by) || !(by %in% colnames(x)))
    by = colnames(x)[1]
  x %>% dplyr::group_by_at(by) %>% dplyr::summarise(...)
}
