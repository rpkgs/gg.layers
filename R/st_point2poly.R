#' st_point2poly
#' 
#' @param xyz A matrix or data frame whose first two columns are x and y;
#'   the optional third column contains values.
#' @param crs A coordinate reference system accepted by [sf::st_crs()].
#'
#' @seealso [terra::rast()], [terra::as.polygons()]
#' 
#' @references
#' 1. https://gis.stackexchange.com/questions/192771/how-to-speed-up-raster-to-polygon-conversion-in-r
#' @export
st_point2poly <- function(xyz, crs = 4326) {
    rast2poly(df2rast(xyz), crs)
}

#' @rdname st_point2poly
#' @export
df2rast <- function(xyz) {
    if (ncol(xyz) < 2) {
        stop("`xyz` must have at least two columns.", call. = FALSE)
    }

    xyz = as.data.frame(xyz)
    xyz = xyz[, seq_len(min(ncol(xyz), 3)), drop = FALSE]
    if (ncol(xyz) == 2) xyz$z = 1L
    names(xyz) = c("x", "y", "z")
    terra::rast(xyz, type = "xyz")
}

#' @rdname st_point2poly
#' @export
rast2poly <- function(r, crs = 4326) {
    terra::crs(r) = sf::st_crs(crs)$wkt
    r %>%
        terra::as.polygons(round = FALSE, aggregate = TRUE, na.rm = TRUE) %>%
        sf::st_as_sf() %>%
        sf::st_make_valid()
}

#' @rdname st_point2poly
#' @export
st_dissolve <- function (x, by = NULL, ...) {
  if (is.null(by) || !(by %in% colnames(x)))
    by = colnames(x)[1]
  x %>% dplyr::group_by_at(by) %>% dplyr::summarise(...)
}
