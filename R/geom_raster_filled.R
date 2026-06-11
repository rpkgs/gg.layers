#' geom_raster_filled
#' @inheritParams ggplot2::geom_raster
#' @inheritParams ggplot2::geom_contour
#' @export
geom_raster_filled <- function(mapping = NULL, data = NULL,
                               stat = "raster_levels", position = "identity",
                               ...,
                               breaks = NULL,
                               hjust = 0.5,
                               vjust = 0.5,
                               interpolate = FALSE,
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRaster,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      breaks = breaks,
      hjust = hjust,
      vjust = vjust,
      interpolate = interpolate,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_raster_filled
#' @export
stat_raster_levels <- function(mapping = NULL, data = NULL,
                               geom = "raster", position = "identity",
                               ...,
                               # bins = NULL,
                               # binwidth = NULL,
                               breaks = NULL,
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatRasterLevels,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      # bins = bins,
      # binwidth = binwidth,
      breaks = breaks,
      na.rm = na.rm,
      ...
    )
  )
}
