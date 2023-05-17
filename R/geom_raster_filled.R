#' geom_raster_filled
#'
#' @inheritParams ggplot2::geom_raster
#' @inheritParams ggplot2::geom_contour
#' @export
geom_raster_filled <- function(mapping = NULL, data = NULL,
                               stat = "raster_filled", position = "identity",
                               ...,
                               breaks = NULL,
                               hjust = 0.5,
                               vjust = 0.5,
                               interpolate = FALSE,
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {
  # check_number_decimal(hjust)
  # check_number_decimal(vjust)
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
stat_raster_filled <- function(mapping = NULL, data = NULL,
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
    stat = StatRasterFilled,
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

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatRasterFilled <- ggproto("StatRasterFilled", Stat,
  required_aes = c("x", "y", "z"),
  default_aes = aes(fill = after_stat(level)), # order = after_stat(level),
  # z and weight get dropped during statistical transformation
  dropped_aes = c("z"),

  setup_params = function(data, params) {
    if (is.null(params$breaks)) {
      params$breaks = pretty(data$z, 7)
    }
    params
  },

  setup_data = function(data, params) {
    data %>% mutate(level = cut(z, params$breaks))
  },
  compute_group = function(data, scales, breaks = NULL) {
    data
  }
)
