#' geom_hspan
#'
#' @description
#' - `xmin`, `xmax`
#' - `ymin`, `ymax`: optional 
#' 
#' @inheritParams ggplot2::geom_polygon
#' 
#' @example R/examples/ex-geom_hspan.R
#' @export
geom_hspan <- function(mapping = NULL, data = NULL,
                       stat = "hspan", position = "identity",
                       ...,
                       ymin = -Inf, ymax = Inf, 
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPolygon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      ymin = ymin, ymax = ymax,
      na.rm = na.rm,
      ...
    )
  )
}

#' @import ggplot2
StatHspan <- ggproto("StatHspan", Stat,
  required_aes = c("xmin", "xmax"),
  compute_group = function(data, scales, ymin = -Inf, ymax = Inf) {
    # x = data$x
    # n = nrow(data) # 需要是偶数
    # xmin = x[seq(1, n, 2)]
    # xmax = x[seq(2, n, 2)]
    xmin = data$xmin
    xmax = data$xmax
    x = c(xmin, xmin, xmax, xmax, xmin)
    y = c(ymin, ymax, ymax, ymin, ymin)
    # cbind(dplyr::select(data[1, ], -x), x, y, row.names = NULL)
    cbind(data, x, y, row.names = NULL)
  }
)

#' @export
stat_hspan <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity",
                       ymin = -Inf, ymax = Inf, 
                       na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatSpike, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(ymin = ymin, ymax = ymax, na.rm = na.rm, ...)
  )
}
