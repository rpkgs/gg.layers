GeomMovMean <- ggproto("GeomMovMean", Geom,
  required_aes = c("x", "y"),
  default_aes = aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = NA),
  setup_data = function(data, param) {
    data
  },
  draw_group = function(self, data, panel_params, coord,
                        halfwin = 3, show.diff = FALSE, na.rm = FALSE) {
    if (show.diff) {
      data <- mutate(data, y = y - movmean(y, halfwin))
    } else {
      data <- mutate(data, y = movmean(y, halfwin))
    }
    GeomLine$draw_panel(data, panel_params, coord)
  }
)

#' geom_movmean
#'
#' @inheritParams ggplot2::geom_line
#' @param halfwin halfwin of movmean, [rtrend::movmean()]
#'
#' @example R/examples/ex-geom_movmean.R
#' @export
geom_movmean <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
                         halfwin = 3, show.diff=FALSE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMovMean,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      halfwin = halfwin,
      show.diff = show.diff,
      na.rm = na.rm,
      ...
    )
  )
}
