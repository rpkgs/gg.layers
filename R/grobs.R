#' grobs
#' @param options parameters (except `plotlist`) of [cowplot::plot_grid()]
#' @keywords internal
#' @export
grobs <- function(..., options = list(nrow = 1)) {
  params = c(plotlist = list(...), options)
  do.call(cowplot::plot_grid, params)
}


#' add grob to a plot
#' 
#' @param ... grob objects
#' 
#' @example R/examples/ex-add_grob.R
#' 
#' @import ggplotify
#' @export
add_grob <- function(p, ..., ggplot = TRUE) {
  g = grid::grobTree(
    as.grob(p),
    grid::grobTree(...,
    vp = grid::viewport()
    )
  )
  if (ggplot) as.ggplot(g) else g
}


annotation_custom2 <- function(grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) {
  layer(
    data = data, stat = StatIdentity, position = PositionIdentity,
    geom = ggplot2:::GeomCustomAnn,
    inherit.aes = TRUE, params = list(
      grob = grob,
      xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax
    )
  )
}
