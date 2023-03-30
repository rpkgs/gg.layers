#' geom_annotation
#' 
#' @inheritParams geom_annotation
#' @inheritParams grid::viewport
#' 
#' @param ... other parameters to `plot.fun`
#' 
#' @example R/examples/ex-geom_anno_func.R
#' @export
geom_annotation_func <- function(
    mapping = NULL, data = NULL,
    plot.fun = NULL, ..., 
    x = 0, y = 0, width = unit(0.5, "npc"), height = unit(0.5, "npc"), just = c(0, 0)
    ) {
  fun <- function(data, coords) {
    p <- plot.fun(data, ...)
    # p <- add_barchart(data$z, brks, cols, fontsize = fontsize, theme = theme, ...)
    
    g <- grid::grobTree(as.grob(p),
      vp = grid::viewport(
        x = x, y = y, just = just,
        width = width, height = height
      )
    )
    g
  }
  grid_panel(fun, mapping, data)
}
