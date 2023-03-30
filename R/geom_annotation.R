#' geom_annotation
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::annotation_custom
#' 
#' @param data A tibble with the column of `grob`
#' 
#' @example R/examples/ex-geom_anno.R
#' 
#' @export
geom_annotation <- function(
    mapping = NULL, data = NULL,
    stat = "identity", position = "identity",
    xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5, 
    just = c(0, 0),
    # x = 0, y = 0, width = unit(0.5, "npc"), height = unit(0.5, "npc"), 
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAnnotation,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      # x = x, y = y, width = width, height = height,
      xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
      just = just, ...
    )
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomAnnotation <- ggproto("GeomAnnotation", Geom,
  extra_params = "",
  handle_na = function(data, params) {
    data
  },

  draw_panel = function(data, panel_params, coord,
      xmin, xmax, ymin, ymax, just) {
      # x, y, width, height, just) {
    grob = data$grob[[1]]
    # corners <- ggplot2:::data_frame0(
    #   x = c(xmin, xmax),
    #   y = c(ymin, ymax),
    #   .size = 2
    # )
    # data <- coord$transform(corners, panel_params)
    # x_rng <- range(data$x, na.rm = TRUE)
    # y_rng <- range(data$y, na.rm = TRUE)
    g <- grid::grobTree(as.grob(grob),
      vp = grid::viewport(
        x = xmin, y = ymin, width = xmax - xmin, height = ymax - ymin, just = c(0, 0)
        # x = x, y = y, width = width, height = height, just = just
      )
    )
    g
  },
  required_aes = c("grob"),
  # default_aes = aes_(x = 0, y = 0, width = unit(0.5, "npc"), height = unit(0.5, "npc"))
  default_aes = aes_(xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5)
)
