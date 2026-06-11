#' South China Sea boundary (南海十段线)
#'
#' An `sf` object with the "十段线" (ten-segment line) national boundary in the
#' South China Sea region, used as the default content of [geom_submap()].
#'
#' @format An `sf` object with a single `MULTILINESTRING` feature (WGS 84),
#' covering roughly 108-123°E and 3-25°N.
#' @source ChinaBound2024, `bou1_4l_十段线.shp`.
#' @seealso [geom_submap()], [add_submap()]
"bou_southsea"

#' South China Sea submap (南海小地图)
#'
#' Build a small inset map of the South China Sea region, framed by a border
#' box. Typically placed in the bottom-right corner of a map of China via
#' [geom_submap()].
#'
#' @param data An `sf` object of the submap boundary. If `NULL` (default), the
#' bundled [bou_southsea] dataset (the "十段线" national boundary in the South
#' China Sea) is used.
#' @param colour,linewidth,fill colour, line width and fill passed to
#' [ggplot2::geom_sf()] for the boundary line.
#' @param border.colour,border.linewidth colour and line width of the box drawn
#' around the submap (`panel.border`).
#' @param bgcolor background colour of the submap panel.
#' @param ... other parameters passed to [ggplot2::geom_sf()].
#'
#' @return A `ggplot` object.
#'
#' @seealso [geom_submap()], [bou_southsea]
#' @importFrom ggplot2 ggplot geom_sf theme_void theme element_rect margin
#' @export
add_submap <- function(
    data = NULL,
    colour = "black", linewidth = 0.3, fill = NA,
    border.colour = "black", border.linewidth = 0.5,
    bgcolor = "white", ...) {
  if (is.null(data)) {
    data <- get0("bou_southsea", envir = asNamespace("gg.layers"))
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("`sf` is required to draw the submap. Please `install.packages('sf')`.")
  }

  ggplot() +
    geom_sf(data = data, colour = colour, linewidth = linewidth, fill = fill, ...) +
    theme_void() +
    theme(
      panel.border = element_rect(
        colour = border.colour, linewidth = border.linewidth, fill = NA),
      panel.background = element_rect(fill = bgcolor, colour = NA),
      plot.margin = margin(0, 0, 0, 0)
    )
}

#' geom_submap
#'
#' Add an inset submap (e.g. the South China Sea, 南海) to a ggplot, placed in a
#' corner via a [grid::viewport()]. The submap content is independent of the main
#' plot data; see [add_submap()] for the map itself.
#'
#' @inheritParams add_submap
#' @inheritParams grid::viewport
#' @param mapping ignored (kept for API consistency); the submap does not use the
#' main plot's aesthetics.
#'
#' @details
#' By default the submap is placed at the bottom-right corner
#' (`x = 1, y = 0, just = c(1, 0)`).
#'
#' @example R/examples/ex-geom_submap.R
#' @importFrom grid viewport grobTree
#' @export
geom_submap <- function(
    mapping = NULL, data = NULL,
    x = 1, y = 0,
    width = unit(0.16, "npc"), height = unit(0.26, "npc"),
    just = c(1, 0), ...) {
  fun <- function(panel_data, coords) {
    p <- add_submap(data, ...)
    grid::grobTree(
      as.grob(p),
      vp = grid::viewport(x = x, y = y, just = just, width = width, height = height)
    )
  }
  # the submap is independent of the main plot data; use a dummy 1-row frame so
  # the layer is drawn exactly once per panel.
  grid_panel(fun, data = data.frame(.submap = 1L), inherit.aes = FALSE)
}
