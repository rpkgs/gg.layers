#' colourbar_triangle
#' 
#' @param ... parameters passed to [ggplot2::guide_colourbar]
#' 
#' @example R/examples/ex-colorbar_triangle.R
#' 
#' @references 
#' 1. <https://stackoverflow.com/questions/68440366/how-can-i-add-triangles-to-a-ggplot2-colorbar-in-r-to-indicate-out-of-bound-valu>
#' 
#' @export 
colourbar_triangle <- function(...) {
  guide <- guide_colourbar(...)
  class(guide) <- c("colourbar_triangle", class(guide))
  guide
}

#' @import gtable
#' @importFrom grid polygonGrob
#' @export 
guide_gengrob.colourbar_triangle <- function(...) {
  # First draw normal colourbar
  guide <- NextMethod()
  # Extract bar / colours
  is_bar <- grep("^bar$", guide$layout$name)
  bar <- guide$grobs[[is_bar]]
  extremes <- c(bar$raster[1], bar$raster[length(bar$raster)])
  # Extract size
  width  <- guide$widths[guide$layout$l[is_bar]]
  height <- guide$heights[guide$layout$t[is_bar]]
  short  <- min(convertUnit(width, "cm",  valueOnly = TRUE),
                convertUnit(height, "cm", valueOnly = TRUE))
  # Make space for triangles
  guide <- gtable_add_rows(guide, unit(short, "cm"),
                           guide$layout$t[is_bar] - 1)
  guide <- gtable_add_rows(guide, unit(short, "cm"),
                           guide$layout$t[is_bar])

  # Draw triangles
  lwd = 0.2
  top <- polygonGrob(
    x = unit(c(0, 0.5, 1), "npc"),
    y = unit(c(0, 1, 0), "npc"),
    gp = gpar(fill = extremes[1], col = "white", lwd = lwd)
  )
  bottom <- polygonGrob(
    x = unit(c(0, 0.5, 1), "npc"),
    y = unit(c(1, 0, 1), "npc"),
    gp = gpar(fill = extremes[2], col = "white", lwd = lwd)
  )
  # Add triangles to guide
  guide <- gtable_add_grob(
    guide, top,
    t = guide$layout$t[is_bar] - 1,
    l = guide$layout$l[is_bar]
  )
  guide <- gtable_add_grob(
    guide, bottom,
    t = guide$layout$t[is_bar] + 1,
    l = guide$layout$l[is_bar]
  )
  return(guide)
}
