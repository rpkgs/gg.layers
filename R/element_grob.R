#' @export
element_grob_text <- function(
    element = element_text(),
    label = "", x = NULL, y = NULL, family = NULL,
    face = NULL, colour = NULL, size = NULL, hjust = NULL, vjust = NULL,
    alpha = 1,
    angle = NULL, lineheight = NULL, margin = NULL, margin_x = FALSE,
    margin_y = FALSE,
    default.units = "native",
    vp = NULL, ...) {
  if (is.null(vp)) vp <- viewport()
  if (is.null(label)) {
    return(zeroGrob())
  }
  vj <- vjust %||% element$vjust
  hj <- hjust %||% element$hjust

  margin <- margin %||% element$margin
  angle <- angle %||% element$angle %||% 0

  gp <- gpar(
    fontsize = size, col = colour, fontfamily = family,
    fontface = face, lineheight = lineheight
  )
  element_gp <- gpar(
    fontsize = element$size, col = element$colour, alpha = alpha,
    fontfamily = element$family, fontface = element$face,
    lineheight = element$lineheight
  )

  titleGrob(label,
    x, y,
    hjust = hj, vjust = vj, angle = angle,
    gp = modify_list(element_gp, gp),
    margin = margin, margin_x = margin_x, margin_y = margin_y,
    debug = element$debug,
    default.units = default.units, vp = vp, ...
  )
}

titleGrob <- function(
    label, x, y, hjust, vjust, angle = 0, gp = gpar(),
    margin = NULL, margin_x = FALSE, margin_y = FALSE, debug = FALSE,
    check.overlap = FALSE, ...) {
  if (is.null(label)) {
    return(zeroGrob())
  }
  grob_details <- title_spec(label,
    x = x, y = y, hjust = hjust,
    vjust = vjust, angle = angle, gp = gp, debug = debug,
    check.overlap = check.overlap, ...
  )
  
  add_margins(
    grob = grob_details$text_grob,
    height = grob_details$text_height,
    width = grob_details$text_width, gp = gp, margin = margin,
    margin_x = margin_x, margin_y = margin_y
  )
}

#' @import grid
title_spec <- function(
    label, x, y, hjust, vjust, angle, gp = gpar(), debug = FALSE,
    check.overlap = FALSE, ...) {
  if (is.null(label)) {
    return(zeroGrob())
  }
  just <- ggplot2:::rotate_just(angle, hjust, vjust)
  n <- max(length(x), length(y), 1)
  x <- x %||% unit(rep(just$hjust, n), "npc")
  y <- y %||% unit(rep(just$vjust, n), "npc")

  text_grob <- textGrob(label, x, y,
    hjust = hjust, vjust = vjust,
    # default.units = "native",
    rot = angle, gp = gp, check.overlap = check.overlap, ...
  )
  descent <- ggplot2:::font_descent(gp$fontfamily, gp$fontface, gp$fontsize, gp$cex)
  text_height <- unit(1, "grobheight", text_grob) + abs(cos(angle[1] / 180 * pi)) * descent
  text_width <- unit(1, "grobwidth", text_grob) + abs(sin(angle[1] / 180 * pi)) * descent
  if (isTRUE(debug)) {
    children <- gList(
      rectGrob(gp = gpar(fill = "cornsilk", col = NA)),
      pointsGrob(x, y, pch = 20, gp = gpar(col = "gold")), text_grob
    )
  } else {
    children <- gList(text_grob)
  }
  list(text_grob = children, text_height = text_height, text_width = text_width)
}

add_margins <- function(
    grob, height, width, margin = NULL, gp = gpar(),
    margin_x = FALSE, margin_y = FALSE) {
  if (is.null(margin)) {
    margin <- margin(0, 0, 0, 0)
  }
  if (margin_x && margin_y) {
    widths <- unit.c(margin[4], width, margin[2])
    heights <- unit.c(margin[1], height, margin[3])
    vp <- viewport(layout = grid.layout(3, 3, heights = heights, widths = widths), gp = gp)
    child_vp <- viewport(layout.pos.row = 2, layout.pos.col = 2)
  } else if (margin_x) {
    widths <- unit.c(margin[4], width, margin[2])
    vp <- viewport(layout = grid.layout(1, 3, widths = widths), gp = gp)
    child_vp <- viewport(layout.pos.col = 2)
    heights <- unit(1, "null")
  } else if (margin_y) {
    heights <- unit.c(margin[1], height, margin[3])
    vp <- viewport(layout = grid.layout(3, 1, heights = heights), gp = gp)
    child_vp <- viewport(layout.pos.row = 2)
    widths <- unit(1, "null")
  } else {
    widths <- width
    heights <- height
    return(gTree(children = grob, widths = widths, heights = heights, cl = "titleGrob"))
  }
  gTree(
    children = grob, vp = vpTree(vp, vpList(child_vp)),
    widths = widths, heights = heights, cl = "titleGrob"
  )
}
