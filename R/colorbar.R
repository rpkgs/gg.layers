# Note: there are two 'at'-s here, one is key$at, which specifies
# the breakpoints of the rectangles, and the other is key$lab$at
# (optional) which is the positions of the ticks. We will use the
# 'at' variable for the latter, 'atrange' for the range of the
# former, and key$at explicitly when needed

#' make_colorbar
#'
#' @inheritParams grid::grid.text
#' @inheritParams ggplot2::theme
#' @param legend.text
#' - `cex`:
#' - `col`:
#' - `font`:
#' - `fontfamily`: The font family
#' - `fontface`: The font face (bold, italic, ...)
#' - `lineheight`:
#' @param padding.left,padding.right padding in the left and right of the legend
#'
#' @param hjust,vjust used in [grid::grid.layout()]
#'
#' @example R/examples/ex-make_colorbar.R
#' @importFrom grid unit
#' @importFrom ggplot2 margin element_grob element_text element_rect element_line
#' @export
make_colorbar <- function(
  at,
  labels = NULL,
  labeller = format,

  space = "right",
  width = 2,
  height = 1,

  col = NULL,
  alpha = 1,

  pretty = FALSE, equispaced = TRUE,
  tick.number = 7,
  tck = 0.3,
  tck.padding = 0, # lines
  raster = FALSE,
  interpolate = FALSE,
  tri.upper = NA,
  tri.lower = NA,

  legend.line = element_line(size = 0.8),
  legend.box = element_rect(size = 0.5),

  hjust = 0.5, vjust = 0.5,

  size = 12,
  family = "Times",

  legend.text.location = c(0.5, 0.5),
  legend.margin = bb_margin(),  #?

  title = NULL,
  legend.text = element_text(hjust = 0.5),
  legend.title = element_text(),
  fct.title.height = 1.8,

  padding.left = unit(2, "points"),
  padding.right = unit(2, "points"),
  ...,
  draw = FALSE, vp = NULL)
{
  if (is.null(col)) col = .cols_lattice
  legend.text$size = legend.text$size %||% size
  legend.text$family = legend.text$family %||% family
  legend.title$size = legend.title$size %||% (size+1)
  legend.title$family = legend.title$family %||% family

  # theme = theme_get()
  key <- mget(ls()) # return all parameters
  # change labeller slightly
  key$labeller <- function(x) {
    if (is.numeric(x)) labeller(x) else x
  }
  key %<>% equispaced_colorkey()

  ## made FALSE later if labels explicitly specified
  check.overlap <- TRUE

  # Getting the locations/dimensions/centers of the rectangles
  key$at <- sort(key$at) ## should check if ordered
  numcol <- length(key$at) - 1

  key$col <- lattice::level.colors(
    x = seq_len(numcol) - 0.5,
    at = seq_len(numcol + 1) - 1,
    col.regions = key$col,
    colors = TRUE
  )

  atrange <- range(key$at, finite = TRUE)
  scat <- as.numeric(key$at) ## problems otherwise with DateTime objects (?)

  if (key$raster && !isTRUE(all.equal(diff(range(diff(scat))), 0))) {
    warning("'at' values are not equispaced; output may be wrong")
  }

  reccentre <- (scat[-1] + scat[-length(scat)]) / 2
  recdim <- diff(scat)

  lab = guess_at_labels(key)
  labscat = at = lab$at
  labels = lab$labels

  ## Tri
  height.Tri <- key$height / numcol
  open.lower <- convertTri(key$tri.lower, scat[1] == -Inf, height = height.Tri)
  open.upper <- convertTri(key$tri.upper, scat[length(scat)] == Inf, height.Tri)
  # if (is.null(legend.text$hjust)) {
    # rot <- null_default(legend.text$rot, 0)
    # rot = 0
    # temp <- switch(space,
    #   # right  = if (rot == -90) c("center", "bottom") else c("left", "center"),
    #   # left   = if (rot == 90) c("center", "bottom") else c("right", "center"),
    #   # top    = if (rot == 0) c("center", "bottom") else c("left", "center"),
    #   # bottom = if (rot == 0) c("center", "top") else c("right", "center")
    #   right  = if (rot == -90) c(0.5, 0) else c(0, 0.5),
    #   left   = if (rot == 90) c(0.5, 0) else c(1, 0.5),
    #   top    = if (rot == 0) c(0.5, 0) else c(0, 0.5),
    #   bottom = if (rot == 0) c(0.5, 1) else c(1, 0.5)
    # )
    # legend.text$hjust %<>% null_default(temp[1])
    # legend.text$vjust %<>% null_default(temp[2])
  # }
  if (space %in% c("right", "left")) {
    xpos <- legend.text.location[1]
    vp_label <- viewport(yscale = atrange)
    x_lab <- rep(xpos, length(labscat))
    y_lab <- labscat
  } else {
    ypos <- legend.text.location[2]
    vp_label <- viewport(xscale = atrange)
    y_lab <- rep(ypos, length(labscat))
    x_lab <- labscat
  }

  # add unit label, 20190924
  if (!(is.null(key$unit) || key$unit == "")) {
    ## DEPRECATED
    # title <- textGrob(title)
    nlab <- length(labels)
    delta <- labscat[nlab] - labscat[nlab - 1]
    labscat[nlab + 1] <- labscat[nlab] + delta * key$unit.adj
    labels[nlab + 1] <- sprintf("%s", key$unit)
  }

  grob_label = element_grob_text(legend.text, labels,
    x = x_lab, y = y_lab, vp = vp_label, default.units = "native")

  width_lab <- max(stringWidth(labels)) # %>% multiply_by(cex)
  if (!is.null(title)) {
    grob_title = element_grob_text(legend.title, title, x = 0.5, y = 0.5)
    #width_title = grobWidth(grob_title) %>% unit2npc()
  } else {
    grob_title = nullGrob()
  }
  # height_title = ifelse(is.null(title), 0, height.Tri)
  height_title = grobHeight(grob_title) %>% convertUnit("npc") %>% as.numeric()
  height_title = height_title * fct.title.height
  # height_title = unit(1, "lines") %>% convertUnit("npc") %>% as.numeric() #%>% multiply_by(1.2)
  # height_title = height_title * legend.title$size /12 * 1.4
  key.rect <- 1 - open.lower - open.upper - height_title

  lgd_width <- unit.c(
    padding.left,
    0.6 * key$width * unit(1, "lines"),
    (key$tck + tck.padding) * unit(1, "lines"),
    width_lab,
    padding.right
  )
  lgd_width %>% unit2npc() #%>% unit("NULL")

  if (space %in% c("left", "top")) lgd_width <- rev(lgd_width)

  lgd_height <- c(
    key$legend.margin$t,
    height_title,
    open.upper,
    key.rect,
    open.lower,
    key$legend.margin$b
  )
  lgd_height <- lgd_height * key$height * sum(lgd_height[2:5])
  lgd_height <- unit(lgd_height, "npc")

  if (space %in% c("right", "left")) {
    layout = c(6, 5)
    just = hjust
  } else if (space %in% c("top", "bottom")) {
    lgd_height %<>% rev()
    wrap(lgd_width, lgd_height)
    layout = c(5, 6)
    just = vjust
  }
  key.layout <- grid.layout(
    nrow = layout[1], ncol = layout[2], respect = TRUE,
    heights = lgd_height,
    widths = lgd_width, just = just
  )
  # print2(lgd_width, lgd_height, max.level=1)

  pos <- colorkey_pos(space)
  key.gf <- key_box(key, key.layout, vp, vp_label, reccentre, recdim, FALSE)
  key.gf %<>% key_triangle(key, open.lower, open.upper)
  key.gf %<>% key_border(key, open.lower, open.upper)
  key.gf %<>% key_tick(key, labscat, vp_label)
  # add grob_label
  key.gf %<>% placeGrob(grob_label, row = pos$label[1], col = pos$label[2])

  pos = colorkey_pos(space)
  key.gf %<>% placeGrob(grob_title, row = pos$title[1], col = pos$title[2])

  if (draw) {
    grid.newpage()
    grid.draw(key.gf)
  }
  class(key.gf) %<>% c("colorbar", "cbar", .)
  key.gf
}

## add gtabel to hold legend
# if (space %in% c("right", "left")) {
#   vp = viewport(x = 0, y = 0, just = c(0, 0))
#   tab <- gtable(unit.c(padding.left, grobWidth(key.gf), padding.right), unit(1, "null"),
#     vp = vp)
#   tab <- gtable_add_grob(tab, key.gf, t = 1, l = 2)
#   class(tab) %<>% c("colorbar", .)
#   return(tab)
# }

#' @import ggplotify
#' @export
print.colorbar <- function(x, ...) {
  print(as.ggplot(x))
}

guess_at_labels <- function(key) {
  atrange <- range(key$at, finite = TRUE)

  # The following code assumes names `key$lab` and `key$lab$lab` (which may have
  # been used in user code), whereas documentation says key$labels and
  # `key$labels$labels`.  To make both work without 'partial matching' warnings,
  # we rename key$labels to `key$lab` etc.
  if (!is.null(key$labels)) {
    key$lab <- key$labels
    keylabels <- NULL
    if (is.list(key$lab) && !is.null(key$lab$labels)) {
      key$lab$lab <- key$lab$labels
      key$lab$labels <- NULL
    }
  }

  lab <- key$lab
  if (is.null(lab)) {
    if (key$pretty) {
      at <- lpretty(atrange, key$tick.number)
      at <- at[at >= atrange[1] & at <= atrange[2]]
    } else {
      # scat <- as.numeric(key$at) ## problems otherwise with DateTime objects (?)
      at <- as.numeric(key$at)
    }
    labels <- key$labeller(at) # , trim = TRUE
  } else if (is.characterOrExpression(lab) && length(lab) == length(key$at)) {
    # check.overlap <- FALSE
    at <- key$at
    labels <- key$lab
  } else if (is.list(key$lab)) {
    at <- if (!is.null(key$lab$at)) key$lab$at else lpretty(atrange, key$tick.number)
    at <- at[at >= atrange[1] & at <= atrange[2]]
    labels <- if (!is.null(key$lab$lab)) {
      # check.overlap <- FALSE
      key$labeller(key$lab$lab)
    } else {
      key$labeller(at)
    }
    # cex, col, rot, font, fontfamily, lineheight
    # legend.text <- modifyList(legend.text, key$lab)
  } else {
    stop("malformed colorkey")
  }
  listk(at, labels)
}

unit2npc <- function(x) {
  convertUnit(x, "npc") %>% as.numeric()
}

num2npc <- function(x) {
  unit(x, "npc")
}
