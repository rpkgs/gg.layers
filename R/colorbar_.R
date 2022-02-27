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

#' @param legend.text.just The justification of the text relative to its (x, y)
#' location. If there are two values, the first value specifies horizontal
#' justification and the second value specifies vertical justification. Possible
#' string values are: "left", "right", "centre", "center", "bottom", and "top".
#' For numeric values, 0 means left (bottom) alignment and 1 means right (top)
#' alignment.

#' @example R/examples/ex-make_colorbar.R
#' @importFrom ggplot2 margin
#' @export
make_colorbar <- function(
  at,
  col = .regions$col,
  alpha = .regions$alpha,
  
  labels = NULL, 
  lab = NULL,
  labeller  = format,
  # format = "%f",
  pretty = FALSE, equispaced = TRUE,
  tick.number = 7,

  tck = 0.2,
  tck.padding = 0, #lines

  width = 2,
  height = 1,

  space = "right",

  raster = FALSE,
  interpolate = FALSE,

  tri.upper = NA,
  tri.lower = NA,
  title = NULL,

  unit = NULL,
  unit.adj = 0.3,

  cex.title = 1,
  axis.line = trellis.par.get("axis.line"),
  legend.text = trellis.par.get("axis.text"),
  legend.text.location = c(0.5, 0.5),
  legend.text.just = NULL,

  legend.margin = bb_margin(),
  rect = list(col = "black", lwd = 0.3),
  hjust = 0, vjust = 0.5,
  ...,
  draw = FALSE, vp = NULL)
{
  .regions <- trellis.par.get("regions")
  key = mget(ls()) # return all parameters
  # change labeller slightly
  key$labeller = function(x) {
    if (is.numeric(x)) labeller(x)  else x
  }
  key %<>% equispaced_colorkey()

  axis.line <- updateList(trellis.par.get("axis.line"), axis.line)
  legend.text <- updateList(trellis.par.get("axis.text"), legend.text)

  key$axis.line <- axis.line # in `key_triangle`

  # layout_name <- ifelse(space %in% c("top", "bottom"), "layout.heights", "layout.widths")
  # colorkey.title.padding   <- lattice.options()[[layout_name]]$colorkey.title.padding
  # colorkey.title.padding$x <- colorkey.title.padding$x *
  #     trellis.par.get(layout_name)$colorkey.title.padding
  ## made FALSE later if labels explicitly specified
  check.overlap <- TRUE

  # Getting the locations/dimensions/centers of the rectangles
  key$at <- sort(key$at) ## should check if ordered
  numcol <- length(key$at)-1
  
  key$col <- level.colors(x = seq_len(numcol) - 0.5,
                          at = seq_len(numcol + 1) - 1,
                          col.regions = key$col,
                          colors = TRUE)
  
  ## FIXME: need to handle DateTime classes properly
  atrange <- range(key$at, finite = TRUE)
  scat <- as.numeric(key$at) ## problems otherwise with DateTime objects (?)

  if (key$raster && !isTRUE(all.equal(diff(range(diff(scat))), 0)))
    warning("'at' values are not equispaced; output may be wrong")

  ## recnum <- length(scat)-1
  reccentre <- (scat[-1] + scat[-length(scat)]) / 2
  recdim <- diff(scat)

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

  lab = key$lab
  if (is.null(lab)) {
    if (key$pretty) {
      at <- lpretty(atrange, key$tick.number)
      at <- at[at >= atrange[1] & at <= atrange[2]]
    } else {
      # scat <- as.numeric(key$at) ## problems otherwise with DateTime objects (?)
      at <- as.numeric(key$at)
    }
    labels <- key$labeller(at) # , trim = TRUE
  } else if (is.characterOrExpression(lab) && length(lab)==length(key$at)) {
    check.overlap <- FALSE
    at <- key$at
    labels <- key$lab
  } else if (is.list(key$lab)) {
    at <- if (!is.null(key$lab$at)) key$lab$at else lpretty(atrange, key$tick.number)
    at <- at[at >= atrange[1] & at <= atrange[2]]
    labels <- if (!is.null(key$lab$lab)) {
      check.overlap <- FALSE
      key$labeller(key$lab$lab)
    } else key$labeller(at) # trim = TRUE
    # cex, col, rot, font, fontfamily, lineheight
    legend.text = modifyList(legend.text, key$lab)
  } else stop("malformed colorkey")

  rot  <- null_default(legend.text$rot, 0)
  cex  <- legend.text$cex
  col  <- legend.text$col
  font <- legend.text$font
  fontfamily <- legend.text$fontfamily
  fontface   <- legend.text$fontface
  lineheight <- legend.text$lineheight

  labscat <- at

  ## Tri
  height.Tri <- key$height/numcol
  open.lower <- convertTri(key$tri.lower, scat[1] == -Inf, height = height.Tri)
  open.upper <- convertTri(key$tri.upper, scat[length(scat)] == Inf, height.Tri)
  key.rect   <- 1 - open.lower - open.upper

  # legend
  if (is.null(legend.text.just)) {
    legend.text.just = switch(space,
        right  = if (rot == -90) c("center", "bottom") else c("left", "center"),
        left   = if (rot == 90) c("center", "bottom") else c("right", "center"),
        top    = if (rot == 0) c("center", "bottom") else c("left", "center"),
        bottom = if (rot == 0) c("center", "top") else c("right", "center")
    )
  }

  # add unit label, 20190924
  if (!(is.null(key$unit) || key$unit == "")){
    nlab <- length(labels)
    delta <- labscat[nlab] - labscat[nlab - 1]
    labscat[nlab+1] <- labscat[nlab] + delta*key$unit.adj
    labels[nlab+1]  <- sprintf("%s", key$unit)
  }
  
  width_lab = stringWidth(labels) %>% max() %>% multiply_by(cex)
  if (space %in% c('right', 'left')) {
    xpos = legend.text.location[1]
    vp_label <- viewport(yscale = atrange)
    x_lab = rep(xpos, length(labscat))
    y_lab = labscat
  } else {
    ypos = legend.text.location[2]
    vp_label <- viewport(xscale = atrange)
    y_lab = rep(ypos, length(labscat))
    x_lab = labscat
  }
  
  grob_labs <- textGrob(
      label = labels,
      x = x_lab, y = y_lab, vp = vp_label,
      default.units = "native",
      # check.overlap = check.overlap,
      just = legend.text.just,
      rot = rot,
      name = trellis.grobname("labels", type = "colorkey"),
      gp = gpar(
          col = col, cex = cex,
          fontfamily = fontfamily,
          fontface = chooseFace(fontface, font),
          lineheight = lineheight
      )
  )

  lgd_width = unit.c(
    0.6 * key$width * unit(1, "lines"),
    (key$tck + tck.padding) * unit(1, "lines"),
    width_lab)
  if (space %in% c('left', 'top')) lgd_width <- rev(lgd_width)

  heights.x <- c(0.5*(1 - key$height) + key$legend.margin$t,
                 key$height*c(open.upper, key.rect, open.lower),
                 0.5*(1 - key$height) + key$legend.margin$b)

  lgd_height <- unit(heights.x, rep("null", 5))

  if (space %in% c("right", "left")) {
    key.layout <- grid.layout(nrow = 5, ncol = 3, respect = TRUE,
                              heights = lgd_height,
                              widths = lgd_width, just = hjust) 
  } else if (space %in% c("top", "bottom")) {
    key.layout <- grid.layout(nrow = 3, ncol = 5, respect = TRUE,
                              heights = lgd_width,
                              widths  = lgd_height, just = vjust)
  }

  key.gf <- key_gf(key, key.layout, vp, vp_label, reccentre, recdim, FALSE)
  key.gf <- key_triangle(key.gf, key, open.lower, open.upper)

  key.gf <- key_border(key.gf, key, open.lower, open.upper)
  key.gf <- key_label(key.gf, key, labscat, grob_labs, vp_label)

  if (draw) {
    grid.newpage()
    grid.draw(key.gf)
  }
  
  class(key.gf) %<>% c("colorbar", "cbar", .)
  key.gf
}

#' @export
bb_margin <- function(t = 0, r = 0, b = 0, l = 0, unit = "pt") {
  # unit
  listk(t, r, b, l)
}

updateList <- function(x, val) {
  if (is.null(x)) x <- list()
  modifyList(x, val)
}

is.characterOrExpression <- function(x){
  is.character(x) || is.expression(x) || is.call(x) || is.symbol(x)
}

lpretty <- function(x, ...){
  eps <- 1e-10
  at <- pretty(x[is.finite(x)], ...)
  ifelse(abs(at-round(at, 3))<eps, round(at, 3), at)
}

chooseFace <- function(fontface = NULL, font = 1) {
  if (is.null(fontface)) font else fontface
}

null_default <- function(x, default = 0) {
  if (is.null(x)) default else x
}

#' @import ggplotify 
#' @export 
print.colorbar <- function(x, ...) {
  print(as.ggplot(x))
}
