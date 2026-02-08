.cols_lattice = c(
    "#FF80FF", "#FF82FF", "#FF85FF", "#FF87FF", "#FF8AFF", "#FF8CFF", "#FF8FFF", "#FF91FF", "#FF94FF",
    "#FF96FF", "#FF99FF", "#FF9CFF", "#FF9EFF", "#FFA1FF", "#FFA3FF", "#FFA6FF", "#FFA8FF", "#FFABFF",
    "#FFADFF", "#FFB0FF", "#FFB3FF", "#FFB5FF", "#FFB8FF", "#FFBAFF", "#FFBDFF", "#FFBFFF", "#FFC2FF",
    "#FFC4FF", "#FFC7FF", "#FFC9FF", "#FFCCFF", "#FFCFFF", "#FFD1FF", "#FFD4FF", "#FFD6FF", "#FFD9FF",
    "#FFDBFF", "#FFDEFF", "#FFE0FF", "#FFE3FF", "#FFE6FF", "#FFE8FF", "#FFEBFF", "#FFEDFF", "#FFF0FF",
    "#FFF2FF", "#FFF5FF", "#FFF7FF", "#FFFAFF", "#FFFCFF", "#FCFFFF", "#FAFFFF", "#F7FFFF", "#F5FFFF",
    "#F2FFFF", "#F0FFFF", "#EDFFFF", "#EBFFFF", "#E8FFFF", "#E6FFFF", "#E3FFFF", "#E0FFFF", "#DEFFFF",
    "#DBFFFF", "#D9FFFF", "#D6FFFF", "#D4FFFF", "#D1FFFF", "#CFFFFF", "#CCFFFF", "#C9FFFF", "#C7FFFF",
    "#C4FFFF", "#C2FFFF", "#BFFFFF", "#BDFFFF", "#BAFFFF", "#B8FFFF", "#B5FFFF", "#B3FFFF", "#B0FFFF",
    "#ADFFFF", "#ABFFFF", "#A8FFFF", "#A6FFFF", "#A3FFFF", "#A1FFFF", "#9EFFFF", "#9CFFFF", "#99FFFF",
    "#96FFFF", "#94FFFF", "#91FFFF", "#8FFFFF", "#8CFFFF", "#8AFFFF", "#87FFFF", "#85FFFF", "#82FFFF", "#80FFFF")

# ' @param space One of `c('top', 'bottom', 'right', 'left')`

#' @export
colorkey_pos <- function(space = "right"){
    # default is right
    .cbar_layout = matrix(c(
            "", "title", "",
            "upper", ""    , "",
            "box"  , "tick", "label",
            "lower", "",      ""), ncol = 3, byrow = TRUE) %>%
        rbind("", ., "") %>% cbind("", ., "")

    dat = rotate_layout(.cbar_layout, space) #%>% c()
    # num begin from left-top
    loc = expand.grid(r = 1:nrow(dat), c = 1:ncol(dat)) %>% set_rownames(NULL)

    dat = c(dat)
    elements = c("box", "lower", "upper", "tick", "label", "title") %>% set_names(., .)
    lapply(elements, function(x) { loc[which(dat == x), ] })
}

rotate_layout <- function(x, space = "right") {
    switch(space,
        right  = x,
        left   = x[, ncol(x):1],
        bottom = t(x)[, nrow(x):1],
        top    = t(x)[ncol(x):1, nrow(x):1]
    )
}

#' fix non-equispaced colorkey
#'
#' @description Not processed if `key$equispaced` is not true.
#'
#' @param key A `colorkey` object (list), at least with the element of `at`.
#'
#' @example R/examples/ex-draw.colorkey_equispaced.R
#'
#' @keywords internal
#' @export
equispaced_colorkey <- function(key) {
  if (!isTRUE(key$equispaced)) return(key) # not processed

  at <- key$at
  is_equispaced <- length(unique(diff(at[is.finite(at)]))) == 1
  if (!is_equispaced) {
    key$at <- seq_along(at)
    labels_at <- seq_along(at)
    labels <- key$labeller(at)
    if (first(at) == -Inf) {
      key$at[1] <- -Inf
      labels_at <- labels_at[-1]
      labels <- labels[-1]
    }
    if (last(at) == Inf) {
      key$at[length(key$at)] <- Inf
      n <- length(labels_at)
      labels_at <- labels_at[-n]
      labels <- labels[-n]
    }
    names = setdiff(names(key$labels), c("at", "labels"))
    key$labels <- list(at = labels_at, labels = labels) %>%
      c(key$labels[names])
  }
  key
}

#' convertTri
#'
#' Setting 'open.lower' and 'open.upper' to non-zero makes colorkey end with
#' triangular extensions, indicating open-ended intervals. Set to non-zero by
#' default only if first/last intervals are unbounded (-Inf / +Inf).
#' (NOTE: default should perhaps be 0 for back-compatibility, but currently
#' these are simply not shown in the legend, so probably new behaviour is no
#' worse).
#' When non-zero, controls fraction of key$height to be used for triangles at ends.
#'
#' @param x height of triangle. If x in the range of `[0, 0.25]`, `height` will
#' be ignored.
#' @param inf boolean
#' @param height height of triangle
#'
#' @keywords internal
#' @export
convertTri <- function(x, inf = FALSE, height = 0.05)
{
  if (length(x) == 1) {
    if (is.numeric(x) && (x >= 0 && x <= 0.25)) {
      return(x)
    } else if (is.na(x)) {
      # return(0.05)
      return(height*inf)
    } else if (isTRUE(x)) {
      return(height)
    } else {
      return(0)
    }
  }
  warning("Invalid value of 'tri.upper/tri.lower' ignored.")
  0
}

#' wrap values of x and y
#'
#' @param x,y object
#' @keywords internal
#' @export
wrap <- function(x, y){
    temp <- x
    xname <- deparse(substitute(x))
    yname <- deparse(substitute(y))

    assign(xname, y, envir = parent.frame())
    assign(yname, temp, envir = parent.frame())
    # eval(x <- y, envir = parent.frame())
    # eval(y <- temp, envir = parent.frame())
}

#' key_gf_nonraster
#'
#' @param key The parameter of colorkey
#' @param key.layout The position
#' @param vp viewport
#' @param vp_label viewport label
#' @param reccentre `reccentre = (scat[-1] + scat[-length(scat)]) / 2`
#' @param recdim `recdim = diff(scat)`
#' @param border boolean, the rentangle border
#'
#' @param ... ignored.
#'
#' @examples
#' \dontrun{
#' key.gf <- key_gf(key, key.layout, vp, vp_label, legend.line, reccentre, recdim)
#' }
#'
#' @rdname draw_colorkey
#' @keywords internal
#' @export
key_box <- function(key, key.layout, vp, vp_label,
    reccentre, recdim, border = TRUE, ...)
{
    space     <- key$space
    legend.line <- key$legend.line

    height = recdim; width = 1 # for image
    # default param for: right and left
    x = rep(.5, length(reccentre))
    y = reccentre

    mat <- matrix(rev(key$col), ncol = 1)
    if (space %in% c("top", "bottom")) {
        mat <- matrix(key$col, nrow = 1)
        wrap(x, y)
        wrap(height, width)
    }

    pos <- colorkey_pos(space)
    key.gf <- frameGrob(layout = key.layout, vp = vp,
        name = lattice::trellis.grobname("frame", type="colorkey"))

    if (key$raster) {
        # raster
        grob = rasterGrob(mat,
            width = 1, height = 1, vp = viewport(clip = "on"),
            name = lattice::trellis.grobname("raster", type = "colorkey"),
            interpolate = key$interpolate
        )
    } else {
        # image, default
        grob = rectGrob(
            x = x, y = y, vp = vp_label,
            default.units = "native",
            height = height, width = width,
            name = lattice::trellis.grobname("image", type = "colorkey"),
            gp = gpar(fill = key$col,
                col = key$legend.box$colour,
                lwd = key$legend.box$linewidth,
                alpha = key$legend.box$alpha)
        )
    }
    key.gf <- placeGrob(key.gf, grob, row = pos$box[1], col = pos$box[2])
    return(key.gf)
}

#' key_label
#'
#' @param key.gf  returned by `key.gf`
#' @param labscat at which to draw labels
#' @param labelsGrob labels grobs
#'
#' @examples
#' \dontrun{
#' key.gf <- key_label(key.gf, key, labscat, vp_label, legend.line)
#' }
#' @rdname draw_colorkey
#' @export
key_tick <- function(key.gf, key, labscat, vp_label, ...)
{
    do.labels <- (length(labscat) > 0)
    space     <- key$space
    legend.line <- key$legend.line

    if (!(is.null(key$unit) || key$unit == ""))
        labscat <- labscat[1:(length(labscat)-1)]

    # default right
    y0 <- y1 <- labscat
    x0 <- rep(0, length(labscat))

    tck = 1 #key$tck
    # tck = key$tck / (1 + key$tck)
    x1 <- rep(tck, length(labscat))
    if (space %in% c("left", "bottom")) {
        x0 <- rep(1, length(labscat))
        x1 <- rep(1 - tck, length(labscat))
    }

    if (space %in% c("top", "bottom")) {
        wrap(x0, y0)
        wrap(x1, y1)
    }

    pos <- colorkey_pos(space)
    if (do.labels && key$tck != 0) {
        grob_tck = segmentsGrob(x0, y0, x1, y1,
            vp = vp_label,
            default.units = "native",
            name = lattice::trellis.grobname("ticks", type = "colorkey"),
            gp = gpar(
                col = legend.line$colour,
                lty = legend.line$linetype,
                lwd = legend.line$size
            )
        )
        key.gf %<>% placeGrob(., grob_tck, row = pos$tick[1], col = pos$tick[2])
    }
    return(key.gf)
}

# key.gf <- key_border(key.gf, key, open.lower, open.upper)

#' @param open.lower,open.upper The width of lower and upper triangle
#' @rdname draw_colorkey
#' @export
key_border <- function(key.gf, key, open.lower, open.upper){
    # alpha = alpha,
    # size renamed to linewidth in ggplot v3.4.0
    legend.line <- key$legend.line
    gp.border <- gpar(
        col = legend.line$colour %||% legend.line$col %||% "black",
        lty = legend.line$linetype %||% legend.line$lty %||% 1,
        lwd = legend.line$linewidth %||% legend.line$lwd %||% 0.5,
        fill = "transparent"
    )

    segment_bolder <- function(x0, y0, x1, y1, rot = 0, name) {
        segmentsGrob2(x0, y0, x1, y1, rot,
            default.units = "npc",
            name = lattice::trellis.grobname(sprintf("border.%s", name), type="colorkey"),
            gp = gp.border)
    }
    space = key$space

    if ((open.upper > 0) || (open.lower > 0)) {
        rot_box <- ifelse(space %in% c("left", "right"), 0, 90)
        line_box <- segment_bolder(
            c(0, 1), c(0, 0), c(0, 1), c(1, 1), rot_box, name = 'sides')
    } else {
        line_box <- segment_bolder(
            c(0, 1, 0, 0), c(0, 0, 0, 1), c(0, 1, 1, 1), c(1, 1, 0, 1), name = 'sides')
    }

    # parameters of right upper triangle, other triangles can be got by rotating
    param <- list(x0 = c(0, 1), y0 = c(0, 0), x1 = c(0.5, 0.5), y1 = c(1, 1))

    rot_lower <- switch(space,
        right  = 180, left   = 180,
        top    =  90, bottom =  90)
    rot_upper <- switch(space,
        right  =  0 , left   =  0,
        top    = -90, bottom = -90)

    param_lower <- c(param, list(rot = rot_lower, name = 'lower'))
    param_upper <- c(param, list(rot = rot_upper, name = 'upper'))

    l_lower <- do.call(segment_bolder, param_lower)
    l_upper <- do.call(segment_bolder, param_upper)

    pos <- colorkey_pos(space)
    if (open.upper > 0)
        key.gf <- placeGrob(frame = key.gf, l_upper, row = pos$upper[1], col = pos$upper[2])
    if (open.lower > 0)
        key.gf <- placeGrob(frame = key.gf, l_lower, row = pos$lower[1], col = pos$lower[2])

    key.gf <- placeGrob(frame = key.gf, line_box, row = pos$box[1], col = pos$box[2])
    return(key.gf)
}

# key_triangle(key.gf, key, gp.border, open.lower, open.upper)
#' @rdname draw_colorkey
#' @export
key_triangle <- function(key.gf, key, open.lower, open.upper){
    space = key$space
    # lwd = key$legend.line$lwd
    lwd = key$legend.line$linewidth
    col = "transparent" # key$legend.line$colour

    gp_lower = gpar(fill = key$col[1], col = col, alpha = key$alpha, lwd = lwd)
    gp_upper = gpar(fill = key$col[length(key$col)], col = col, alpha = key$alpha, lwd = lwd)

    # right upper
    pnts0 <- cbind(x = c(0, 1, 0.5),
                   y = c(0, 0, 1))

    pos <- colorkey_pos(space)
    if (open.lower > 0) {
        name <- lattice::trellis.grobname("lower.arrow", type="colorkey")
        rot <- ifelse(space %in% c("right", "left"), 180, 90)
        pnts <- rotate(pnts0[, 1], pnts0[, 2], rot = rot)
        ## vp = viewport(yscale = atrange),
        key.gf <- placeGrob(key.gf,
            polygonGrob(pnts[, 1], pnts[, 2], default.units = "npc", name = name, gp = gp_lower),
            row = pos$lower[1], col = pos$lower[2])
    }

    if (open.upper > 0) {
        name <- lattice::trellis.grobname("upper.arrow", type="colorkey")
        rot = ifelse(space %in% c("right", "left"), 0, -90)
        pnts <- rotate(pnts0[, 1], pnts0[, 2], rot = rot)
        ## vp = viewport(yscale = atrange),
        key.gf <- placeGrob(key.gf,
            polygonGrob(pnts[, 1], pnts[, 2], default.units = "npc", name = name, gp = gp_upper),
            row = pos$upper[1], col = pos$upper[2])
    }
    return(key.gf)
}

## for ggplot2
make_triangle <- function(space = "right", cols, lwd = 0.4) {
  col = "black"
  alpha = 1
  gp_lower <- gpar(fill = cols[1], col = col, alpha = alpha, lwd = lwd)
  gp_upper <- gpar(fill = cols[length(cols)], col = col, alpha = alpha, lwd = lwd)

  pnts0 <- cbind(
    x = c(0, 1, 0.5),
    y = c(0, 0, 1)
  )

  ## 下界
  # name <- lattice::trellis.grobname("lower.arrow", type = "colorkey")
  rot <- ifelse(space %in% c("right", "left"), 180, 90)
  pnts <- rotate(pnts0[, 1], pnts0[, 2], rot = rot)
  g_lower <- polygonGrob(pnts[, 1], pnts[, 2], default.units = "npc", gp = gp_lower)

  ## 上界
  # name <- lattice::trellis.grobname("upper.arrow", type = "colorkey")
  rot <- ifelse(space %in% c("right", "left"), 0, -90)
  pnts <- rotate(pnts0[, 1], pnts0[, 2], rot = rot)
  g_upper <- polygonGrob(pnts[, 1], pnts[, 2], default.units = "npc", gp = gp_upper)

  list(lower = g_lower, upper = g_upper)
}


#' rotate x and y
#'
#' @param x,y numeric vector
#' @param rot the angle to rotate
#' @param center the center of rotation
#'
#' @keywords internal
#' @export
rotate <- function(x, y, rot = 0, center = c(0.5, 0.5)){
    center <- matrix(center, nrow = length(x), ncol = 2, byrow = TRUE)
    theta  <- rot/180
    coef   <- cbind(c(cospi(theta), -sinpi(theta)),
                    c(sinpi(theta),  cospi(theta)))
    pnts   <- (cbind(x, y) - center) %*% coef + center
    return(pnts)
}

#' segmentsGrob2
#'
#' @keywords internal
#' @inheritParams grid::segmentsGrob
#' @param rot degree
#'
#' @export
segmentsGrob2 <- function(x0, y0, x1, y1, rot = 90, ...) {
    P0 <- rotate(x0, y0, rot)
    P1 <- rotate(x1, y1, rot)
    g <- segmentsGrob(P0[, 1], P0[, 2], P1[, 1], P1[, 2], ...)
    g
}

#' @importFrom gtable gtable gtable_add_grob
#' @export
bb_margin <- function(t = 0, r = 0, b = 0, l = 0, unit = "npc") {
    # unit
    listk(t, r, b, l)
}

updateList <- function(x, val) {
    if (is.null(x)) x <- list()
    modifyList(x, val)
}

is.characterOrExpression <- function(x) {
    is.character(x) || is.expression(x) || is.call(x) || is.symbol(x)
}

lpretty <- function(x, ...) {
    eps <- 1e-10
    at <- pretty(x[is.finite(x)], ...)
    ifelse(abs(at - round(at, 3)) < eps, round(at, 3), at)
}

chooseFace <- function(fontface = NULL, font = 1) {
    if (is.null(fontface)) font else fontface
}

null_default <- function(x, default = 0) {
    if (is.null(x)) default else x
}
