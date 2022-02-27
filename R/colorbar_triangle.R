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
    # browser()
    # eval(x <- y, envir = parent.frame())
    # eval(y <- temp, envir = parent.frame())
}

#' draw lattice colorkey
#' 
#' - `colorkey_pos`: The position of colorkey
#' 
#' @param space One of `c('top', 'bottom', 'right', 'left')`
#' @param ncol How many columns in right mode? This parameter is designed for 
#' future extension.
#' 
#' @keywords internal
#' 
#' @rdname draw_colorkey
#' @export
colorkey_pos <- function(space, ncol = 3){
    # nrow = 5
    # num begin from left-top
    ## main
    # How many columns in right key
    box <- switch(space, 
            right  = c(3   , 1), 
            left   = c(3   , ncol), 
            bottom = c(1   , 3), 
            top    = c(ncol, 3))
    lower <- switch(space, 
            right  = c(4, 1), 
            left   = c(4, ncol), 
            bottom = c(1   , 2), 
            top    = c(ncol, 2))
    upper <- switch(space, 
            right  = c(2, 1), 
            left   = c(2, ncol), 
            bottom = c(1   , 4), 
            top    = c(ncol, 4))
    ## tick 
    tick <- switch(space, 
            right  = c(3, 2), 
            left   = c(3, ncol - 1), 
            bottom = c(2      , 3), 
            top    = c(ncol -1, 3))
    label <- switch(space, 
            right  = c(3, 3), 
            left   = c(3, ncol - 2), 
            bottom = c(3      , 3), 
            top    = c(ncol -2, 3))
    list(box = box, lower = lower, upper = upper, tick = tick, label = label)
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
#' key.gf <- key_gf(key, key.layout, vp, vp_label, axis.line, reccentre, recdim)
#' }
#' 
#' @rdname draw_colorkey
#' @export
key_gf <- function(key, key.layout, vp, vp_label, 
    reccentre, recdim, border = TRUE, ...)
{
    space     <- key$space
    axis.line <- key$axis.line

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
        
        name = trellis.grobname("frame", type="colorkey"))
    
    if (key$raster) {
        # raster
        key.gf <- placeGrob(key.gf,
            rasterGrob(mat,
                       width = 1, height = 1,
                       vp = viewport(clip = "on"),
                       name = trellis.grobname("raster", type="colorkey"),
                       interpolate = key$interpolate),
            row = pos$box[1], col = pos$box[2])
    } else {
        # image, default
        key.gf <- placeGrob(key.gf, 
            rectGrob(x = x, y = y,
                     vp = vp_label,
                     default.units = "native",
                     height = height, width = width, 
                     name = trellis.grobname("image", type="colorkey"),
                     gp = gpar(fill = key$col,
                            col = key$rect$col,
                            lwd = key$rect$lwd,
                            alpha = key$alpha)),
            row = pos$box[1], col = pos$box[2])
    }
    # border
    # if (border) {
    #     # border
    #     key.gf <- placeGrob(frame = key.gf,
    #         rectGrob(name = trellis.grobname("border", type="colorkey"),
    #                  gp = gpar(lty = axis.line$lty,
    #                           col = key$rect$col,
    #                           lwd = key$rect$lwd,
    #                           alpha = axis.line$alpha,
    #                           fill = "transparent")),
    #         pos$box[1], col = pos$box[2])
    # }
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
#' key.gf <- key_label(key.gf, key, labscat, vp_label, axis.line)
#' }
#' @rdname draw_colorkey
#' @export
key_label <- function(key.gf, key, labscat, labelsGrob, vp_label, ...) 
{
    do.labels <- (length(labscat) > 0)
    space     <- key$space
    axis.line <- key$axis.line
    
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
    if (do.labels) {    
        if (key$tck != 0)
        key.gf <- placeGrob(frame = key.gf,
            segmentsGrob(x0, y0, x1, y1,
                         vp = vp_label,
                         default.units = "native",
                         name = trellis.grobname("ticks", type="colorkey"),
                         gp = gpar(col = axis.line$col,
                                  lty = axis.line$lty,
                                  lwd = axis.line$lwd)),
            row = pos$tick[1], col = pos$tick[2])
        key.gf <- placeGrob(key.gf, labelsGrob, row = pos$label[1], col = pos$label[2])
    }
    return(key.gf)
}

# key.gf <- key_border(key.gf, key, open.lower, open.upper)

#' @param open.lower,open.upper The width of lower and upper triangle
#' @rdname draw_colorkey
#' @export
key_border <- function(key.gf, key, open.lower, open.upper){
    gp.border <- with(key$axis.line,
        gpar(col = col, lty = lty, lwd = lwd, alpha = alpha, fill = "transparent"))

    segment_bolder <- function(x0, y0, x1, y1, rot = 0, name) {
        segmentsGrob2(x0, y0, x1, y1, rot,
            default.units = "npc",
            name = trellis.grobname(sprintf("border.%s", name), type="colorkey"),
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
    
    l_lower <- do.call(segmentsGrob2, param_lower)
    l_upper <- do.call(segmentsGrob2, param_upper)

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
    lwd = key$axis.line$lwd

    gp_lower = gpar(fill = key$col[1], col = "transparent", alpha = key$alpha, lwd = lwd)
    gp_upper = gpar(fill = key$col[length(key$col)], col = "transparent", alpha = key$alpha, lwd = lwd)
    
    # right upper
    pnts0 <- cbind(x = c(0, 1, 0.5), 
                   y = c(0, 0, 1))

    pos <- colorkey_pos(space)
    if (open.lower > 0) {
        name <- trellis.grobname("lower.arrow", type="colorkey")
        rot <- ifelse(space %in% c("right", "left"), 180, 90)
        pnts <- rotate(pnts0[, 1], pnts0[, 2], rot = rot)
        ## vp = viewport(yscale = atrange),
        key.gf <- placeGrob(key.gf, 
            polygonGrob(pnts[, 1], pnts[, 2], default.units = "npc", name = name, gp = gp_lower), 
            row = pos$lower[1], col = pos$lower[2])
    }

    if (open.upper > 0) {
        name <- trellis.grobname("upper.arrow", type="colorkey")
        rot = ifelse(space %in% c("right", "left"), 0, -90)
        pnts <- rotate(pnts0[, 1], pnts0[, 2], rot = rot)
        ## vp = viewport(yscale = atrange),
        key.gf <- placeGrob(key.gf, 
            polygonGrob(pnts[, 1], pnts[, 2], default.units = "npc", name = name, gp = gp_upper), 
            row = pos$upper[1], col = pos$upper[2])
    }
    return(key.gf)
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
