#' draw horizontal and vertical polygons
#'
#' @inheritParams lattice::panel.levelplot
#' @param vals numeric vector
#' @param x The corresponding x position of `vals`
#' @param type one of "horizontal" or "vertical"
#' @param length.out the length of interpolated `vals` and `x`
#' @param alpha the alpha of polygon's fill color
#' @param zlim limits of `vals`
#'
#' @examples
#' set.seed(1)
#' y <- rnorm(10)
#' x <- seq_along(y)
#' @importFrom dplyr nth
#' @export
draw_polygon <- function(
    vals, x = NULL, type = "horizontal", length.out = 1e4,
    col.regions = c("blue", "red"), alpha = 0.6,
    zlim = c(-Inf, Inf),
    ...) {
  n <- length(vals)
  vals <- vals %>% c(0, ., 0)
  x <- x %>% c(.[1], ., .[n])

  if (is.null(x)) x <- seq_along(vals)
  xx <- seq(min(x), max(x), length.out = length.out)
  suppressWarnings(yy <- approx(x, vals, xx)$y)

  I_good <- which(!is.na(yy))
  ind <- first(I_good):last(I_good)
  yy <- yy[ind]
  xx <- xx[ind]

  # polygon(x = xx, y = clamp_min(yy, 0), col = "red")
  if (length(col.regions) >= 4) {
    col.neg <- alpha(nth(col.regions, 2), alpha = alpha)
    col.pos <- alpha(nth(col.regions, -2), alpha = alpha)
  } else {
    col.neg <- alpha(nth(col.regions, 1), alpha = alpha)
    col.pos <- alpha(nth(col.regions, -1), alpha = alpha)
  }

  params <- listk(type = "n", ...)
  if (all(is.finite(zlim))) params$xlim <- zlim

  xxx <- xx %>% c(., rev(.))
  if (type == "horizontal") {
    params %<>% c(list(x, vals), .)
    do.call(plot, params)
    # grid(nx = NULL, ny = NA)
    # {
    #     y <- vals
    #     plot(x, y)
    #     xxx = x %>% c(., rev(.))
    #     polygon(xxx, clamp(y, c(0, zlim[2])) %>% c(., .*0), col = col.pos)
    #     polygon(xxx, clamp(y, c(zlim[1], 0)) %>% c(., .*0), col = col.neg)
    # }
    polygon(xxx, clamp2(yy, c(0, zlim[2])) %>% c(., . * 0), col = col.pos)
    polygon(xxx, clamp2(yy, c(zlim[1], 0)) %>% c(., . * 0), col = col.neg)
  } else {
    params %<>% c(list(vals, x), .)
    do.call(plot, params)
    # grid(nx = NULL, ny = NA)
    polygon(clamp2(yy, c(0, zlim[2])) %>% c(., . * 0), xxx, col = col.pos)
    polygon(clamp2(yy, c(zlim[1], 0)) %>% c(., . * 0), xxx, col = col.neg)
  }
}

clamp2 <- function(x, lims = c(0, 1), fill.na = FALSE) {
  if (fill.na) {
    x[x < lims[1]] <- NA_real_
    x[x > lims[2]] <- NA_real_
  } else {
    x[x < lims[1]] <- lims[1]
    x[x > lims[2]] <- lims[2]
  }
  x
}
