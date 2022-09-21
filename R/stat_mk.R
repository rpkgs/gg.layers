#' @import ggplot2
StatMK <- ggproto("StatMK", Stat,
  required_aes = c("x", "y"),
  compute_group = function(data, scales, slope_FUN = slope_mk) {
    x <- data$x
    y <- data$y
    slp = slope_FUN(y, x)["slope"]
    # rng <- range(data$x, na.rm = TRUE)
    # grid <- data.frame(x = rng)
    # mod <- mkTrend(data$y, data$x)
    # grid$y <- grid$x * mod["slp"] + mod["intercept"]
    # mod <- lm(y ~ x, data = data)
    # grid$y <- predict(mod, newdata = grid)
    # grid
    intercept <- mean(y - slp * x, na.rm = T)
    df = data.frame(intercept = intercept, slope = slp)
    df
  }
)

#' stat_mk
#'
#' @inheritParams ggplot2::stat_smooth
#'
#' @return No return. This function is used to calculate data for gglot2 `geom_*`,
#' just like [ggplot2::stat_smooth()].
#'
#' @example R/examples/ex-stat_mk.R
#' 
#' @import rtrend
#' @export
stat_mk <- function(mapping = NULL, data = NULL, geom = "abline",
                    position = "identity",
                    slope_FUN = slope_mk,
                    na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
  layer(
    stat = StatMK, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(slope_FUN = slope_FUN, na.rm = na.rm, ...)
  )
}
