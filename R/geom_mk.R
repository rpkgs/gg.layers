cal_slope <- function(data, fun_slope = slope_mk) {
  x <- data$x
  y <- data$y
  slope <- fun_slope(y, x)["slope"]
  intercept <- mean(y - slope * x, na.rm = T)

  # rng <- range(data$x, na.rm = TRUE)
  # grid <- data.frame(x = rng)
  # mod <- mkTrend(data$y, data$x)
  # grid$y <- grid$x * mod["slp"] + mod["intercept"]
  # mod <- lm(y ~ x, data = data)
  # grid$y <- predict(mod, newdata = grid)
  vars_other <- setdiff(colnames(data), c("x", "y"))
  df <- data.frame(intercept, slope) %>%
    cbind(data[1, vars_other])
  df
}


GeomMK <- ggproto("GeomMK", Geom,
  required_aes = c("x", "y"),
  default_aes = aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = NA),
  setup_data = function(data, param) {
    data
  },
  # draw_key = GeomLine$draw_key,
  draw_group = function(self, data, panel_params, coord,
                        fun_slope = slope_mk, na.rm = FALSE) {
    df <- cal_slope(data, fun_slope)
    GeomAbline$draw_panel(df, panel_params, coord)
  }
)

#' geom_mk
#'
#' @inheritParams ggplot2::geom_abline
#' @param fun_slope function to calculate slope, default [rtrend::slope_mk()]
#' 
#' @export
geom_mk <- function(mapping = NULL, data = NULL,
                    stat = "identity", position = "identity",
                    ...,
                    fun_slope = slope_mk,
                    na.rm = FALSE,
                    show.legend = NA,
                    inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMK,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      fun_slope = fun_slope,
      ...
    )
  )
}


#' @import ggplot2
StatMK <- ggproto("StatMK", Stat,
  required_aes = c("x", "y"),
  dropped_aes = c("x", "y"),
  compute_group = function(data, scales, fun_slope = slope_mk) {
    cal_slope(data, fun_slope)
  }
)

#' @inheritParams ggplot2::stat_smooth
#' 
#' @return No return. This function is used to calculate data for gglot2 `geom_*`,
#' just like [ggplot2::stat_smooth()].
#' 
#' @example R/examples/ex-stat_mk.R
#' 
#' @rdname geom_mk
#' @import rtrend
#' @export
stat_mk <- function(mapping = NULL, data = NULL, geom = "abline",
                    position = "identity",
                    fun_slope = slope_mk,
                    na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
  layer(
    stat = StatMK, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(fun_slope = fun_slope, na.rm = na.rm, ...)
  )
}
