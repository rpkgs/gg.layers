#' geom_spike
#'
#' @inheritParams ggplot2::geom_point
#' @param halfwin halfwin of movmean, [rtrend::movmean()]
#'
#' @example R/examples/ex-geom_movmean.R
#' @export
geom_spike <- function(mapping = NULL, data = NULL,
                       stat = "spike", position = "identity",
                       ...,
                       halfwin = 3, sd.times = 3, trs = NA, verbose=FALSE,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      halfwin = halfwin,
      sd.times = sd.times,
      verbose = verbose,
      trs = trs,
      ...
    )
  )
}

#' @import ggplot2
StatSpike <- ggproto("StatSpike", Stat,
  required_aes = c("x", "y"),
  # dropped_aes = c("x", "y"),
  compute_group = function(data, scales, halfwin = 3, sd.times = 3, trs = NA, verbose=FALSE) {
    y <- data$y
    y2 <- movmean(y, halfwin)
    if (is.na(trs)) {
      # sd <- sd(y2, na.rm = TRUE)
      sd <- sd(diff(y2), na.rm = TRUE)
      trs = sd * sd.times
      if (verbose) cat(sprintf("trs: %.2f\n", trs))
    }
    inds_spike <- which(abs(y - y2) >= trs)
    dat <- data[inds_spike, ]
    dat
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
stat_spike <- function(mapping = NULL, data = NULL, geom = "point",
                       position = "identity",
                       halfwin = 3, sd.times = 3, trs = NA, verbose=FALSE,
                       na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatSpike, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(halfwin = halfwin, sd.times = sd.times, trs = trs, verbose = verbose, na.rm = na.rm, ...)
  )
}
