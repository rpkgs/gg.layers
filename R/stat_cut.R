#' stat_cut
#' @inheritParams ggplot2::Stat
#'
#' @example R/examples/ex-stat_cut.R
#' @export
stat_cut <- function(mapping = NULL, data = NULL,
                     geom = "point",
                     position = "identity",
                     ...,
                     # bins = NULL,
                     # binwidth = NULL,
                     breaks = NULL,
                     include.lowest = FALSE,
                     na.rm = FALSE,

                     show.legend = NA,
                     inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatCut,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      # bins = bins,
      # binwidth = binwidth,
      breaks = breaks,
      include.lowest = include.lowest,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname stat_cut
#' @export
StatCut <- ggproto("StatCut", Stat,
  required_aes = c("z"),
  # default_aes = aes(color = after_stat(level)), # order = after_stat(level),
  # z and weight get dropped during statistical transformation
  dropped_aes = c("z"),
  setup_params = function(data, params) {
    if (is.null(params$breaks)) {
      params$breaks <- pretty(data$z, 7)
    }
    params
  },
  setup_data = function(data, params) {
    brks = params$breaks
    # brks = c(-Inf, params$breaks, Inf)
    data %>% mutate(level = cut(z, brks, include.lowest = params$include.lowest))
  },
  compute_group = function(data, scales, breaks = NULL, include.lowest = FALSE) {
    data
  }
)

