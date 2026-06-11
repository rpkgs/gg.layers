#' @rdname geom_raster_filled
#' @export
stat_levels <- function(mapping = NULL, data = NULL,
                        geom = "point", position = "identity",
                        ...,
                        breaks = NULL,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatLevels,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      breaks = breaks,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#'
#' @details
#' `StatLevels` is the generic, geom-agnostic engine: it bins the continuous
#' aesthetic `z` into a discrete `level` via `cut(z, breaks)`. `StatRasterLevels`
#' and [StatPointLevels] inherit all of this binning logic and differ only in
#' which aesthetic the computed `level` is mapped to (`fill` vs `colour`).
StatLevels <- ggproto("StatLevels", StatIdentity,
  required_aes = c("z"),
  default_aes = aes(fill = after_stat(level)), # order = after_stat(level),
  # z gets dropped during statistical transformation
  dropped_aes = c("z"),
  setup_params = function(data, params) {
    if (is.null(params$breaks)) {
      params$breaks <- pretty(data$z, 7)
    }
    params
  },
  setup_data = function(data, params) {
    data %>% mutate(level = cut(z, params$breaks))
  },
  compute_group = function(data, scales, breaks = NULL) {
    data
  }
)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
# Shares all binning logic (setup_params / setup_data / compute_group) with the
# generic StatLevels; only the target aesthetic differs: points map the binned
# `level` to `colour`, whereas rasters map it to `fill`.
StatPointLevels <- ggproto("StatPointLevels", StatLevels,
  default_aes = aes(colour = after_stat(level))
)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
# Raster variant: inherits the binning logic from StatLevels and maps `level`
# to `fill` (StatLevels' default aesthetic).
StatRasterLevels <- ggproto("StatRasterLevels", StatLevels)


#' geom_point_levels
#'
#' Points coloured by a discretized (binned) continuous variable `z`. This is
#' the point counterpart of [geom_raster_filled()]: instead of filling raster
#' cells, it bins `z` into intervals defined by `breaks` and maps the resulting
#' `level` to the point `colour`.
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_contour
#' @param breaks Numeric vector of break points used by [base::cut()] to bin
#' `z` into discrete levels. When `NULL` (default), `pretty(z, 7)` is used.
#'
#' @seealso [geom_raster_filled()]
#'
#' @example R/examples/ex-geom_point_levels.R
#' @export
geom_point_levels <- function(mapping = NULL, data = NULL,
                              stat = "point_levels", position = "identity",
                              ...,
                              breaks = NULL,
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
      breaks = breaks,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_point_levels
#' @export
stat_point_levels <- function(mapping = NULL, data = NULL,
                              geom = "point", position = "identity",
                              ...,
                              breaks = NULL,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatPointLevels,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      breaks = breaks,
      na.rm = na.rm,
      ...
    )
  )
}
