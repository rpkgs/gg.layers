#' @importFrom rlang list2 try_fetch inject

dapply = ggplot2:::dapply
snake_class = ggplot2:::snake_class

# Wrapping vctrs data_frame constructor with no name repair
data_frame0 <- function(...) vctrs::data_frame(..., .name_repair = "minimal")

#' prob density
#' 
#' @param geom Use to override the default connection between [geom_prob_2d()] and [stat_prob_2d].
#' 
#' @inheritParams ggplot2::stat_density_2d
#' @example  R/examples/ex-stat_prob_2d.R
#' @export
stat_prob_2d <- function(mapping = NULL, data = NULL,
                            geom = "density_2d", position = "identity",
                            ...,
                            contour = TRUE,
                            contour_var = "density",
                            n = 100,
                            h = NULL,
                            adjust = c(1, 1),
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatProb2d,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      contour = contour,
      contour_var = contour_var,
      n = n,
      h = h,
      adjust = adjust,
      ...
    )
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatProb2d <- ggproto("StatProb2d", Stat,
  default_aes = aes(colour = "#3366FF", size = 0.5),

  required_aes = c("x", "y"),
  # because of the chained calculation in compute_panel(),
  # which calls compute_panel() of a different stat, we declare
  # dropped aesthetics there
  dropped_aes = character(0),

  extra_params = c(
    "na.rm", "contour", "contour_var",
    "bins", "binwidth", "breaks"
  ),

  # when contouring is on, are we returning lines or bands?
  contour_type = "lines",

  compute_layer = function(self, data, params, layout) {
    # first run the regular layer calculation to infer densities
    data <- ggproto_parent(Stat, self)$compute_layer(data, params, layout)

    # if we're not contouring we're done
    if (!isTRUE(params$contour)) return(data)

    # set up data and parameters for contouring
    contour_var <- params$contour_var %||% "density"
    if (!isTRUE(contour_var %in% c("z", "density", "ndensity", "count"))) {
      cli::cli_abort(c(
        "Invalid value of {.arg contour_var} ({.val {contour_var}})",
        "i" = "Supported values are {.val density}, {.val ndensity}, and {.val count}."
      ))
    }
    
    data$z <- data[[contour_var]]
    z.range <- range(data$z, na.rm = TRUE, finite = TRUE)
    params <- params[intersect(names(params), c("bins", "binwidth", "breaks"))]
    params$z.range <- z.range

    if (isTRUE(self$contour_type == "bands")) {
      contour_stat <- ggproto(NULL, StatContourFilled)
    } else { # lines is the default
      contour_stat <- ggproto(NULL, StatContour)
    }
    # update dropped aes
    contour_stat$dropped_aes <- c(contour_stat$dropped_aes, "density", "ndensity", "count")

    df = dapply(data, "PANEL", function(data) {
      scales <- layout$get_scales(data$PANEL[1])
      try_fetch(
        inject(contour_stat$compute_panel(data = data, scales = scales, !!!params)),
        error = function(cnd) {
          cli::cli_warn("Computation failed in {.fn {snake_class(self)}}", parent = cnd)
          data_frame0()
        }
      )
    })
    df$level = 1 - df$level
    df$level %<>% as.factor()
    df
  },

  compute_group = function(data, scales, na.rm = FALSE, h = NULL, adjust = c(1, 1),
                           n = 100, ...) {
    if (is.null(h)) {
      h <- c(MASS::bandwidth.nrd(data$x), MASS::bandwidth.nrd(data$y))
      h <- h * adjust
    }

    # calculate density
    dens <- MASS::kde2d(
      data$x, data$y, h = h, n = n,
      lims = c(scales$x$dimension(), scales$y$dimension())
    )

    # prepare final output data frame
    nx <- nrow(data) # number of observations in this group
    df <- expand.grid(x = dens$x, y = dens$y)
    df$density <- as.vector(dens$z)
    
    df$group <- data$group[1]
    df$ndensity <- df$density / max(df$density, na.rm = TRUE)
    df$count <- nx * df$density
    df$n <- nx
    df$level <- 1
    df$piece <- 1

    ## update z, kongdd, 20220920
    dx <- diff(dens$x[1:2])  # lifted from emdbook::HPDregionplot()
    dy <- diff(dens$y[1:2])
    sz <- sort(dens$z)
    c1 <- cumsum(sz) * dx * dy
    prob <- approx(sz, c1, dens$z)$y
    df$density <- prob
    # summary(df$density) %>% print()
    # df$prob = prob
    # print(head(df))
    df
  }
)

# #' @rdname ggplot2-ggproto
# #' @format NULL
# #' @usage NULL
# #' @export
# StatProb2dFilled <- ggproto("StatProb2dFilled", StatProb2d,
#   default_aes = aes(colour = NA, fill = after_stat(level)),
#   contour_type = "bands"
# )

# #' @rdname geom_density_2d
# #' @export
# stat_density_2d_filled <- function(mapping = NULL, data = NULL,
#                                    geom = "density_2d_filled", position = "identity",
#                                    ...,
#                                    contour = TRUE,
#                                    contour_var = "density",
#                                    n = 100,
#                                    h = NULL,
#                                    adjust = c(1, 1),
#                                    na.rm = FALSE,
#                                    show.legend = NA,
#                                    inherit.aes = TRUE) {
#   layer(
#     data = data,
#     mapping = mapping,
#     stat = StatDensity2dFilled,
#     geom = geom,
#     position = position,
#     show.legend = show.legend,
#     inherit.aes = inherit.aes,
#     params = list2(
#       na.rm = na.rm,
#       contour = contour,
#       contour_var = contour_var,
#       n = n,
#       h = h,
#       adjust = adjust,
#       ...
#     )
#   )
# }

# #' @rdname geom_density_2d
# #' @usage NULL
# #' @export
# stat_density2d_filled <- stat_density_2d_filled
