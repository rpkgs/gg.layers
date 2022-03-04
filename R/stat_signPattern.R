#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSignPattern <- ggproto("StatSignPattern", StatSf,
    compute_panel = function(self, data, scales, coord) {
        # print(params)
        # print(params$level)
        ind = which(data$mask)
        if (length(ind) == 0) {
            data.frame()
        } else {
            d = data[ind, c("x", "y")] %>% cbind(z = 1)
            st = st_point2poly(d) # sf_poly

            data = data.frame(geometry = st$geometry, PANEL = data$PANEL[1], group = data$group[1])
            ans = ggproto_parent(StatSf, self)$compute_group(data, scales, coord)
            ans
        }
    }, required_aes = c("x", "y", "mask")
)

# ' @importFrom ggpattern geom_sf_pattern
#' @import ggplot2 ggpattern
#' @importFrom ggplot2 ggproto
#' @export
stat_signPattern <- function(
    mapping = NULL, data = NULL,
    geom = "sf",
    position = "identity",
    show.legend = NA, inherit.aes = TRUE,
    ...)
{
    c(layer_sf(
        data = data,
        mapping = mapping,
        stat = StatSignPattern,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            # level = level,
            ...
        )), coord_sf(default = TRUE))
}



## rewrite this function for significant level

#' @import ggpattern
# geom_sf_pattern(
#     pattern_fill = "transparent", fill = "transparent",
#     # pattern = 0,
#     pattern_size = 0.2,
#     # pattern_type = 3,
#     pattern_spacing = 0.01,
#     pattern_density = 0.02 / 2, # this could change to width
#     color = "transparent",
#     size = 0.2
# )


# #' @import ggplot2
# StatSpatialSignDist <- ggproto("StatLm", Stat,
#     required_aes = c("x", "y"),
#     compute_group = function(data, scales, ...) {
#         rng <- range(data$x, na.rm = TRUE)
#         grid <- data.frame(x = rng)
#         mod <- mkTrend(data$y, data$x)
#         grid$y <- grid$x * mod["slp"] + mod["intercept"]
#         browser()
#         # mod <- lm(y ~ x, data = data)
#         # grid$y <- predict(mod, newdata = grid)
#         grid
#     }
# )
