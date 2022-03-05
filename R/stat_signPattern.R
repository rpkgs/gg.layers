#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSignPattern <- ggproto("StatSignPattern", StatSf,
    compute_panel = function(self, data, scales, coord) {
        st = st_df2hatch(data, hatch = FALSE) # st polygon
        ggproto_parent(StatSf, self)$compute_group(st, scales, coord)
    }, 
    required_aes = c("x", "y", "mask"), 
    default_aes = aes(mask = TRUE)
)

#' stat_signPattern
#' 
#' @inheritParams ggpattern::geom_sf_pattern
#' @param ... other parameters to [ggpattern::geom_sf_pattern()]
#' 
#' @seealso [ggpattern::geom_sf_pattern()]
#' 
#' @example R/examples/ex-geom_signPattern.R
#' @import ggplot2
#' @importFrom ggpattern geom_sf_pattern GeomSfPattern
#' @importFrom ggplot2 ggproto
#' @export
stat_signPattern <- function(
    mapping = NULL, data = NULL,
    geom = "sf_pattern",
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
