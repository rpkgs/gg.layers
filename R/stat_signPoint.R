#' @name ggplot2-ggproto
#' @title ggplot2-ggproto
#' @keywords internal
NULL

#' resample_points
#' @param data A data.frame, with the columns of "x", "y", and "mask"(optional).
#' @param fact postive integer. Plot one point in `fact` steps.
#' @keywords internal
#' @export
resample_points <- function(data, fact = 1) {
    if (!is.null(data$mask)) {
        ind = which(data$mask)
        data = data[ind, ]
    }
    x = data$x %>% unique() %>% sort() #%>% sort()
    y = data$y %>% unique() %>% sort() #%>% sort()
    dx = diff(x) %>% getmode()
    dy = diff(y) %>% getmode()

    x_sel = seq(min(x), max(x), fact*dx)
    y_sel = seq(min(y), max(y), fact*dy)
    data %>% subset(x %in% x_sel & y %in% y_sel)
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSignPoint <- ggproto("StatSignPoint", StatSf,
    compute_panel = function(self, data, scales, coord, fact = 1) {
        resample_points(data, fact)
    },
    required_aes = c("x", "y"), # , "mask"
    default_aes = aes(mask = TRUE)
)

#' stat_signPoint
#' 
#' @inheritParams st_hatched_polygon
#' @inheritParams ggplot2::geom_sf
#' @param fact postive integer. Plot one point in `fact` steps.
#' 
#' @section Aesthetics:
#' [geom_signPoint()] requires the following aesthetics:
#' - `x`:
#' - `y`:
#' - `mask` (optional): Boolean, `mask=TRUE` is regarded as significant. 
#'    Only plot regions with `mask = TRUE`.
#' 
#' @example R/examples/ex-geom_signPoint.R
#' @export
stat_signPoint <- function(
    mapping = NULL, data = NULL,
    geom = "point",
    position = "identity",
    show.legend = NA, inherit.aes = TRUE,
    ...,
    fact = 1) {
    layer(
        geom = geom,
        stat = StatSignPoint,
        data = data,
        mapping = mapping,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(fact = fact, ...)
    )
}

# TODO: THIS FUNCTION CAN BE SIMPLIFIED ().

#' @rdname stat_signPoint
#' @export
geom_signPoint <- function(
    mapping = aes(), data = NULL, stat = "signPoint",
    position = "identity", show.legend = NA,
    inherit.aes = TRUE,
    ...,
    fact = 1) {
    layer(
        geom = GeomPoint,
        stat = stat,
        data = data,
        mapping = mapping,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(fact = fact, ...)
    )
}
