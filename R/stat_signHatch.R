#' @name ggplot2-ggproto
#' @title ggplot2-ggproto
#' @keywords internal
NULL

#' st_df2hatch
#' 
#' @param data A data.frame, with the columns of "x", "y", "mask"(optional), and 
#' "PANEL"(optional), "group"(optional).
#' @inheritParams st_hatched_polygon
#' 
#' @return A multi-line sf object 
#' @keywords internal
#' @export
st_df2hatch <- function(data, density = 1, angle = 45){
    if (!is.null(data$mask)) {
        ind = which(data$mask)
        if (length(ind) == 0) return(data.frame())
        data = data[ind, ]
    }
    d = data[, c("x", "y")]
    poly = st_point2poly(d) # sf_poly

    patch <- st_hatched_polygon(poly, density = density, angle = angle)
    if (!is.null(data$PANEL) && !is.null(data$group)) {
        data.frame(geometry = patch$geometry, PANEL = data$PANEL[1], group = data$group[1])
    } else {
        data.frame(geometry = patch$geometry)
    }
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSignHatch <- ggproto("StatSignHatch", StatSf,
    compute_panel = function(self, data, scales, coord, density = 1, angle = 45) {
        dat = st_df2hatch(data, density, angle)
        ggproto_parent(StatSf, self)$compute_group(dat, scales, coord)
    },
    required_aes = c("x", "y", "mask"), 
    default_aes = aes(mask = TRUE)
)

#' geom_signHatch
#' 
#' @inheritParams st_hatched_polygon
#' @inheritParams ggplot2::geom_sf
#' 
#' @section Aesthetics:
#' [geom_signHatch()] requires the following aesthetics:
#' - `x`:
#' - `y`:
#' - `mask`: 
#' @example R/examples/ex-geom_signHatch.R
#' @export
stat_signHatch <- function(
    mapping = NULL, data = NULL,
    geom = "sf",
    position = "identity",
    show.legend = NA, inherit.aes = TRUE,
    ...,
    density = 1, angle = 45)
{
    c(layer_sf(
        data = data,
        mapping = mapping,
        stat = StatSignHatch,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            density = density, angle = angle,
            ...
        )), coord_sf(default = TRUE))
}

#' @rdname stat_signHatch
#' @export
geom_signHatch <- function(mapping = aes(), data = NULL, stat = "signHatch",
    position = "identity", show.legend = NA,
    inherit.aes = TRUE,
    ...,
    density = 1, angle = 45)
{
    c(
        layer_sf(
            geom = GeomSf,
            data = data,
            mapping = mapping,
            stat = stat,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                density = density, angle = angle,
                ...
            )
        ),
        coord_sf(default = TRUE)
    )
}
