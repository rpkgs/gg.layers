#' @name ggplot2-ggproto
#' @title ggplot2-ggproto
#' @keywords internal
NULL

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSignHatch <- ggproto("StatSignHatch", StatSf,
    compute_panel = function(self, data, scales, coord, density = 1, angle = 45) {
        ind = which(data$mask)
        if (length(ind) == 0) {
            data.frame()
        } else {
            d = data[ind, c("x", "y")] %>% cbind(z = 1)
            poly = st_point2poly(d) # sf_poly

            patch <- st_hatched_polygon(poly, density = density, angle = angle)
            dat = data.frame(geometry = patch$geometry, PANEL = data$PANEL[1], group = data$group[1])
            ans = ggproto_parent(StatSf, self)$compute_group(dat, scales, coord)
            ans
        }
    },
    required_aes = c("x", "y", "mask")
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
#' 
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
