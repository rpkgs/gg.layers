#' @name ggplot2-ggproto
#' @title ggplot2-ggproto
#' @keywords internal
NULL

#' st_df2hatch
#'
#' @inheritParams st_hatched_polygon
#' @param data A data.frame, with the columns of "x", "y", "mask"(optional), and
#' "PANEL"(optional), "group"(optional).
#' @param hatch boolean. If `FALSE`, a polygon will be returned, other than lines.
#' Meanwhile, the parameters `density` and `angle` will be ignored.
#'
#' @return A multi-line sf object
#' @keywords internal
#' @export
st_df2hatch <- function(data, density = 1, angle = 45, hatch = TRUE){
    if (!is.null(data$mask)) {
        ind = which(data$mask)
        if (length(ind) == 0) return(data.frame())
        data = data[ind, ]
    }
    d = data[, c("x", "y")]

    st = st_point2poly(d) # sf_poly
    # convert poly to hatch
    if (hatch) st <- st_hatched_polygon(st, density = density, angle = angle)

    if (!is.null(data$PANEL) && !is.null(data$group)) {
        data.frame(geometry = st$geometry, PANEL = data$PANEL[1], group = data$group[1])
    } else {
        data.frame(geometry = st$geometry)
    }
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSignHatch <- ggproto("StatSignHatch", StatSf,
    compute_panel = function(self, data, scales, coord, density = 1, angle = 45) {
        dat = st_df2hatch(data, density, angle)
        # https://github.com/tidyverse/ggplot2/blob/main/R/stat-sf.R
        # Compute stat_sf() by panel instead of group (#5170), compute_panel, compute_group
        ggproto_parent(StatSf, self)$compute_panel(dat, scales, coord)
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
