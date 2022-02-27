## OVERWRITE colorkey in lattice
draw.colorkey <- function(key, draw = FALSE, vp = NULL) {
    if (!is.list(key)) stop("key must be a list")
    key$draw <- draw
    key$vp <- vp
    g <- do.call(make_colorbar, key)
    g
}

#' @export
#' @rdname draw.colorkey
draw.colorkey2 <- draw.colorkey
