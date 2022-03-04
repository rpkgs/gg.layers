## OVERWRITE colorkey in lattice
#' draw.colorkey
#' @inheritParams lattice::draw.colorkey
#' @seealso [make_colorbar()]
draw.colorkey <- function(key, draw = FALSE, vp = NULL) {
    if (!is.list(key)) stop("key must be a list")
    key$draw <- draw
    key$vp <- vp
    g <- do.call(make_colorbar, key)
    g
}

#' @export
#' @rdname draw.colorkey
#' @keywords internal
draw.colorkey2 <- draw.colorkey
