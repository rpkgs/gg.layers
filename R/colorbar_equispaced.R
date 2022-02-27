#' fix non-equispaced colorkey
#'
#' @description Not processed if `key$equispaced` is not true.
#'
#' @param key A `colorkey` object (list), at least with the element of `at`.
#'
#' @example R/examples/ex-draw.colorkey_equispaced.R
#'
#' @keywords internal
#' @export
equispaced_colorkey <- function(key) {
  if (!isTRUE(key$equispaced)) return(key) # not processed

  at <- key$at
  is_equispaced <- length(unique(diff(at[is.finite(at)]))) == 1
  if (!is_equispaced) {
    key$at <- seq_along(at)
    labels_at <- seq_along(at)
    labels <- key$labeller(at)
    if (first(at) == -Inf) {
      key$at[1] <- -Inf
      labels_at <- labels_at[-1]
      labels <- labels[-1]
    }
    if (last(at) == Inf) {
      key$at[length(key$at)] <- Inf
      n <- length(labels_at)
      labels_at <- labels_at[-n]
      labels <- labels[-n]
    }
    names = setdiff(names(key$labels), c("at", "labels"))
    key$labels <- list(at = labels_at, labels = labels) %>%
      c(key$labels[names])
  }
  key
}

#' convertTri
#'
#' Setting 'open.lower' and 'open.upper' to non-zero makes colorkey end with
#' triangular extensions, indicating open-ended intervals. Set to non-zero by
#' default only if first/last intervals are unbounded (-Inf / +Inf).
#' (NOTE: default should perhaps be 0 for back-compatibility, but currently
#' these are simply not shown in the legend, so probably new behaviour is no
#' worse).
#' When non-zero, controls fraction of key$height to be used for triangles at ends.
#'
#' @param x height of triangle. If x in the range of `[0, 0.25]`, `height` will
#' be ignored.
#' @param inf boolean
#' @param height height of triangle
#'
#' @keywords internal
#' @export
convertTri <- function(x, inf = FALSE, height = 0.05)
{
  if (length(x) == 1) {
    if (is.numeric(x) && (x >= 0 && x <= 0.25)) {
      return(x)
    } else if (is.na(x)) {
      # return(0.05)
      return(height*inf)
    } else if (isTRUE(x)) {
      return(height)
    } else {
      return(0)
    }
  }
  warning("Invalid value of 'tri.upper/tri.lower' ignored.")
  0
}
