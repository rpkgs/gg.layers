#' @keywords internal
#' @export
censor2 <- function(x, range = c(0, 1), only.finite = TRUE) {
  force(range)
  finite <- if (only.finite) is.finite(x) else TRUE
  x[finite & x < range[1]] <- range[1]
  x[finite & x > range[2]] <- range[2]
  x
}

#' scale_fill_gradientn2
#'
#' @inheritParams ggplot2::scale_fill_gradientn
#' @param oob function that handles limits outside of the scale limits
#' 
#' @export 
scale_fill_gradientn2 <- function(
    ...,
    colours,
    values = NULL,
    space = "Lab",
    na.value = "transparent",
    guide = colourbar_triangle(),
    oob = censor2,
    aesthetics = "colour",
    colors) {

  scale_fill_gradientn(
    ...,
    colours = colours,
    values = values,
    na.value = na.value,
    guide = guide,
    oob = oob,
    aesthetics,
    colors = colors
  )
}
