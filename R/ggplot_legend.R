#' ggplot_legend
#' @return A grob object
#' 
#' @importFrom ggplot2 ggplot_gtable
#' @export
ggplot_legend <- function(g) {
  tmp <- ggplot_gtable(ggplot_build(g))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
