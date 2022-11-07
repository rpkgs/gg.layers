#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import gggrid
#' @import grid magrittr ggplot2
#' @importFrom gridtext richtext_grob
#' @importFrom grid gpar
#' @importFrom stats median approx setNames quantile lm predict cor.test qf
#' @importFrom dplyr first last tibble 
#' @importFrom utils str
## usethis namespace: end
NULL
# ' @importFrom ggplot2 geom_abline

.onLoad <- function(libname, pkgname) {
  
}

#' @keywords internal
#' @export 
init_lattice <- function() {
    # lattice.layers:::`+.trellis`
    # environment(latticeExtra:::`+.trellis`)
    suppressWarnings({
        eval(parse(text = 'environment(draw.colorkey) <- environment(lattice::xyplot)'))  
        eval(parse(text = 'assignInNamespace("draw.colorkey", draw.colorkey, ns="lattice")'))
    })
    # asign_func(draw.colorkey, lattice::draw.colorkey)
    # invisible()
}
