#' add_colorbar
#' 
#' @param width 
#' 
#' @import gtable ggplotify
#' @importFrom ggplot2 ggplotGrob ggplot_add
#' @export
add_colorbar <- function(p, g, width = NULL, 
    title = NULL, 
    legend.title = element_text(hjust = 0, vjust = 0, size = 14, family = "Times")) 
{
    if (!("gtable" %in% class(p))) p <- ggplotGrob(p)
    dim = dim(p)

    loc = p$layout %>% subset(grepl("panel", name)) %>% .[nrow(.), ]

    if (!is.null(title)) {
        g_title = element_grob(legend.title, title, x = 0, y = 0.5)
    } else {
        g_title = nullGrob()
    }

    if (is.null(width)) {   
        width = max(grobWidth(g), grobWidth(g_title))
    }
    
    # width = grid::grobWidth(g) * width
    p2 = p %>% gtable_add_cols(width)
    # p2$layout$clip <- "on"
    # g = as.grob(g)
    ans = gtable_add_grob(p2, g, l = dim[2] + 1, t = loc$t, b = loc$b, clip = "off")
    ans <- gtable_add_grob(ans, g_title, l = dim[2] + 1, t = loc$t - 1, clip = "off")
    ans
    # as.ggplot(ans)
}

#' grobs
#' @param options parameters (except `plotlist`) of [cowplot::plot_grid()]
#' @export
grobs <- function(..., options = list(nrow = 1)) {
    params = c(plotlist = list(...), options)
    do.call(cowplot::plot_grid, params)
}


# #' @export
# `+.gtable` <- function(e1, e2) {
#     if (is.null(e2)) { return(e1) }
#     gtable_add(e2, e1)
# }

#' @export
gtable_add <- function(object, plot, object_name) UseMethod("gtable_add", object)

#' @export
gtable_add.colorbar <- function(object, plot, object_name) {
    # plot = plot + theme(legend.position = "none")
    add_colorbar(plot, object)
}

#' @export
gtable_add.default <- function(object, plot, object_name) {
    as.ggplot(plot)
}

#' @export
ggplot_add.colorbar <- function(object, plot, object_name) {
    # plot = plot + theme(legend.position = "none")
    add_colorbar(plot, object)
}

# #' @export
# `+.cbar` <- function(e1, e2) {
#     if (is.null(e2)) { return(e1) }
#     colorbar_add(e2, e1)
# }

#' @export
colorbar_add <- function(b1, b2) UseMethod("colorbar_add", b1)


#' @export
colorbar_add.colorbar <- function(b1, b2) {
    grobs(b1, b2, options = list(nrow = 1))
}

#' @export
colorbar_add.gg <- function(b1, b2) {
    add_colorbar(b2, b1)
    # grobs(b1, b2, options = list(nrow = 1))
}

#' @export
print.gtable <- function(x, ..., verbose = FALSE) {
    if (verbose) {
        gtable:::print.gtable(r)
    } else {
        print(as.ggplot(x))
    }
}
