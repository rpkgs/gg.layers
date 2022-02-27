#' @import gtable ggplotify
#' @importFrom ggplot2 ggplotGrob ggplot_add
#' @export
add_colorbar <- function(p, g, width = 1) {
    p_table <- ggplotGrob(p)
    dim = dim(p_table)

    loc = p_table$layout %>% subset(name == "panel")

    width = grid::grobWidth(g) * width
    p2 = p_table %>% gtable_add_cols(width)

    ans = gtable_add_grob(p2, g, l = dim[2] + 1, t = loc$t, b = loc$b)
    as.ggplot(ans)
}

#' @export
ggplot_add.colorbar <- function(object, plot, object_name) {
    # plot = plot + theme(legend.position = "none")
    add_colorbar(plot, object)
}
