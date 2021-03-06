# ' @importFrom ggpmisc stat_poly_eq
# stat_reg_coef <- function() {
#     my.format <- 'R^2 ~`=`~"%.2f" * ","~slope~`=`~%.3g*", "*pvalue ~`=`~"%.3f"'
#     stat_poly_eq(formula = y ~ x,
#                  output.type = "numeric", parse = TRUE,
#                  mapping = aes(color = NULL,
#                                label = sprintf(my.format, after_stat(adj.r.squared), # adj.r.squared
#                                                after_stat(b_1), after_stat(p.value))))
# }

#' `stat` of regression coefficient and good-of-fitting information
#'
#' @inheritParams gridtext::richtext_grob
#' @param ... other parameters to [gggrid::grid_panel()]
#'
#' @importFrom gridtext richtext_grob
#' @importFrom grid gpar grobHeight
#' @import gggrid
#' @export
stat_gof <- function(
    mapping = NULL, data = NULL,
    format = "NSE = {str_num(NSE,2)}, R^2 = {str_num(R2, 2)} \n RMSE = {str_num(RMSE,2)}",
    x = 0, y = 1, hjust = 0, vjust = 1, mar = 0.02, 
    show.bias = TRUE, show.line = FALSE)
{
    fun <- function(data, coords, ...) {
        g = GOF(data$x, data$y)
        label = with(g, glue(format))
        if (show.bias) label = sprintf("%s \n Bias(%%) = %.2f%%", label, g$Bias_perc)
        richtextGrob(label, x, y, hjust, vjust, mar, ...)
    }
    if (show.line) {
        list(grid_panel(fun, mapping, data), geom_abline(slope = 1, color = "red"))
    } else {
        list(grid_panel(fun, mapping, data))
    }
}

# #' @importFrom grid convertWidth stringWidth convertHeight stringHeight grobHeight
# #' @export
# text_height_npc <- function(txt, to_width = FALSE) {
#     if (to_width) {
#         convertWidth(convertHeight(stringHeight(txt), "inch"), "npc", TRUE)
#     } else {
#         convertHeight(stringHeight(txt), "npc", TRUE)
#     }
# }
