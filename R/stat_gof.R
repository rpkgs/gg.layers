fmt_gof = "*NSE* = {str_num(NSE,2)}, *R^2* = {str_num(R2, 2)} \n *RMSE* = {str_num(RMSE,2)}"

#' @import ggplot2
#' @export
StatGOF <- ggproto("StatGOF", Stat,
  default_aes = aes(hjust = 0, yjust = 1),
  required_aes = c("x", "y"),

  compute_group = function(data, scales, x, y, label.format, show.bias) {
  
    g <- GOF(data$x, data$y)
    label <- with(g, glue(label.format))
    if (show.bias) label <- sprintf("%s \n Bias(%%) = %.2f%%", label, g$Bias_perc)
    hjust = data$hjust %||% 0
    vjust = data$vjust %||% 1
    df <- data.frame(npcx = x, npcy = y, hjust = hjust[1], vjust = vjust[1], label)
    # print(head(data))
    # print(head(df))
    df
  }
)

#' stat_gof
#' 
#' @inheritParams geom_richtext_npc
#'
#' @return No return. This function is used to calculate data for gglot2 `geom_*`,
#' just like [ggplot2::stat_smooth()].
#' 
#' @export
stat_gof <- function(mapping = NULL, data = NULL,
                     geom = GeomRichTextNpc,
                     position = "identity", na.rm = FALSE, show.legend = NA,
                     # parameters
                     show.bias = TRUE,
                     label.format = fmt_gof,
                     x = 0.05, y = 0.95,
                     # hjust = 0, vjust = 1,
                     inherit.aes = TRUE, ...) {
  layer(
    stat = StatGOF, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      x = x,
      y = y,
      show.bias = show.bias,
      label.format = label.format,
      na.rm = na.rm,
      # hjust = hjust, vjust = hjust,
      ...
    )
  )
}

#' @rdname stat_gof
#' @export 
geom_gof <- function(mapping = NULL, data = NULL,
                     stat = StatGOF, position = "identity",
                     ...,
                     show.bias = TRUE,
                     label.format = fmt_gof,
                     x = 0, y = 1,
                     hjust = 0, vjust = 1, 
                     size = 5, 
                     na.rm = FALSE,
                     
                     show.legend = NA,
                     inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRichTextNpc,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      x = x,
      y = y,
      show.bias = show.bias,
      label.format = label.format,
      na.rm = na.rm,
      hjust = hjust, vjust = vjust,
      size = 5, 
      # hjust = 0, vjust = 1, 
      ...
    )
  )
}


# #' `stat` of regression coefficient and good-of-fitting information
# #'
# #' @inheritParams gridtext::richtext_grob
# #' @param ... other parameters to [gggrid::grid_panel()]
# #'
# #' @importFrom gridtext richtext_grob
# #' @importFrom grid gpar grobHeight
# #' @export
# stat_gof <- function(mapping = NULL, data = NULL,
#                      format = "NSE = {str_num(NSE,2)}, R^2 = {str_num(R2, 2)} \n RMSE = {str_num(RMSE,2)}",
#                      x = 0, y = 1, hjust = 0, vjust = 1, mar = 0.02,
#                      show.bias = TRUE, show.line = FALSE) {
#   fun <- function(data, coords, ...) {
#     g <- GOF(data$x, data$y)
#     label <- with(g, glue(format))
#     if (show.bias) label <- sprintf("%s \n Bias(%%) = %.2f%%", label, g$Bias_perc)
#     textGrob("hello")
#     # richtextGrob(label, x, y, hjust, vjust, mar, ...)
#   }

#   if (show.line) {
#     list(grid_panel2(fun, mapping, data), geom_abline(slope = 1, color = "red"))
#   } else {
#     list(grid_panel2(fun, mapping, data))
#   }
# }

# #' @importFrom grid convertWidth stringWidth convertHeight stringHeight grobHeight
# #' @export
# text_height_npc <- function(txt, to_width = FALSE) {
#     if (to_width) {
#         convertWidth(convertHeight(stringHeight(txt), "inch"), "npc", TRUE)
#     } else {
#         convertHeight(stringHeight(txt), "npc", TRUE)
#     }
# }
