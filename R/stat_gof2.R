fmt_gof <- "*NSE* = {str_num(NSE,2)}, *R^2* = {str_num(R2, 2)} \n *RMSE* = {str_num(RMSE,2)}"

# 1. 洪峰
eval_Qmax <- function(Qobs, Qsim, ...) {
  sim <- max(Qsim, na.rm = TRUE)
  obs <- max(Qobs, na.rm = TRUE)
  err <- sim - obs
  bias <- err / obs

  err_permit <- diff(range(Qobs, na.rm = TRUE)) * 0.2 # 实测变幅的20%
  if (err_permit < 0.05 * obs) {
    err_permit <- 0.05 * obs
  }
  # abs(err) <= err_permit
  data.table(obs, sim, err, err_permit, bias, passed = abs(err) <= err_permit)
}

# symbol_right = "\u2713"
symbol_right = "<span>\u2713</span>"
symbol_wrong = "<span>\u2717</span>" # "×"
# symbol_wrong = "×" # ""
# symbol_right = "\u2714" # heavy check mark

#' @import ggplot2
#' @export
StatGOF2 <- ggproto("StatGOF2", Stat,
  default_aes = aes(hjust = 0, yjust = 1),
  required_aes = c("obs", "sim"),
  dropped_aes = c("obs", "sim", "x", "y"),
  compute_group = function(data, scales, x, y, label.format, show.bias, eval.flood = FALSE) {
    # x0 <- mean(data$x, na.rm = TRUE)
    # y0 <- mean(data$y, na.rm = TRUE)
    g <- GOF(data$obs, data$sim)
    label <- with(g, glue(label.format))

    if (show.bias) label <- sprintf("%s \n Bias(%%) = %.2f%%", label, g$Bias_perc)

    if (eval.flood) {
      flood <- eval_Qmax(data$obs, data$sim)
      passed <- if (flood$passed) symbol_right else symbol_wrong # flood test
      label <- sprintf("%s, %s", label, passed)
    }

    hjust <- data$hjust %||% 0
    vjust <- data$vjust %||% 1

    df <- data.frame(
      # x = x0, y = y0,
      npcx = x, npcy = y, hjust = hjust[1], vjust = vjust[1], label
    )
    df
  }
)

#' stat_gof2
#'
#' @inheritParams geom_richtext_npc
#'
#' @param label.format default value: `"*NSE* = {str_num(NSE,2)}, *R^2* = {str_num(R2, 2)} \n
#' *RMSE* = {str_num(RMSE,2)}"`.
#'
#' `label.format` will be evaluated by [glue::glue()]. Variables inside `{}`
#' will be evaluated. All variables returned by [GOF()] are supported.
#'
#' @return No return. This function is used to calculate data for gglot2
#' `geom_*`, just like [ggplot2::stat_smooth()].
#'
#'
#' @section required axes:
#' - `obs`: observed
#' - `sim`: simulated
#'
#' @example R/examples/ex-stat_reg_gof2.R
#' @export
stat_gof2 <- function(mapping = NULL, data = NULL,
                      geom = GeomRichTextNpc,
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      # parameters
                      show.bias = TRUE,
                      label.format = fmt_gof,
                      x = 0.05, y = 0.95,
                      # hjust = 0, vjust = 1,
                      inherit.aes = TRUE, ...) {
  layer(
    stat = StatGOF2, data = data, mapping = mapping, geom = geom,
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

#' @rdname stat_gof2
#' @export
geom_gof2 <- function(mapping = NULL, data = NULL,
                      stat = StatGOF2, position = "identity",
                      ...,
                      show.bias = TRUE,
                      label.format = fmt_gof,
                      eval.flood = FALSE,
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
      eval.flood = eval.flood,
      label.format = label.format,
      na.rm = na.rm,
      hjust = hjust, vjust = vjust,
      size = size,
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
