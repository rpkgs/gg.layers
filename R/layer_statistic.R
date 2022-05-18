#' @export
layer_statistic <- function(
  mapping = NULL, data = NULL,
  x = 0, y = 1, hjust = 0, vjust = 1, family = "Times",
  digit = 2,
  color = NULL, ...)
{
  fun <- function(data, coords) {
    l = data$z %>% stat_statistic()

    fmt = glue(" = %.{digit}f ± %.{digit}f")
    num = sprintf(fmt, l$mean, l$sd)
    label = eval(substitute(expression(bar(italic(u))*" ± "*italic(sd)~num),
                            list(num = num)))

    element_grob_text(label = label,
                      x = x, y = y,
                      vjust = vjust, hjust = hjust, family = family, ...)
    # richtextGrob(label, x, y, hjust, vjust, mar,
    #     family = family, fontface = fontface, fontsize = fontsize,
    #     color = color, ...)
  }
  grid_panel(fun, mapping, data)
}
