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

#' @export
layer_PosNeg <- function(
    mapping = NULL, data = NULL,
    x = 0, y = 0, width = unit(0.3, "npc"), height = width, just = c(0, 0),
    fontsize = 12, theme = NULL, ...)
{
    fun <- function(data, coords) {
        p = add_PosNeg(data$z, fontsize = fontsize, theme = theme, ...)
        g = grid::grobTree(as.grob(p),
            vp = grid::viewport(
                x = x, y = y, just = just,
                width = width, height = height)
        )
        g
    }
    grid_panel(fun, mapping, data)
}

add_PosNeg <- function(z, ...) {
  z = sign(z)
  n = length(z)
  n_pos = sum(z == 1, na.rm = TRUE)
  n_neg = sum(z == -1, na.rm = TRUE)

  df = data.table(sign = factor(c("N", "P")),
                perc = c(n_neg, n_pos)/n) %>%
    mutate(label = sprintf("%s: %.1f%%", sign, perc*100),
            pos = c(perc[1], perc[1] + perc[2]*0.75))

  ggplot(df, aes(x="", y=perc, fill=sign))+
    geom_bar(width = 1, stat = "identity", alpha = 0.8) +
    coord_polar("y", start=0) +
    scale_fill_manual(values = cols[c(2, length(cols)-1)]) +
    scale_y_continuous(labels = scales::percent) +
    geom_text(aes(y = pos, label = label), fontface = 2) + #, color = sign
    labs(x = NULL) +
    theme(
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major.x = element_line(size = 0.3, linetype = 1),
      legend.position = "none",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank())
}
