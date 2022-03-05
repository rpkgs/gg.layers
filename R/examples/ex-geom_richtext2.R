library(ggplot2)

labels1 = c(
  "Some text **in bold.**",
  "Linebreaks<br>Linebreaks<br>Linebreaks",
  "*x*<sup>2</sup> + 5*x* + *C*<sub>*i*</sub>",
  "Some <span style='color:blue'>blue text **in bold.**</span><br>And *italics text.*<br>
    And some <span style='font-size:18pt; color:black'>large</span> text."
)

labels2 <- c(
  "gC m^{-2} d^{-1}",
  "gC m^-2 d^-1",
  "gC m_{-2} d_{-1}",
  "gC m_-2 d_-1"
  # "gC \n mm/d"
)

df <- data.frame(
  x = c(.2, .1, .5, .9),
  y = c(.8, .4, .1, .5),
  hjust = c(0.5, 0, 0, 1),
  vjust = c(0.5, 1, 0, 0.5),
  angle = c(0, 0, 45, -45),
  color = c("black", "blue", "black", "red"),
  fill = c("cornsilk", "white", "lightblue1", "white")
)

fun <- function(labels) {
  df$label = labels
  ggplot(df, aes(x, y, label = label, angle = angle, color = color,
                 hjust = hjust, vjust = vjust)) +
    geom_richtext2(
      fill = NA, label.color = NA, # remove background and outline
      label.padding = grid::unit(rep(0, 4), "pt") # remove padding
    ) +
    geom_point(color = "black", size = 2) +
    scale_color_identity() +
    xlim(0, 1) +
    ylim(0, 1)
}
fun(labels1)
fun(labels2)
# labels without frame or background are also possible
