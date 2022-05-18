#' layer_PosNeg
#' 
#' @export
layer_PosNeg <- function(
    mapping = NULL, data = NULL,
    x = 0, y = 0, 
    # width = unit(0.3, "npc"), height = width, just = c(0, 0),
    size = 12, ...)
{
  # cols[c(2, length(cols)-1)]
  fun <- function(data, coords) {
    add_PosNeg(data$z, x, y, size = size, ...)
  }
  grid_panel(fun, mapping, data)
}

#' @param ... other parameters to [element_grob_text()]
#' @rdname layer_PosNeg
#' @export
add_PosNeg <- function(z, 
  x = 0.5, y = 0.5, height.factor = 1.2,
  cols = c("blue", "red"), alpha = 0.8, ...)
{
  z = sign(z)
  n = length(z)
  n_pos = sum(z == 1, na.rm = TRUE)
  n_neg = sum(z == -1, na.rm = TRUE)

  levs = c("P", "N") #%>% rev()
  cols = set_names(cols, c("N", "P"))
  df = data.table(sign = factor(c("N", "P"), levs),
                perc = c(n_neg, n_pos)/n) %>%
    mutate(label = sprintf("%s: %.1f%%", sign, perc*100),
            pos = c(perc[1]*0.75, perc[1] + perc[2]*0.75))

  g1 = element_grob_text(label = df$label[1], x = x, y = y, colour = cols[1], alpha = alpha, ...)
  height = as.numeric(convertHeight(grobHeight(g1), "npc")) * height.factor

  g2 = element_grob_text(label = df$label[2], x = x, y = y - height, colour = cols[2], alpha = alpha, ...)
  grobTree(g1, g2)  
}
