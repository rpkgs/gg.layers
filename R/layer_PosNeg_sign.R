#' @rdname layer_PosNeg
#' @export
layer_PosNeg_sign <- function(
    mapping = NULL, data = NULL,
    x = 0.5, y = 0.5, height.factor = 1.2,
    # width = unit(0.3, "npc"), height = width, just = c(0, 0),
    size = 12, ...) {
  fun <- function(data, coords) {
    add_PosNeg_sign(data$z, data$mask, x, y, height.factor, size = size, ...)
  }
  grid_panel(fun, mapping, data)
}

# mask:
# `sign`    :  1
# `not-sign`: -1

#' @param ... other parameters to [element_grob_text()]
#' @rdname layer_PosNeg
#' @export
add_PosNeg_sign <- function(
    z, mask,
    x = 0.5, y = 0.5, height.factor = 1.2,
    cols = c("blue", "red"), alpha = 0.8, ...) {
  z <- sign(z)
  n <- length(z)
  n_pos <- sum(z == 1, na.rm = TRUE)
  n_pos_sign <- sum(z == 1 & mask == 1, na.rm = TRUE)

  n_neg <- sum(z == -1, na.rm = TRUE)
  n_neg_sign <- sum(z == -1 & mask == 1, na.rm = TRUE)

  levs <- c("P", "N") # %>% rev()
  cols <- set_names(cols, c("N", "P"))
  df <- data.frame(
    sign = factor(c("N", "P"), levs),
    perc = c(n_neg, n_pos) / n,
    perc_sign = c(n_neg_sign, n_pos_sign) / n
  ) %>%
    mutate(
      label = sprintf(
        "%s: %.1f%% (%.1f%%)",
        .data$sign, .data$perc * 100, .data$perc_sign * 100
      ),
      pos = c(.data$perc[1] * 0.75, .data$perc[1] + .data$perc[2] * 0.75)
    )

  if (is.null(mask)) {
    df %<>% mutate(label = sprintf("%s: %.1f%%", .data$sign, .data$perc * 100))
  }
  g1 <- element_grob_text(label = df$label[1], x = x, y = y, colour = cols[1], alpha = alpha, ...)
  height <- as.numeric(convertHeight(grobHeight(g1), "npc")) * height.factor

  g2 <- element_grob_text(label = df$label[2], x = x, y = y - height, colour = cols[2], alpha = alpha, ...)
  grobTree(g1, g2)
}
