#' @importFrom ggplot2 ggplot_build geom_text aes_string 
#' @export
facet_tag <- function (p, open = "(", close = ")", tag_pool = letters, x = -Inf, 
    i_start = 1, 
    update_theme = FALSE,
    y = Inf, hjust = -0.5, vjust = 1.5, fontface = 2, fontsize = 14, family = "TimesSimSun", 
    ...) 
{
    gb <- ggplot_build(p)
    lay <- gb$layout$layout
    tags <- cbind(lay, label = paste0(open, tag_pool[as.numeric(lay$PANEL) + i_start - 1], 
                                      close), x = x, y = y)
    p <- p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), 
                  ..., hjust = hjust, vjust = vjust, fontface = fontface, size = fontsize, 
                  family = family, inherit.aes = FALSE)
    
    # update_theme = TRUE
    if (update_theme) {
        p <- p +
            theme(
                panel.background = element_rect(fill = "white"),
                panel.border = element_rect(fill = NA, color = "grey60", size = 0.5),
                strip.text = element_text(size = fontsize - 1, family = family),
                # strip.text = element_blank(),
                strip.background = element_blank(), 
                axis.title = element_text(size = fontsize, face = 2),
                legend.title = element_text(size = fontsize+1, face = 2),
                legend.text = element_text(size = fontsize+1)
            )
    }
    p
}

# theme(
#     legend.position = "bottom",
#     axis.text = element_text(color = "black", size = 17),
#     plot.margin = margin(-5, 0, 0, 0),
#     legend.box.margin = margin(-5, 0, 0, 0),
#     legend.text = element_text(size = 18),
#     axis.title.x = element_text(margin = margin(0.2, 0, -0.3, 0, "cm"))
# )

#' @export 
facet_label <- function(labels,
  i_start = 1,
  mapping = NULL, data = NULL,
  x = 0, y = 1, hjust = 0, vjust = 1, mar = 0.03,
  family = "Times", fontface = 2, fontsize = 12,
  color = NULL, ...)
{
  fun <- function(data, coords) {
    group = data$PANEL[1] %||% 0
    i = group %>% as.integer() 
    i = i_start - 1 + i

    label = sprintf("(%s) %s", letters[i], labels[i])
    richtextGrob(label, x, y, hjust, vjust, mar,
        family = family, fontface = fontface, fontsize = fontsize,
        color = color, ...)
  }
  grid_group(fun, mapping, data)
}
