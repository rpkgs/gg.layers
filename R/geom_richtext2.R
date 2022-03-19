margin_adj <- function(x, margin) {
    if (is.numeric(x)) x <- ifelse(x > 0.5, x - margin, x + margin)
    x
}

# adjust label location slightly by `mar`

#' richtextGrob
#' 
#' @inheritParams gridtext::richtext_grob
#' @param ... other parameters to [gridtext::richtext_grob()]
#'
#' @examples
#' text <- c(
#'     "Some text **in bold.**",
#'     "Linebreaks<br>Linebreaks<br>Linebreaks",
#'     "*x*<sup>2</sup> + 5*x* + *C*<sub>*i*</sub>",
#'     "Some <span style='color:blue'>blue text **in bold.**</span><br>And *italics text.*<br>And some <span style='font-size:18pt; color:black'>large</span> text."
#' )
#' @keywords internal
#' @export
richtextGrob <- function(label, x = 0, y = 1, hjust = 0, vjust = 1, mar = 0.02,
                         padding = unit(c(1, 1, 1, 1) * 0, "pt"),
                         lineheight = 1.5,
                         fontsize = 12,
                         family = "Times",
                         color = "black",
                         ...) {
    label %<>% str_mk()
    x %<>% margin_adj(mar)
    y %<>% margin_adj(mar)

    gp <- gpar(col = color, lineheight = lineheight, fontsize = fontsize, fontfamily = family)
    richtext_grob(label,
        x = x, y = y,
        hjust = hjust, vjust = vjust, padding = padding, ..., gp = gp
    )
}

# richtext_grob <- function(label, x, y, ...) {
#     label %<>% str_mk()
#     richtext_grob
# }
#' @import ggtext
#' @export
GeomRichText2 <- ggproto("GeomRichText2", GeomRichText, 
    draw_panel = function(data, panel_params, coord, 
                          label.padding = unit(c(0.25, 0.25, 0.25, 0.25), "lines"),
                          label.margin = unit(c(0, 0, 0, 0), "lines"),
                          label.r = unit(0.15, "lines"),
                          na.rm = FALSE) {
        data$label %<>% str_mk()
        GeomRichText$draw_panel(data, panel_params, coord, 
          label.padding, label.margin, label.r, na.rm)
    }
)

#' Richtext labels
#'
#' This geom draws text labels similar to [ggplot2::geom_label()], but formatted
#' using basic markdown/html. Parameter and aesthetic names follow the conventions
#' of [ggplot2::geom_label()], and therefore the appearance of the frame around
#' the label is controlled with `label.colour`, `label.padding`, `label.margin`,
#' `label.size`, `label.r`, even though the same parameters are called `box.colour`,
#' `box.padding`, `box.margin`, `box.size`, and `box.r` in [geom_textbox()]. Most 
#' styling parameters can be used as aesthetics and can be applied separately to
#' each text label drawn. The exception is styling parameters that are specified
#' as grid units (e.g., `label.padding` or `label.r`), which can only be specified
#' for all text labels at once. See examples for details.
#' 
#' @inheritSection ggtext::geom_richtext Aesthetics
#' @inheritParams ggtext::geom_richtext
#' 
#' @return A ggplot2 layer that can be added to a plot created with
#'   [ggplot2::ggplot()].
#' 
#' @seealso [ggtext::geom_textbox()], [ggtext::element_markdown()]
#' @example R/examples/ex-geom_richtext2.R
#' 
#' @export
geom_richtext2 <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      nudge_x = 0,
                      nudge_y = 0,
                      label.padding = unit(c(0.25, 0.25, 0.25, 0.25), "lines"),
                      label.margin = unit(c(0, 0, 0, 0), "lines"),
                      label.r = unit(0.15, "lines"),
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y` but not both.", call. = FALSE)
    }
    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRichText2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      label.padding = label.padding,
      label.margin = label.margin,
      label.r = label.r,
      na.rm = na.rm,
      ...
    )
  )
}
