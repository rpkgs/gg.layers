#' Text with Normalised Parent Coordinates
#' 
#' @inheritParams ggtext::geom_richtext
#' @inheritSection ggtext::geom_richtext Aesthetics
#' 
#' @example R/examples/ex-geom_richtext_npc.R
#' 
#' @seealso [ggtext::geom_richtext()], [geom_richtext2()]
#' @export
geom_richtext_npc <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      nudge_x = 0,
                      nudge_y = 0,
                      # parse = FALSE,
                      # check_overlap = FALSE,
                      # na.rm = FALSE,
                      label.padding = unit(c(0.25, 0.25, 0.25, 0.25), "lines"),
                      label.margin = unit(c(0, 0, 0, 0), "lines"),
                      label.r = unit(0.15, "lines"),
                      na.rm = FALSE, 

                      show.legend = FALSE,
                      inherit.aes = FALSE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y`.", call. = FALSE)
    }
    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRichTextNpc,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      label.padding = label.padding, 
      label.margin = label.margin, 
      label.r = label.r, 
      na.rm = na.rm, 
      # parse = parse,
      # check_overlap = check_overlap,
      # na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomRichTextNpc <- ggplot2::ggproto("GeomRichTextNpc", GeomRichText,
  required_aes = c("npcx", "npcy", "label"),
  default_aes = GeomRichText$default_aes,
  # default_aes = ggplot2::aes(
  #   colour = "black", size = 3.88, angle = 0, hjust = "inward",
  #   vjust = "inward", alpha = NA, family = "", fontface = 1, lineheight = 1.2
  # ),
  draw_panel = function(data, panel_params, coord, 
    label.padding = unit(c(0.25, 0.25, 0.25, 0.25), "lines"),
    label.margin = unit(c(0, 0, 0, 0), "lines"),
    label.r = unit(0.15, "lines"),
    na.rm = FALSE) {

    data$npcx <- compute_npcx(data$npcx)
    data$npcy <- compute_npcy(data$npcy)

    ranges <- coord$backtransform_range(panel_params)

    data$x <- ranges$x[1] + data$npcx * (ranges$x[2] - ranges$x[1])
    data$y <- ranges$y[1] + data$npcy * (ranges$y[2] - ranges$y[1])

    data$label %<>% str_mk()
    GeomRichText$draw_panel(
      data, panel_params, coord,
      label.padding, label.margin, label.r, na.rm
    )
  },
  draw_key = function(...) { grid::nullGrob() }
)
