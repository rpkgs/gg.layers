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
  required_aes = c("npcx", "npcy", "label"), #
  # default_aes = GeomRichText$default_aes,
  default_aes = ggplot2::aes(
    # x = 0.5,
    # y = 0.5,
    colour = "black",
    fill = NA, label.color = NA, label.size = 0.25,
    size = 3.88, angle = 0, 
    hjust = 0.5, vjust = 0.5, 
    # hjust = 0, vjust = 1, 
    alpha = NA, family = "", fontface = 1, lineheight = 1.2
  ),
  # setup_data = function(data, params) {
  #   data
  # },
  draw_panel = function(data, panel_params, coord,
    label.padding = unit(c(0.25, 0.25, 0.25, 0.25), "lines"),
    label.margin = unit(c(0, 0, 0, 0), "lines"),
    label.r = unit(0.15, "lines"),
    na.rm = FALSE) {

    ranges <- coord$backtransform_range(panel_params)

    # data$x <- compute_npcx(data$x)
    # data$y <- compute_npcy(data$y)
    # data$x <- ranges$x[1] + data$x * (ranges$x[2] - ranges$x[1])
    # data$y <- ranges$y[1] + data$x * (ranges$y[2] - ranges$y[1])

    x <- compute_npcx(data$npcx)
    y <- compute_npcy(data$npcy)
    data$x <- ranges$x[1] + x * (ranges$x[2] - ranges$x[1])
    data$y <- ranges$y[1] + y * (ranges$y[2] - ranges$y[1])

    data$label %<>% str_mk()

    GeomRichText$draw_panel(
      data, panel_params, coord,
      label.padding, label.margin, label.r, na.rm
    )
  },
  draw_key = function(...) { grid::nullGrob() }
)

#' @rdname geom_richtext_npc
#' @importFrom ggplot2 theme_get
#' @export
annotate_richtext_npc <- function(x, y, label,
                              size = 5, family = "", ...) {

  if (family == "") family = theme_get()$text$family
  data <- data.frame(x, y, label)
  geom_richtext_npc(
    data = data,
    aes(npcx = x, npcy = y, label = label), size = size, family = family, ...
  )
}

#' @rdname geom_richtext_npc
#' @export
annotate_richlabel_npc <- function(x, y, label,
  size = 5, family = "", fill = "white", label.color = "black", ...) {
  
  annotate_richtext_npc(x, y, label, size, family, 
    fill = fill, label.color = label.color, ...)
}

#' @export
annotate_text_npc <- function(x, y, label,
                              size = 5, family = "", ...) {

  if (family == "") family = theme_get()$text$family
  data <- data.frame(x, y)
  geom_text_npc(
    data = data,
    aes(npcx = x, npcy = y), size = size, family = family, label = label, ...
  )
}

#' @rdname geom_richtext_npc
#' @export
#' @importFrom ggpp geom_text_npc
annotate_label_npc <- function(x, y, label,
  size = 5, family = "", fill = "white", label.color = "black", ...) {
  
  annotate_text_npc(x, y, label, size, family, 
    fill = fill, label.color = label.color, ...)
}
