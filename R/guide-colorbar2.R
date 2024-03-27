#' Continuous colour bar guide
#'
#' Colour bar guide shows continuous colour scales mapped onto values.
#' Colour bar is available with `scale_fill` and `scale_colour`.
#' For more information, see the inspiration for this function:
#' \href{http://www.mathworks.com/help/techdoc/ref/colorbar.html}{Matlab's colorbar function}.
#'
#' Guides can be specified in each `scale_*` or in [guides()].
#' `guide="legend"` in `scale_*` is syntactic sugar for
#' `guide=guide_legend()` (e.g. `scale_colour_manual(guide = "legend")`).
#' As for how to specify the guide for each scale in more detail,
#' see [guides()].
#' 
#' @inheritParams ggplot2::guide_colourbar
#' 
#' @return A guide object
#' @export
#' @family guides
#' 
#' @examples
#' df <- expand.grid(X1 = 1:10, X2 = 1:10)
#' df$value <- df$X1 * df$X2
#'
#' p1 <- ggplot(df, aes(X1, X2)) + geom_tile(aes(fill = value))
#' p2 <- p1 + geom_point(aes(size = value))
#'
#' # Basic form
#' p1 + scale_fill_continuous(guide = "colourbar")
#' p1 + scale_fill_continuous(guide = guide_colourbar())
#' p1 + guides(fill = guide_colourbar())
#'
#' # Control styles
#'
#' # bar size
#' p1 + guides(fill = guide_colourbar(theme = theme(
#'   legend.key.width  = unit(0.5, "lines"),
#'   legend.key.height = unit(10, "lines")
#' )))
#'
#'
#' # no label
#' p1 + guides(fill = guide_colourbar(theme = theme(
#'   legend.text = element_blank()
#' )))
#'
#' # no tick marks
#' p1 + guides(fill = guide_colourbar(theme = theme(
#'   legend.ticks = element_blank()
#' )))
#'
#' # label position
#' p1 + guides(fill = guide_colourbar(theme = theme(
#'   legend.text.position = "left"
#' )))
#'
#' # label theme
#' p1 + guides(fill = guide_colourbar(theme = theme(
#'   legend.text = element_text(colour = "blue", angle = 0)
#' )))
#'
#' # small number of bins
#' p1 + guides(fill = guide_colourbar(nbin = 3))
#'
#' # large number of bins
#' p1 + guides(fill = guide_colourbar(nbin = 100))
#'
#' # make top- and bottom-most ticks invisible
#' p1 +
#'   scale_fill_continuous(
#'     limits = c(0,20), breaks = c(0, 5, 10, 15, 20),
#'     guide = guide_colourbar(nbin = 100, draw.ulim = FALSE, draw.llim = FALSE)
#'   )
#'
#' # guides can be controlled independently
#' p2 +
#'   scale_fill_continuous(guide = "colourbar") +
#'   scale_size(guide = "legend")
#' p2 + guides(fill = "colourbar", size = "legend")
#'
#' p2 +
#'   scale_fill_continuous(guide = guide_colourbar(theme = theme(
#'     legend.direction = "horizontal"
#'   ))) +
#'   scale_size(guide = guide_legend(theme = theme(
#'     legend.direction = "vertical"
#'   )))
guide_colourbar2 <- function(
  title = waiver(),
  theme = NULL,
  nbin = NULL,
  display = "raster",
  raster = deprecated(),
  alpha = NA,
  draw.ulim = TRUE,
  draw.llim = TRUE,
  position = NULL,
  direction = NULL,
  reverse = FALSE,
  order = 0,
  available_aes = c("colour", "color", "fill"),
  ...
) {
  if (lifecycle::is_present(raster)) {
    deprecate_soft0("3.5.0", "guide_colourbar(raster)", "guide_colourbar(display)")
    check_bool(raster)
    display <- if (raster) "raster" else "rectangles"
  }
  display <- arg_match0(display, c("raster", "rectangles", "gradient"))
  nbin <- nbin %||% switch(display, gradient = 15, 300)

  theme <- ggplot2:::deprecated_guide_args(theme, ...)
  if (!is.null(position)) {
    position <- arg_match0(position, c(.trbl, "inside"))
  }
  ggplot2:::check_number_decimal(alpha, min = 0, max = 1, allow_na = TRUE)

  new_guide(
    title = title,
    theme = theme,
    nbin = nbin,
    display = display,
    alpha = alpha,
    draw_lim = c(isTRUE(draw.llim), isTRUE(draw.ulim)),
    position = position,
    direction = direction,
    reverse = reverse,
    order = order,
    available_aes = available_aes,
    name = "colourbar",
    super = GuideColourbar2
  )
}

#' @export
#' @rdname guide_colourbar2
guide_colorbar2 <- guide_colourbar2

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto GuideColourbar
#' @importFrom rlang exprs
#' @export
GuideColourbar2 <- ggproto(
  "GuideColourbar", GuideColourbar,

  params = list(
    # title
    title = waiver(),

    # theming
    theme = NULL,
    default_ticks = element_line(colour = "white", linewidth = 0.5 / .pt),
    default_frame = element_blank(),

    # bar
    nbin = 300,
    display = "raster",
    alpha = NA,

    draw_lim = c(TRUE, TRUE),

    # general
    direction = NULL,
    reverse = FALSE,
    order = 0,

    # parameter
    name = "colourbar",
    hash = character(),
    position = NULL
  ),

  available_aes = c("colour", "color", "fill"),

  hashables = exprs(title, key$.label, decor, name),

  build_decor = function(decor, grobs, elements, params) {
    if (params$display == "raster") {
      image <- switch(
        params$direction,
        "horizontal" = t(decor$colour),
        "vertical"   = rev(decor$colour)
      )
      grob <- rasterGrob(
        image  = image,
        width  = 1,
        height = 1,
        default.units = "npc",
        gp = gpar(col = NA),
        interpolate = TRUE
      )
    } else if (params$display == "rectangles") {
      if (params$direction == "horizontal") {
        width  <- 1 / nrow(decor)
        height <- 1
        x <- (seq(nrow(decor)) - 1) * width
        y <- 0
      } else {
        width  <- 1
        height <- 1 / nrow(decor)
        y <- (seq(nrow(decor)) - 1) * height
        x <- 0
      }
      grob <- rectGrob(
        x = x, y = y,
        vjust = 0, hjust = 0,
        width = width, height = height,
        default.units = "npc",
        gp = gpar(col = NA, fill = decor$colour)
      )
    } else if (params$display == "gradient") {
      check_device("gradients", call = expr(guide_colourbar()))
      value <- if (isTRUE(params$reverse)) {
        rescale(decor$value, to = c(1, 0))
      } else {
        rescale(decor$value, to = c(0, 1))
      }
      position <- switch(
        params$direction,
        horizontal = list(y1 = unit(0.5, "npc"), y2 = unit(0.5, "npc")),
        vertical   = list(x1 = unit(0.5, "npc"), x2 = unit(0.5, "npc"))
      )
      gradient <- inject(linearGradient(decor$colour, value, !!!position))
      grob <- rectGrob(gp = gpar(fill = gradient, col = NA))
    }

    frame <- element_grob(elements$frame, fill = NA)

    l = make_triangle(cols = decor$colour)
    frame %<>% placeGrob(grob, row = 2, col = 1)
    frame %<>% placeGrob(l$lower, row = 3, col = 1)
    frame %<>% placeGrob(l$upper, row = 1, col = 1)

    list(bar = grob, frame = frame, ticks = grobs$ticks)
  }
)
