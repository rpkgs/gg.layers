#' guide_coloursteps2
#'#' @param title A title for the guide.
#' @param theme A theme object for rendering the guide.
#' @param alpha Alpha transparency level.
#' @param even.steps Logical; should the steps be evenly spaced?
#' @param show.limits Logical; should the limits be shown?
#' @param direction Direction of the guide ("horizontal" or "vertical").
#' @param reverse Logical; should the guide be reversed?
#' @param order Order of the guide.
#' @param available_aes Available aesthetics for this guide.
#' @param barheight Height of the color bar. Deprecated in ggplot2 3.5.0.
#' @param ... Additional arguments passed to the guide.
#'#' @example R/examples/ex-stat_cut.R
#' @export
guide_coloursteps2 <- function(
  title = waiver(),
  theme = NULL,
  alpha = NA,
  even.steps  = TRUE,
  show.limits = NULL,
  direction = NULL,
  reverse = FALSE,
  order = 0,
  available_aes = c("colour", "color", "fill"),
  barheight = unit(0.9, "npc"),
  ...
) {

  theme <- ggplot2:::deprecated_guide_args(theme, barheight=barheight, ...)
  ggplot2:::check_number_decimal(alpha, min = 0, max = 1, allow_na = TRUE)

  new_guide(
    title = title,
    theme = theme,
    alpha = alpha,
    even.steps  = even.steps,
    show.limits = show.limits,
    direction = direction,
    reverse = reverse,
    order = order,
    super = GuideColoursteps2
  )
}

#' @export
#' @rdname guide_coloursteps2
guide_colorsteps2 <- guide_coloursteps2

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GuideColoursteps2 <- ggproto(
  "GuideColoursteps2", GuideColoursteps,

  build_decor = function(decor, grobs, elements, params) {

    size <- abs(decor$max - decor$min)
    just <- as.numeric(decor$min > decor$max)
    lwd <- 0.4

    n = nrow(decor)
    bh = sum(size) / n

    brks = seq(0, 1, length.out = n-2+1)
    nbrk = length(brks)
    decor2 = decor[2:(n-1), ] %>%
      mutate(min = brks[1:(nbrk - 1)], max = brks[2:nbrk])

    fill2 = decor$colour[2:(n-1)]
    size2 = size[2:(n-1)] * n / (n - 2)

    gp <- gpar(lwd = lwd, col = "black", fill = fill2)

    if (params$direction == "vertical") {
      grob <- rectGrob(
        x = 0, y = decor2$min,
        width = 1, height = size2,
        vjust = just, hjust = 0, gp = gp
      )
    } else {
      grob <- rectGrob(
        x = decor2$min, y = 0,
        height = 1, width = size2,
        hjust = just, vjust = 0, gp = gp
      )
    }

    key.layout <- grid.layout(
      nrow = 3, ncol = 1, # respect = TRUE,
      heights = c(bh, (n - 2) * bh, bh), widths = 1, just = c(0, 0)
    )
    l <- make_triangle(cols = decor$colour, lwd = lwd)
    .frame <- frameGrob(layout = key.layout)
    .frame %<>% placeGrob(grob, row = 2, col = 1)
    .frame %<>% placeGrob(l$lower, row = 3, col = 1)
    .frame %<>% placeGrob(l$upper, row = 1, col = 1)

    frame <- element_grob(elements$frame, fill = NA)

    list(bar = .frame, frame = frame, ticks = grobs$ticks)
  }
)
