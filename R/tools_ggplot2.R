place_annotation <- function (grob, panel_params,
  bbox = c(0.5, 1, 0, 1),
  unit = "npc",
  # xscale = FALSE,
  # yscale = FALSE,
  clip = "on", ...)
{
  width <- diff(bbox[1:2])
  height <- diff(bbox[3:4])
  x <- bbox[1]
  y <- bbox[3]

  l <- get_scales(panel_params)
  xlim = l$xlim
  ylim = l$ylim

  # print2(bbox, l, x, y, width, height)
  vp = viewport(x, y, width, height,
    name = "panel.annotation",
    xscale = xlim, yscale = ylim,
    just = c(0, 0),
    default.units = unit, clip = clip
  )
  gTree(children = gList(grob), vp = vp)
}

make_vp_lims <- function(lims,
  bbox = c(0.5, 1, 0, 1),
  unit = "npc",
  clip = "on",
  # xscale = FALSE,
  # yscale = FALSE,
  ...)
{
  width <- diff(bbox[1:2])
  height <- diff(bbox[3:4])
  x <- bbox[1]
  y <- bbox[3]

  # lims$x %<>% unit(unit)
  # lims$y %<>% unit(unit)
  # print(lims)
  # lims <- get_scales(panel_params)
  # print2(bbox, l, x, y, width, height)
  vp <- viewport(x, y, width, height,
    name = "panel.annotation",
    # xscale = lims$x, yscale = lims$y,
    just = c(0, 0),
    default.units = unit, clip = clip
  )
  vp
}

bbox2npc <- function(bbox, panel_params) {
  c(scale_x(bbox[1:2], panel_params),
    scale_y(bbox[3:4], panel_params))
}

get_scales <- function(panel_params) {
  xlim = panel_params$x$continuous_range
  ylim = panel_params$y$continuous_range
  listk(xlim, ylim)
}

# 2023-08-05, Yuxuan Xie
# When the data contains `sf` objects, the structure of `panel`
# will change and `panel_params` will not contain `x` and `y`,
# but `x_range` and `y_range` instead.
scale_x <- function(x, panel_params) {
  # the old version
  # lims <- panel_params$x$continuous_range
  x_name <- match.arg("x", names(panel_params))
  if (x_name == "x") lims <- panel_params$x$continuous_range
  if (x_name == "x_range") lims <- panel_params$x_range
  (x - lims[1]) / (lims[2] - lims[1])
}

scale_y <- function(y, panel_params) {
  # the old version
  # lims <- panel_params$y$continuous_range
  y_name <- match.arg("y", names(panel_params))
  if (y_name == "y") lims <- panel_params$y$continuous_range
  if (y_name == "y_range") lims <- panel_params$y_range
  (y - lims[1]) / (lims[2] - lims[1])
}

panel_vp <- function(panel_params) {
  viewport(
    xscale = panel_params$x$continuous_range,
    yscale = panel_params$y$continuous_range
  )
}
