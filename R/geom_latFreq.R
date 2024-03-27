#' geom_latFreq
#' @inheritParams ggplot2::geom_point
#' @param bbox bounding box of the plot, in the form of `c(xmin, xmax, ymin, ymax)`.
#' @param options parameters of [make_latFreq()]
#' 
#' @example R/examples/ex-geom_latFreq.R
#' @export
geom_latFreq <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
                        # obs.colour = "black",
                        # obs.size = 5,
                        options = list(),
                        bbox = c(185, 240, -60, 90),
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLatFreq,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      options = options,
      bbox = bbox,
      na.rm = na.rm,
      ...
    )
  )
}

#' @importFrom dplyr group_by group_map
#' @importFrom data.table rbindlist data.table as.data.table
#' @export
GeomLatFreq <- ggproto("GeomLatFreq", Geom,
  required_aes = c("y", "z"),
  # default_aes = aes(
  #   shape = 19, colour = "black", size = 5, fill = "black",
  #   alpha = 1, stroke = 0.5
  # ),

  setup_data = function(data, params) {
    ylim = params$bbox[3:4]
    data %<>% subset(y >= ylim[1] & y <= ylim[2])
    # data %<>% transform(x = sd.mod * R, y = sd.mod * sin(acos(R)))
    # maxsd <- max(data$sd.obs, data$sd.mod) * 1.2
    # data$ymin_final <- 0
    # data$ymax_final <- maxsd
    # data$xmin_final <- 0
    # data$xmax_final <- maxsd
    data
  },

  draw_panel = function(data, panel_params, coord, options, bbox) {
    grob = as.grob(function(){
      .param = c(data[c("y", "z")], options)
      do.call(make_latFreq, .param)
      # make_latFreq(data$y, data$y, ...)
    })

    bbox2 = bbox2npc(bbox, panel_params)
    unit = "npc"

    vp = make_vp_lims(lims, bbox2, unit, clip = "off")
    g = gTree(children = gList(grob), vp = vp)
    # write_fig(g, "debug")
    # g = place_annotation(grob, panel_params, bbox, unit, clip = "off")
    grid::gList(g)
  }
)

round_decade <- function(x) {
  p <- floor(log10(abs(x)))
  if (x > 1000) p <- p - 1
  times <- 10^p
  round(x / times) * times
}


#' make_latFreq
#' @param ... ignored
#' @keywords internal
#' @export 
make_latFreq <- function(
    y, z,
    length.out = 1e4,
    tcl = 0.4, cex = 1,
    xlab = "", ylab = "",
    xlabels = TRUE, ylabels = TRUE,
    ylim = NULL, zlim = NULL, zlim_ratio = c(-1, 1),
    prob_z = 0.9,
    is_spatial = FALSE,
    zticks = NULL,
    digit = 1,
    family = "Times",
    debug = FALSE,
    ...) {

  if (is.null(ylim)) ylim <- range(y, na.rm = TRUE)

  yaxt <- "s"
  if (is_spatial) {
    yaxt <- "n"
    yticks <- if (diff(range(y)) > 60) seq(-60, 90, 30) else pretty(y)
  }

  if (is.null(zlim)) {
    zmax <- quantile(abs(z), prob_z, na.rm = TRUE)
    zmax <- if (zmax > 0.5) round_decade(zmax) else round(zmax, 1)
    zlim <- zlim_ratio * zmax
  } else {
    zmax <- max(zlim)
  }
  
  # 这里算法需要改进
  # ## 方案1
  # d <- data.table(vals = z, x = y)
  # d_group <- d[, .(value = mean(vals, na.rm = TRUE)), .(x)] %>%
  #   .[x <= ylim[2] & x >= ylim[1]]
  # d_group[is.na(value), value := 0]

  ## 方案2
  d_group = upper_envelope(y, z, 0.5, nchunk = 400)[, .(x, value = mid)]

  if (!debug) {
    old.par = par(mar = c(0, 0, 0, 0), mgp = c(1, 0, 0), oma = c(0, 0, 0, 0))
    # on.exit(par(old.par))
  }

  draw_polygon(d_group$value, d_group$x,
    length.out = nrow(d_group), type = "vertical",
    tcl = tcl,
    # ...,
    ylim = ylim, zlim = zlim,
    xaxs = "i", yaxs = "i",
    xlab = "", ylab = "",
    xaxt = "n", yaxt = yaxt
  )

  if (is_spatial) {
    at <- seq(-60, 90, 10)
    abline(h = seq(-30, 60, 30), lty = 3, col = "grey", lwd = 0.5)
    if (ylabels) {
      ylabels <- as.character(yticks)
      ylabels[c(1, length(ylabels))] <- " "
    }
    axis(side = 2, tcl = tcl, at = yticks, labels = ylabels, cex.axis = cex) # label_sp(yticks)
    axis(
      side = 2, tcl = tcl / 2, at = seq(-60, 90, 10), labels = rep("", length(at)),
      lwd = 0.5, cex.axis = cex
    )

    if (is.null(zticks)) {
      xticks_major <- c(-1, 0, 1) * zmax
      xticks_minor <- c(-1, 1) * zmax / 2
      axis(
        side = 1, tcl = tcl / 2, at = xticks_minor, labels = rep("", length(xticks_minor)),
        lwd = 0.5, cex.axis = cex
      )
    } else {
      xticks_major <- zticks
    }
    axis(side = 1, tcl = tcl, at = xticks_major, labels = xticks_major, cex.axis = cex)
  }
}
