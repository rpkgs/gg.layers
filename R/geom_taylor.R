SD <- function(x, subn) {
  meanx <- mean(x, na.rm = TRUE)
  devx <- x - meanx
  ssd <- sqrt(sum(devx * devx, na.rm = TRUE) / (length(x[!is.na(x)]) - subn))
  return(ssd)
}

#' @export
taylor_data <- function(ref, model, sd.method = "sample", normalize = FALSE, ...) {
  R <- cor(ref, model, use = "pairwise")
  if (is.list(ref)) {
    ref <- unlist(ref)
  }
  if (is.list(model)) ref <- unlist(model)

  subn <- sd.method != "sample"
  sd.f <- SD(model, subn)
  sd.r <- SD(ref, subn)

  if (normalize) {
    sd.f <- sd.f / sd.r
    sd.r <- 1
  }
  data.table(sd.obs = sd.r, sd.mod = sd.f, R)
}

#' geom_taylor
#' 
#' @inheritParams ggplot2::geom_point
#' @param show.obs.label logical, whether to show the label of observed point.
#' @param obj.colour color of observed point. 
#' @param obj.size size of observed point.
#' 
#' @example R/examples/ex-geom_taylor.R
#' @export
geom_taylor <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
                        obs.colour = "black",
                        obs.size = 5,
                        show.obs.label = TRUE, 

                        na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE) {
  c(
    layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomTaylor,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        obs.colour = obs.colour, obs.size = obs.size,
        show.obs.label = show.obs.label, 
        na.rm = na.rm,
        ...
      )
    ),
    coord_equal(expand = FALSE)
  )
}


#' @importFrom dplyr group_by group_map
#' @importFrom data.table rbindlist data.table as.data.table
#' @export
GeomTaylor <- ggproto("GeomTaylor", GeomPoint,
  required_aes = c("sd.obs", "sd.mod", "R"),
  default_aes = aes(
    shape = 19, colour = "black", size = 5, fill = "black",
    alpha = 1, stroke = 0.5
  ),
  setup_data = function(data, params) {
    data %<>% transform(x = sd.mod * R, y = sd.mod * sin(acos(R)))
    maxsd <- max(data$sd.obs, data$sd.mod) * 1.2

    data$ymin_final <- 0
    data$ymax_final <- maxsd
    data$xmin_final <- 0
    data$xmax_final <- maxsd
    data
    # d2 = data %>% group_by(PANEL, group) %>%
    #   group_modify(~taylor_data(.$x, .$y))
    # d2
  },

  draw_panel = function(data, panel_params, coord, 
    show.obs.label = TRUE, 
    obs.colour = "black", obs.size = 5) {
    # panel_params$x.range <- c(0, maxsd)
    # panel_params$y.range <- c(0, maxsd)
    sd.obs = data$sd.obs[1]
    # maxsd <- max(data$sd.obs, data$sd.mod) * 1.1
    maxsd <- max(data$ymax_final, data$xmax_final) * 0.9

    data = dplyr::select(data, -ends_with("final"))
    ## TODO: future updates
    # coords <- coord$transform(data, panel_params)
    vp = panel_vp(panel_params)
    # g = rectGrob(x = 1, y = 1,
    #   hjust = 0, vjust = 0,
    #   vp = vp, default.units = "native",
    #   gp = gpar(fill = "red"), width = 0.5, height = 0.5)
    linewidth = 0.7 #data$linewidth[1]
    common <- list(
      colour   = "black",
      alpha    = 1,
      linewidth= linewidth,
      group    = data$group[1]
    )

    ## s大圆
    x <- cos(seq(0, pi / 2, by = 0.01)) * maxsd
    y <- sin(seq(0, pi / 2, by = 0.01)) * maxsd
    sd_big = new_data_frame(c(listk(x, y), common))
    grob_sd_big <- linesGrob(x, y,
      vp = vp, default.units = "native",
      gp = gpar(lwd = linewidth * .pt, lty = 1))
    # grob_sd_big <- GeomLine$draw_panel(sd_big, panel_params, coord)

    ## s小圆
    x <- cos(seq(0, pi / 2, by = 0.01)) * sd.obs
    y <- sin(seq(0, pi / 2, by = 0.01)) * sd.obs
    sd_sml = new_data_frame(c(listk(x, y, linetype = 5), common))
    sd_sml %<>% as.data.table()
    grob_sd_sml <- linesGrob(x, y,
      vp = vp, default.units = "native",
      gp = gpar(lwd = linewidth * .pt, lty = 5)
    )
    # grob_sd_sml <- GeomLine$draw_panel(sd_sml, panel_params, coord)

    ## 相关系数的虚线
    corr.lines <- c(0.2, 0.4, 0.6, 0.8, 0.9)
    d_corr = lapply(corr.lines, function(cor){
      data.table(x = c(0, maxsd * cor),
                 y = c(0, maxsd * sqrt(1 - cor ^ 2)), group = cor)
    }) %>% rbindlist() %>%
      mutate(group = factor(group), linewidth = linewidth, colour = "black", alpha = 0.1)
    grob_corr_line <- GeomLine$draw_panel(d_corr, panel_params, coord)

    ## RMSE的半圆
    rms.col = "darkgoldenrod"

    gamma <- pretty(c(0, maxsd), n = 5)
    gamma = gamma[gamma < maxsd]
    labelpos <- seq(45, 70, length.out = length(gamma))

    d_rmse = lapply(seq_along(gamma), function(i) {
      xcurve <- cos(seq(0, pi, by = 0.03)) * gamma[i] + sd.obs
      endcurve <- which(xcurve < 0)
      endcurve <- ifelse(length(endcurve), min(endcurve) - 1, 105)

      ycurve <- sin(seq(0, pi, by = 0.03)) * gamma[i]
      maxcurve <- xcurve * xcurve + ycurve * ycurve
      startcurve <- which(maxcurve > maxsd * maxsd)
      startcurve <- ifelse(length(startcurve), max(startcurve) + 1, 0)

      data.table(x = xcurve[startcurve:endcurve], y = ycurve[startcurve:endcurve], group = i)
    }) %>% rbindlist() %>%
      mutate(colour = rms.col, linetype = 5, linewidth = linewidth, alpha = 1)

    d_rmse_txt = lapply(seq_along(gamma), function(i) {
      xcurve <- cos(seq(0, pi, by = 0.03)) * gamma[i] + sd.obs
      ycurve <- sin(seq(0, pi, by = 0.03)) * gamma[i]
      data.table(x = xcurve[labelpos[i]], y = ycurve[labelpos[i]], label = gamma[i],
        group = 1L, size = 4, colour = rms.col, alpha = 1, angle = 0, hjust = -0.5, vjust = 1.5)
    }) %>% rbindlist()

    ## ticks
    ticks <- list(
      big = acos(seq(0.1, 0.9, by = 0.1)),
      med = acos(seq(0.05, 0.95, by = 0.1)),
      sml = acos(seq(0.91, 0.99, by = 0.01))
    )
    scales = c(0.96, 0.98, 0.99)

    d_tick = lapply(1:3, function(i) {
      tick = ticks[[i]]
      scale = scales[i]
      data.table(
        x = cos(tick) * maxsd,
        y = sin(tick) * maxsd,
        xend = cos(tick) * scale * maxsd,
        yend = sin(tick) * scale * maxsd,
        group = scale
      )
    }) %>% rbindlist()
    d_tick %<>% mutate(colour = "black", linewidth = 0.4, alpha = 1)

    ## tick labels
    angles <- 180/pi * c(ticks$big, acos(c(0.95, 0.99)))
    tick_mar = 1.015
    d_tick_txt <- data.table(
      x = cos(c(ticks$big, acos(c(0.95, 0.99)))) * tick_mar * maxsd,
      y = sin(c(ticks$big, acos(c(0.95, 0.99)))) * tick_mar * maxsd,
      label = c(seq(0.1, 0.9, by = 0.1), 0.95, 0.99),
      size = 4, angle = angles, colour = "black", alpha = 1, hjust = 0
    )
    d_tick_title = data.table(
      x = 0.82 * maxsd, y = 0.82 * maxsd, label = "Correlation",
      size = 5, colour = "black", alpha = 1, angle = 315
    )

    d_obs = data.frame(x = sd.obs, y = 0,
                       shape = 19, colour = obs.colour, size = obs.size,
                       fill = obs.colour,
                       alpha = 1, stroke = 0.5)
    label_obs = ifelse(show.obs.label, "Observed", "")
    d_obs_txt = cbind(d_obs, label = label_obs, fontsize = 8, vjust = -1, angle = 0)

    grid::gList(
      # g,
      GeomPoint$draw_panel(data, panel_params, coord),
      GeomPoint$draw_panel(d_obs, panel_params, coord),
      GeomText$draw_panel(d_obs_txt, panel_params, coord),

      grob_corr_line,
      grob_sd_big,
      grob_sd_sml,

      GeomSegment$draw_panel(d_tick, panel_params, coord),
      GeomText$draw_panel(d_tick_txt, panel_params, coord),
      GeomText$draw_panel(d_tick_title, panel_params, coord),

      GeomText$draw_panel(d_rmse_txt, panel_params, coord),
      GeomLine$draw_panel(d_rmse, panel_params, coord)
    )
  }
)


# coord_taylor <- function(xlim = NULL, ylim = NULL, expand = TRUE,
#                             default = FALSE, clip = "on") {
#   ggproto(NULL, CoordTaylor,
#     limits = list(x = xlim, y = ylim),
#     expand = expand,
#     default = default,
#     clip = clip
#   )
# }

# CoordTaylor <- ggproto("CoordTaylor", Coord,
#   setup_panel_params = function(self, scale_x, scale_y, params = list()) {
#     c(
#       view_scales_from_scale(scale_x, self$limits$x, self$expand),
#       view_scales_from_scale(scale_y, self$limits$y, self$expand)
#     )
#   }
# )
