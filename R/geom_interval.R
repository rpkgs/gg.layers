cal_interval <- function(y, interval = 0.8, na.rm = TRUE) {
  alpha <- 1 - interval
  qs <- quantile(y, probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
  mean <- mean(y, na.rm = TRUE)
  median <- median(y, na.rm = TRUE)
  listk(ymin = qs[1], ymax = qs[2], mean, median)
}

#' @import ggplot2
StatInterval <- ggproto("StatInterval", Stat,
  required_aes = c("x", "y"),
  # setup_data = function(data, params) {
  #   data
  # },
  compute_group = function(self, data, scales,
                           fun_middle = "median", interval = 0.8, na.rm = TRUE) {
    data <- as.data.table(data)
    by <- setdiff(colnames(data), c("y"))
    df <- data[, cal_interval(y, interval, na.rm = TRUE), by]
    df$y <- df[[fun_middle]]
    df %>% reorder_name(c("x", "y", "ymin", "ymax", "median", "mean"))
  }
)

#' @rdname geom_interval
#' @export
stat_interval <- function(
    mapping = NULL, data = NULL,
    geom = GeomInterval,
    position = "identity",
    interval = 0.8,
    fun_middle = "median",
    # by = "x",
    na.rm = FALSE, show.legend = NA,
    inherit.aes = TRUE, ...) {
  layer(
    stat = StatInterval,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      interval = interval,
      fun_middle = fun_middle,
      na.rm = na.rm, ...
    )
  )
}

#' @keywords internal
#' @export
draw_key_line2 <- function(data, params, size) {
  if (is.null(data$linewidth)) {
    data$linewidth <- 0.5
  }
  lwd <- min(data$linewidth, min(size) / 4)
  # data$alpha
  segmentsGrob(0.1, 0.5, 0.9, 0.5, gp = gpar(
    col = alpha(data$colour %||% data$fill %||% "black"),
    # fill = alpha(params$arrow.fill %||% data$colour %||%
    #   data$fill %||% "black", data$alpha),
    lwd = (data$linewidth %||% 0.5) * .pt,
    lty = data$linetype %||% 1,
    lineend = params$lineend %||% "butt"
  ), arrow = params$arrow)
}

#' @keywords internal
#' @export
draw_key_polygon2 <- function(data, params, size) {
  if (is.null(data$linewidth)) {
    data$linewidth <- 0.5
  }
  lwd <- min(data$linewidth, min(size) / 4)
  rectGrob(
    width = unit(1, "npc") - unit(lwd, "mm"),
    height = unit(1, "npc") - unit(lwd, "mm"),
    gp = gpar(
      col = data$colour %||% NA,
      fill = alpha(data$fill %||% "grey20"), # , data$alpha
      lty = data$linetype %||% 1, 
      lwd = lwd * .pt,
      linejoin = params$linejoin %||% "mitre",
      lineend = params$lineend %||% "butt"
    )
  )
}

GeomInterval <- ggproto("GeomInterval", GeomRibbon,
  default_aes = aes(colour = "black", fill = "red", linewidth = 0.5, alpha = 0.6),
  setup_data = function(data, param) {
    data
  },
  # draw_key = GeomLine$draw_key,
  draw_key = draw_key_line2,
  draw_group = function(self, data, panel_params, coord,
                        alpha.line = 1, na.rm = FALSE) {
    data_ribbon <- mutate(data, colour = "transparent")
    data_line <- mutate(data, alpha = alpha.line)

    gTree(children = gList(
      GeomRibbon$draw_panel(data_ribbon, panel_params, coord, na.rm = na.rm),
      GeomLine$draw_panel(data_line, panel_params, coord, na.rm = na.rm)
    ))
  }
)

#' geom_interval
#'
#' @inheritParams ggplot2::geom_ribbon
#'
#' @example R/examples/ex-geom_interval.R
#' @export
geom_interval <- function(mapping = NULL, data = NULL,
                          stat = "interval", position = "identity",
                          ...,
                          fun_middle = "median", interval = 0.8,
                          alpha.line = 1,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomInterval,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      fun_middle = fun_middle,
      interval = interval,
      alpha.line = alpha.line, 
      ...
    )
  )
}
