# https://github.com/slowkow/ggrepel/blob/master/R/position-nudge-repel.R

#' @export
position_nudge_npc <- function(x = 0, y = 0) {
  ggproto(NULL, PositionNudgeNpc,
    x = x,
    y = y
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionNudgeNpc <- ggproto("PositionNudgeNpc", Position,
  x = 0,
  y = 0,

  setup_params = function(self, data) {
    list(x = self$x, y = self$y)
  },

  compute_panel = function(self, data, params, scales) {

    nudge_npc <- function(width, nudge) {
      function(x) {
        x + width * nudge
      }
    }
    width = scales$x$range$range %>% diff()
    height = scales$y$range$range %>% diff()

    x_orig <- data$x
    y_orig <- data$y
    if (any(params$x != 0)) {
        if (any(params$y != 0)) {
            data <- transform_position(data, function(x) x + width * params$x, function(y) y + height*params$y)
        } else {
            data <- transform_position(data, function(x) x + width*params$x, NULL)
        }
    } else if (any(params$y != 0)) {
        data <- transform_position(data, NULL, function(y) y + height*params$y)
    }
    data$nudge_x <- data$x
    data$nudge_y <- data$y
    data$x <- x_orig
    data$y <- y_orig
    data
  }
)
