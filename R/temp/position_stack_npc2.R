PositionStack <- ggproto("PositionStack", Position,
  type = NULL,
  vjust = 1,
  fill = FALSE,
  reverse = FALSE,

  setup_params = function(self, data) {
    flipped_aes <- has_flipped_aes(data)
    data <- flip_data(data, flipped_aes)
    list(
      var = self$var %||% stack_var(data),
      fill = self$fill,
      vjust = self$vjust,
      reverse = self$reverse,
      flipped_aes = flipped_aes
    )
  },

  setup_data = function(self, data, params) {
    data <- flip_data(data, params$flipped_aes)
    if (is.null(params$var)) {
      return(data)
    }

    data$ymax <- switch(params$var,
      y = data$y,
      ymax = as.numeric(ifelse(data$ymax == 0, data$ymin, data$ymax))
    )

    data <- remove_missing(
      data,
      vars = c("x", "xmin", "xmax", "y"),
      name = "position_stack"
    )
    flip_data(data, params$flipped_aes)
  },

  compute_panel = function(data, params, scales) {
    data <- flip_data(data, params$flipped_aes)
    if (is.null(params$var)) {
      return(data)
    }

    negative <- data$ymax < 0
    negative[is.na(negative)] <- FALSE

    neg <- data[negative, , drop = FALSE]
    pos <- data[!negative, , drop = FALSE]

    if (any(negative)) {
      neg <- collide(neg, NULL, "position_stack", pos_stack,
        vjust = params$vjust,
        fill = params$fill,
        reverse = params$reverse
      )
    }
    if (any(!negative)) {
      pos <- collide(pos, NULL, "position_stack", pos_stack,
        vjust = params$vjust,
        fill = params$fill,
        reverse = params$reverse
      )
    }

    data <- rbind(neg, pos)[match(seq_len(nrow(data)), c(which(negative), which(!negative))),]
    flip_data(data, params$flipped_aes)
  }
)

pos_stack <- function(df, width, vjust = 1, fill = FALSE) {
  n <- nrow(df) + 1
  y <- ifelse(is.na(df$y), 0, df$y)
  heights <- c(0, cumsum(y))

  if (fill) {
    heights <- heights / abs(heights[length(heights)])
  }
# We need to preserve ymin/ymax order. If ymax is lower than ymin in input, it should remain that way
  if (!is.null(df$ymin) && !is.null(df$ymax)) {
    max_is_lower <- df$ymax < df$ymin
  } else {
    max_is_lower <- rep(FALSE, nrow(df))
  }
  ymin <- pmin(heights[-n], heights[-1])
  ymax <- pmax(heights[-n], heights[-1])
  df$npcy <- (1 - vjust) * ymin + vjust * ymax
  # df$ymin <- as.numeric(ifelse(max_is_lower, ymax, ymin))
  # df$ymax <- as.numeric(ifelse(max_is_lower, ymin, ymax))
  df
}
