# ' @importFrom ggpmisc stat_poly_eq
# stat_reg_coef <- function() {
#     my.format <- 'R^2 ~`=`~"%.2f" * ","~slope~`=`~%.3g*", "*pvalue ~`=`~"%.3f"'
#     stat_poly_eq(formula = y ~ x,
#                  output.type = "numeric", parse = TRUE,
#                  mapping = aes(color = NULL,
#                                label = sprintf(my.format, after_stat(adj.r.squared), # adj.r.squared
#                                                after_stat(b_1), after_stat(p.value))))
# }

#' @inheritParams stats::lm
#' @param position "dodge" or "identity"
#'
#' @details
#' - `b`: the object returned by [broom::tidy()]
#' - `s`: the object returned by [broom::glance()]
#' 
#'    + "*R*^2 = {str_num(s$r.squared, digits)}"
#' 
#' - `slope`: 
#' - `pvalue`: 
#' - `pcode`: significant code, e.g., `**`, `*`, `-`
#' 
#' @example R/examples/ex-stat_reg_gof.R
#' @importFrom broom tidy glance
#' @import glue rtrend
#' @rdname stat_gof
#' @export
stat_reg <- function(mapping = NULL, data = NULL,
    formula = y ~ x, digits = 2, units = "",
    format = paste0(
      'Slope = {str_num(slope, digits)}{unit}',
      ', p-value = {str_num(pvalue, digits)}'),
    fun_slope = NULL,
    x = 0, y = 1, hjust = 0, vjust = 1, mar = 0.03, height.factor = 1.2,
    family = "Times",
    color = NULL,
    position = "dodge",
    ...)
{
  fun <- function(data, coords) {
    if (is.null(fun_slope)) {
      l = lm(formula, data)
      b = broom::tidy(l)
      s = broom::glance(l)
      slope = b$estimate[2]
      pvalue = b$p.value[2]
    } else {
      l = tryCatch({
        fun_slope(data$y, data$x)
      }, error = function(e) {
        message(sprintf('%s', e$message))
      })
      slope = l["slope"]
      pvalue = l["pvalue"]
    }
    pcode = signif_code(pvalue)
    if (is.na(pvalue)) pvalue = 0 # note here
    # dots = list(...)
    # color = if (is.null(dots$color)) data$colour[1] else dots$color
    if (is.null(color)) color = data$colour[1]

    group = data$group[1] %||% 0
    unit = ifelse(length(units) > 1, units[data$PANEL[1]], units)
    if (unit != "") unit %<>% paste0(" ", .)

    label = glue(format)
    # lines = strsplit(label, "<br>")[[1]]
    # height = sapply(lines, text_height_npc) %>% sum()
    g = richtextGrob(label, x, y, hjust, vjust, mar, color = color, ...)
    height = grobHeight(g) * height.factor

    if (position == "dodge")
      y = unit(margin_adj(y, mar), "npc") - pmax(group - 1, 0) * height # unit
    richtextGrob(label, x, y, hjust, vjust, mar,
        family = family, color = color, ...)
  }
  grid_group(fun, mapping, data)
}

#' @export
signif_code <- function(pvalue) {
  at <- c(0, 0.001, 0.01, 0.05, 1)
  lev <- c("***", "**", "*", "-")
  # include.lowest = FALSE, right = TRUE
  # **: (0   , 0.01]
  # * : (0.01, 0.05]
  # - : (0.05, 1   ]
  at <- c(0, 0.01, 0.05, 1)
  lev <- c("**", "*", "-")
  cut(pvalue, at, lev, include.lowest = TRUE) %>% as.character()
}
