# ' @importFrom ggpmisc stat_poly_eq
# stat_reg_coef <- function() {
#     my.format <- 'R^2 ~`=`~"%.2f" * ","~slope~`=`~%.3g*", "*pvalue ~`=`~"%.3f"'
#     stat_poly_eq(formula = y ~ x,
#                  output.type = "numeric", parse = TRUE,
#                  mapping = aes(color = NULL,
#                                label = sprintf(my.format, after_stat(adj.r.squared), # adj.r.squared
#                                                after_stat(b_1), after_stat(p.value))))
# }

#' markdown superscript and subscript
#'
#' @param x character vector
#' @examples
#' x = c(
#'     "gC m^{-2} d^{-1}",
#'     "gC m^-2 d^-1",
#'     "gC m_{-2} d_{-1}",
#'     "gC m_-2 d_-1", 
#'     "gC \n mm/d"
#' )
#' str_mk(x)
#' @author Dongdong Kong
#' @importFrom stringr str_replace_all
#' @export
str_mk <- function(x) {
    replacement <- c(
        "(\\^\\{?)([\\w\\+\\-\\*\\\\]*)(\\}?)" = "<sup>\\2</sup>",
        "(\\_\\{?)([\\w\\+\\-\\*\\\\]*)(\\}?)" = "<sub>\\2</sub>", 
        "\n" = "<br>"
    )
    str_replace_all(x, replacement)
}

#' Rounding of Numbers, and convert to string
#'
#' @inheritParams base::round
#'
#' @examples
#' format(round(2, 2)) # 2
#' str_num(2, 2)    # 2.00
#' @export
str_num <- function(x, digits = 2) {
    fmt = sprintf("%%.%df", digits)
    sprintf(fmt, x)
}

margin_adj <- function(x, margin) {
    if (is.numeric(x)) x = ifelse(x > 0.5, x - margin, x + margin)
    x
}

# adjust label location slightly by `mar`

#' richtextGrob
#' 
#' @inheritParams gridtext::richtext_grob
#' @param ... other parameters to [gridtext::richtext_grob()]
#' 
#' @examples
#' text <- c(
#'     "Some text **in bold.**",
#'     "Linebreaks<br>Linebreaks<br>Linebreaks",
#'     "*x*<sup>2</sup> + 5*x* + *C*<sub>*i*</sub>",
#'     "Some <span style='color:blue'>blue text **in bold.**</span><br>And *italics text.*<br>And some <span style='font-size:18pt; color:black'>large</span> text."
#' )
#' @export
richtextGrob <- function(label, x = 0, y = 1, hjust = 0, vjust = 1, mar = 0.02,
    padding = unit(c(1, 1, 1, 1)*0, "pt"),
    lineheight = 1.5,
    color = "black",
    ...)
{
    label %<>% str_mk()
    x %<>% margin_adj(mar)
    y %<>% margin_adj(mar)
    
    gp = gpar(col = color, lineheight = lineheight)
    richtext_grob(label,
        x = x, y = y,
        hjust = hjust, vjust = vjust, padding = padding, ..., gp = gp
    )
}

#' `stat` of good-of-fitting
#'
#' @inheritParams gridtext::richtext_grob
#' @param ... other parameters to [gggrid::grid_panel()]
#'
#' @importFrom gridtext richtext_grob
#' @importFrom grid gpar grobHeight
#' @import gggrid
#' @export
stat_gof <- function(
    mapping = NULL, data = NULL,
    format = "NSE = {str_num(NSE,2)}, R^2 = {str_num(R2, 2)} \n RMSE = {str_num(RMSE,2)}",
    x = 0, y = 1, hjust = 0, vjust = 1, mar = 0.02, show.bias = TRUE)
{
    fun <- function(data, coords, ...) {
        g = GOF(data$x, data$y)
        label = with(g, glue(format))
        if (show.bias) label = sprintf("%s \n Bias(%%) = %.2f%%", label, g$Bias_perc)
        richtextGrob(label, x, y, hjust, vjust, mar, ...)
    }
    list(grid_panel(fun, mapping, data),
         geom_abline(slope = 1, color = "red"))
}

#' `stat` of regression coefficient and good-of-fitting information
#'
#' @inheritParams stat_gof
#' @inheritParams stats::lm
#' @param position "dodge" or "identity"
#' 
#' @details
#' - `b`: the object returned by [broom::tidy()]
#' - `s`: the object returned by [broom::glance()]
#' 
#' @example R/examples/ex-stat_reg_gof.R
#' @importFrom broom tidy glance
#' @import glue
#' @export
stat_reg_gof <- function(mapping = NULL, data = NULL,
    formula = y ~ x, digits = 2, unit = "",
    format = 'slope = {str_num(b[2, "estimate"], digits)}{unit}, pvalue = {str_num(b[2, "p.value"], digits)} \n R^2 = {str_num(s$r.squared, digits)}',
    x = 0, y = 1, hjust = 0, vjust = 1, mar = 0.02, height.factor = 1.2,
    color = NULL,
    position = "dodge",
    ...)
{
    if (unit != "") unit %<>% paste0(" ", .)

    fun <- function(data, coords) {
        l = lm(formula, data)
        b = broom::tidy(l)
        s = broom::glance(l)

        # dots = list(...)
        # color = if (is.null(dots$color)) data$colour[1] else dots$color
        if (is.null(color)) color = data$colour[1]

        label = glue(format)
        # lines = strsplit(label, "<br>")[[1]]
        # height = sapply(lines, text_height_npc) %>% sum()
        group = data$group[1]
        g = richtextGrob(label, x, y, hjust, vjust, mar, color = color, ...)
        height = grobHeight(g) * height.factor

        if (position == "dodge") y = unit(margin_adj(y, mar), "npc") - (group - 1) * height # unit 
        richtextGrob(label, x, y, hjust, vjust, mar, color = color, ...)
    }
    grid_group(fun, mapping, data)
}

# #' @importFrom grid convertWidth stringWidth convertHeight stringHeight grobHeight
# #' @export
# text_height_npc <- function(txt, to_width = FALSE) {
#     if (to_width) {
#         convertWidth(convertHeight(stringHeight(txt), "inch"), "npc", TRUE)
#     } else {
#         convertHeight(stringHeight(txt), "npc", TRUE)
#     }
# }
