#' markdown superscript and subscript
#'
#' @param x character vector
#' @examples
#' x <- c(
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
#' str_num(2, 2) # 2.00
#' @export
str_num <- function(x, digits = 2) {
    fmt <- sprintf("%%.%df", digits)
    sprintf(fmt, x)
}

#' @export
labeller_num <- function(fmt = "%.2f") {
    function(x) sprintf(fmt, x)
}
