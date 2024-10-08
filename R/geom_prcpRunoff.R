check_ggplot2_varnames <- function(l) {
  names <- names(l) %>% stringr::str_replace("color", "colour")
  set_names(l, names)
}

#' @export
guess_prcp_coef <- function(y, prcp, ratio = 0.5) {
  qmax <- max(y, na.rm = TRUE)
  pmax <- max(prcp, na.rm = TRUE)
  qmax / pmax * ratio # coef
}

#' @export
theme_dual_axis <- function(col_left = "darkorange", col_right = "blue") {
  theme(
    axis.title.y.left = element_text(color = col_left),
    axis.text.y.left = element_text(color = col_left),
    axis.ticks.y.left = element_line(color = col_left),
    # right
    axis.title.y.right = element_text(color = col_right),
    axis.text.y.right = element_text(color = col_right),
    axis.ticks.y.right = element_line(color = col_right)
  )
}

#' @importFrom rlang %||%
#' @importFrom magrittr %<>% %>%
#' @importFrom ggplot2 ggproto GeomLine aes layer
GeomPrcpRunoff <- ggproto(
  "GeomPrcpRunoff", GeomLine,
  default_aes = aes(
    colour = "black",
    fill = "black",
    linewidth = 0.1,
    linetype = 1,
    alpha = NA,
    width = NA
  ),
  required_aes = c("x", "y", "prcp"),
  dropped_aes = c("prcp"),
  setup_params = function(data, params) {
    # print2("setup_params", params)
    # qmax <- max(data$y, na.rm = T)
    # params$qmax <- params$qmax %||% qmax
    if (length(params$params_prcp) > 0) {
      params$params_prcp %<>% check_ggplot2_varnames()
    }
    # params$prcp.ratio <- params$prcp.ratio %||% 0.5
    # prcp_max <- max(data$prcp, na.rm = T)
    # qmax <- params$qmax
    # prcp.coef <- qmax / prcp_max * params$prcp.ratio # A_prcp = prcp.coef * prcp
    # params$prcp.coef <- params$prcp.coef %||% prcp.coef
    ## 把参数放到数据中
    params
  },
  setup_data = function(data, params) {
    # print2("setup_data", params)
    data$width <- data$width %||% params$width %||% resolution(data$x, FALSE)
    # qmax <- max(data$y, na.rm = T)
    data
  },

  # unused parameters also should be listed here
  draw_panel = function(data, panel_params, coord,
                        params_prcp = list(),
                        prcp.ratio, qmax, prcp.coef) {
    trans <- \(x) qmax - (x * prcp.coef)
    trans_inv <- \(x) (qmax - x) / prcp.coef

    data <- transform(data,
      xmin = x - width / 2, xmax = x + width / 2, width = NULL, prcp = NULL,
      ymin = trans(prcp), ymax = qmax
    )

    # panel_params$y.sec$scale$secondary.axis$trans = env_trans$trans
    default_params_prcp <- list(
      fill = "blue", colour = "white",
      linetype = "solid", linewidth = 0.1
    )

    params_prcp <- modifyList(default_params_prcp, params_prcp)
    df_prcp <- modifyList(data, params_prcp)

    grid::gList(
      ggplot2::GeomLine$draw_panel(data, panel_params, coord),
      ggplot2::GeomTile$draw_panel(df_prcp, panel_params, coord)
    )
  }
)


# @param prcp.ratio the ratio of precipitation to `ymax`

#' Draw precipitation bar on the top of the panel
#'
#' @inheritParams ggplot2::geom_tile
#'
#' @param params_prcp parameters for precipitation hist, default `list(fill =
#' "blue", colour = "white", linetype = "solid", linewidth = 0.1)`. See
#' [ggplot2::geom_tile()] for all supported parameters.
#'
#' @param prcp.ratio the ratio of precipitation to `ymax` (not used currently)
#' @param prcp.coef coefficient of precipitation, `y_new = qmax - prcp * prcp.coef`
#' prcp.coef = qmax / max(prcp)
#'
#' @param params_prcp
#' - `color`: color of precipitation
#' - `fill`: fill of precipitation
#' @param qmax maximum of streamflow, used to calculate `prcp.coef`
#'
#' @param sec.axis secondary axis for precipitation, returned by
#' [ggplot2::sec_axis()]
#' @param sec.name name of secondary axis
#'
#' @importFrom ggplot2 layer
#' @importFrom rlang list2
#'
#' @section Aesthetics:
#' - `x`: date or continuous variable
#' - `y`: runoff
#' - `prcp`: precipitation
#'
#' @example R/examples/ex-geom_prcpRunoff.R
#'
#' @author Xie YuXuan and Dongdong Kong
#' @export
geom_prcpRunoff <- function(
    mapping = NULL, data = NULL, stat = "identity",
    position = "identity", ..., linejoin = "mitre",
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
    # prcp.color = "black", prcp.fill = "black",
    prcp.ratio = 0.5,
    params_prcp = list(),
    prcp.coef = 1,
    qmax = NULL,
    sec.axis = NULL,
    sec.name = "Precipitation (mm)") {
  layer <- layer(
    geom = GeomPrcpRunoff,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      na.rm = na.rm,
      params_prcp = params_prcp,
      # prcp.ratio = prcp.ratio,
      qmax = qmax,
      prcp.coef = prcp.coef,
      # env_trans = env_trans,
      ...
    )
  )
  ## set of dual axis
  ## add a spy variable:
  # env_trans <- list2env(list(trans = ~.))
  # get_trans <- function() { env_trans$trans }
  if (is.null(sec.axis)) {
    if (!is.null(qmax)) {
      trans_inv <- ~ (qmax - .) / prcp.coef
      ylim <- c(0, qmax)
    } else {
      trans_inv <- ~ (max(.) - .) / prcp.coef
      ylim <- NULL
    }
    sec.axis <- ggplot2::sec_axis(name = sec.name, trans = trans_inv, labels = \(x) x)
  }

  scale_y <- scale_y_continuous(sec.axis = sec.axis, expand = c(0, 0))
  coord <- coord_cartesian(ylim = ylim, clip = "on")
  c(layer, scale_y, coord)
}
