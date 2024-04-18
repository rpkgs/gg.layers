#' ggplot_multiaxis
#'
#' @param ... ggplot2 objects, which should have axis.tick.y.right and axis.title.y.left
#' @param linewidth line width of right axis line
#' @param tck tick length of right axis
#' 
#' @example R/examples/ex-multiple_axis.R
#' @importFrom grid addGrob
#' @export
ggplot_multiaxis <- function(..., linewidth = 1.4, tck = 0.2, x = -0.02) {
  grobs = list(...)
  p <- Reduce(\(p1, p2) .ggplot_multiaxis(p1, p2, linewidth = linewidth, tck = tck, x = x), grobs)
  p
}

#' @rdname ggplot_multiaxis
#' @export
ggplot_multiAxis <- ggplot_multiaxis

#' @export
.ggplot_multiaxis <- function(p1, p2,
  linewidth = 1.4, tck = 0.2,
  x = -0.02, show = FALSE) {

  g1 <- p1
  g2 <- p2
  if (!("gtable" %in% class(p1)))  g1 <- ggplotGrob(p1)
  if (!("gtable" %in% class(p2)))  {
    p2 <- p2 + theme(
      panel.background = element_rect(fill = "transparent"),
      panel.grid = element_blank()
    ) +  scale_y_continuous(position = "right")
    g2 <- ggplotGrob(p2)
  }

  g1_grob_names <- sapply(g1$grob, \(x) x$name)
  g2_grob_names <- sapply(g2$grob, \(x) x$name)

  I_panel1 <- g1$grobs %>%  {grep("panel", g1_grob_names)}
  panel2   <- g2$grobs %>%  {.[[grep("panel", g2_grob_names)]]}
  g1$grobs[[I_panel1]] %<>% addGrob(panel2)

  ## 2. find ylab-r position
  I_yr1  <- g1$layout %$% {r[grep("ylab-r", name)]} %>% unique()
  I_yr2  <- g2$layout %$% {r[grep("axis-r|ylab-r", name)]} %>% unique() # get `axis-r` and `ylab-r`

  g = g2[, I_yr2]
  col = g$grobs[[4]]$children[[1]]$gp$col
  gp = gpar(col = col, lwd = linewidth)

  axis_y        = segmentsGrob(x, 0, x    , 1, gp = gp)
  axis_y_top    = segmentsGrob(x, 1, x+tck, 1, gp = gp)
  axis_y_bottom = segmentsGrob(x, 0, x+tck, 0, gp = gp)

  axis_line = grobTree(axis_y,axis_y_top,axis_y_bottom)
  # axis_line = list(axis_y, axis_y_top, axis_y_bottom)

  t = subset(g$layout, grepl("axis", name))$t # find axis
  axis_y2 = gtable_add_grob(g, axis_line, t = t, l = 1, clip="off") #%>% grid.draw()

  all <- gtable:::cbind.gtable(
    g1[, seq(max(I_yr1))],
    axis_y2,
    # rect,
    # g1[, seq(max(I_yr1)+1, ncol(g1))],
    size = "first")

  if (show){
    grid.newpage()
    grid.draw(all)
  }
  all
}

# label_left <- function(len = 4, digit = 1) {
#   fmt1 = sprintf("%%%ds", len)
#   fmt2 = sprintf("%%.%df", digit)
#   function(x) {
#     sprintf(fmt1, sprintf(fmt2, x))
#   }
# }

# label_right <- function(len = 4, digit = 1) {
#   fmt1 = sprintf("%%-%ds", len)
#   fmt2 = sprintf("%%.%df", digit)
#   function(x) { sprintf(fmt1, sprintf(fmt2, x)) }
# }

# label_tag2 <- function(i) {
#   sprintf("(%s)", letters[i])
# }

# add_label <- function(p, label) {
#   p + geom_text(data = data.table(x = -Inf, y = Inf), aes(x, y),
#     label = label, hjust = -0.2, vjust = 2, size = 5)
# }
