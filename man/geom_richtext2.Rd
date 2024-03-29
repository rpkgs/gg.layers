% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_richtext2.R
\name{geom_richtext2}
\alias{geom_richtext2}
\title{Richtext labels}
\usage{
geom_richtext2(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  nudge_x = 0,
  nudge_y = 0,
  label.padding = unit(c(0.25, 0.25, 0.25, 0.25), "lines"),
  label.margin = unit(c(0, 0, 0, 0), "lines"),
  label.r = unit(0.15, "lines"),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}} or
\code{\link[ggplot2:aes_]{aes_()}}. If specified and \code{inherit.aes = TRUE} (the
default), it is combined with the default mapping at the top level of the
plot. You must supply \code{mapping} if there is no plot mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{stat}{The statistical transformation to use on the data for this
layer, as a string.}

\item{position}{Position adjustment, either as a string, or the result of
a call to a position adjustment function. Cannot be jointy specified with
\code{nudge_x} or \code{nudge_y}.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}

\item{nudge_x}{Horizontal and vertical adjustment to nudge labels by.
Useful for offsetting text from points, particularly on discrete scales.
Cannot be jointly specified with \code{position}.}

\item{nudge_y}{Horizontal and vertical adjustment to nudge labels by.
Useful for offsetting text from points, particularly on discrete scales.
Cannot be jointly specified with \code{position}.}

\item{label.padding}{Amount of padding around label. Defaults to 0.25 lines.}

\item{label.margin}{Unit vector of length four specifying the margin
outside the text label.}

\item{label.r}{Radius of rounded corners. Defaults to 0.15 lines.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}
}
\value{
A ggplot2 layer that can be added to a plot created with
\code{\link[ggplot2:ggplot]{ggplot2::ggplot()}}.
}
\description{
This geom draws text labels similar to \code{\link[ggplot2:geom_text]{ggplot2::geom_label()}}, but formatted
using basic markdown/html. Parameter and aesthetic names follow the conventions
of \code{\link[ggplot2:geom_text]{ggplot2::geom_label()}}, and therefore the appearance of the frame around
the label is controlled with \code{label.colour}, \code{label.padding}, \code{label.margin},
\code{label.size}, \code{label.r}, even though the same parameters are called \code{box.colour},
\code{box.padding}, \code{box.margin}, \code{box.size}, and \code{box.r} in \code{\link[=geom_textbox]{geom_textbox()}}. Most
styling parameters can be used as aesthetics and can be applied separately to
each text label drawn. The exception is styling parameters that are specified
as grid units (e.g., \code{label.padding} or \code{label.r}), which can only be specified
for all text labels at once. See examples for details.
}
\section{Aesthetics}{



\code{geom_richtext()} understands the following aesthetics (required
aesthetics are in bold; select aesthetics are annotated):
\itemize{
\item \strong{\code{x}}
\item \strong{\code{y}}
\item \strong{\code{label}}
\item \code{alpha}
\item \code{angle}
\item \code{colour} Default color of label text and label outline.
\item \code{family}
\item \code{fontface}
\item \code{fill} Default fill color of label background.
\item \code{group}
\item \code{hjust}
\item \code{label.colour} Color of label outline. Overrides \code{colour}.
\item \code{label.size} Width of label outline.
\item \code{lineheight}
\item \code{size} Default font size of label text.
\item \code{text.colour} Color of label text. Overrides \code{colour}.
\item \code{vjust}
}

}

\examples{
library(ggplot2)

labels1 = c(
  "Some text **in bold.**",
  "Linebreaks<br>Linebreaks<br>Linebreaks",
  "*x*<sup>2</sup> + 5*x* + *C*<sub>*i*</sub>",
  "Some <span style='color:blue'>blue text **in bold.**</span><br>And *italics text.*<br>
    And some <span style='font-size:18pt; color:black'>large</span> text."
)

labels2 <- c(
  "gC m^{-2} d^{-1}",
  "gC m^-2 d^-1",
  "gC m_{-2} d_{-1}",
  "gC m_-2 d_-1"
  # "gC \n mm/d"
)

df <- data.frame(
  x = c(.2, .1, .5, .9),
  y = c(.8, .4, .1, .5),
  hjust = c(0.5, 0, 0, 1),
  vjust = c(0.5, 1, 0, 0.5),
  angle = c(0, 0, 45, -45),
  color = c("black", "blue", "black", "red"),
  fill = c("cornsilk", "white", "lightblue1", "white")
)

fun <- function(labels) {
  df$label = labels
  ggplot(df, aes(x, y, label = label, angle = angle, color = color,
                 hjust = hjust, vjust = vjust)) +
    geom_richtext2(
      fill = NA, label.color = NA, # remove background and outline
      label.padding = grid::unit(rep(0, 4), "pt") # remove padding
    ) +
    geom_point(color = "black", size = 2) +
    scale_color_identity() +
    xlim(0, 1) +
    ylim(0, 1)
}
fun(labels1)
fun(labels2)
# labels without frame or background are also possible
}
\seealso{
\code{\link[ggtext:geom_textbox]{ggtext::geom_textbox()}}, \code{\link[ggtext:element_markdown]{ggtext::element_markdown()}}
}
