% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_prcpRunoff.R
\name{geom_prcpRunoff}
\alias{geom_prcpRunoff}
\title{Draw precipitation bar on the top of the panel}
\usage{
geom_prcpRunoff(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  linejoin = "mitre",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  prcp.color = "black",
  prcp.fill = "black",
  params_prcp = list(),
  prcp.coef = 1,
  prcp.qmax = NULL,
  sec.axis = NULL,
  sec.name = "Precipitation (mm)"
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

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
layer, either as a \code{ggproto} \code{Geom} subclass or as a string naming the
stat stripped of the \code{stat_} prefix (e.g. \code{"count"} rather than
\code{"stat_count"})}

\item{position}{Position adjustment, either as a string naming the adjustment
(e.g. \code{"jitter"} to use \code{position_jitter}), or the result of a call to a
position adjustment function. Use the latter if you need to change the
settings of the adjustment.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}

\item{linejoin}{Line join style (round, mitre, bevel).}

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

\item{prcp.color}{color of precipitation}

\item{prcp.fill}{fill of precipitation}

\item{params_prcp}{parameters for precipitation hist, default \code{list(fill = "blue", colour = "white", linetype = "solid", linewidth = 0.1)}. See
\code{\link[ggplot2:geom_tile]{ggplot2::geom_tile()}} for all supported parameters.}

\item{prcp.coef}{coefficient of precipitation, `y_new = prcp.qmax
\itemize{
\item prcp * prcp.coef`
}}

\item{prcp.qmax}{maximum of streamflow, used to calculate \code{prcp.coef}}

\item{sec.axis}{secondary axis for precipitation, returned by
\code{\link[ggplot2:sec_axis]{ggplot2::sec_axis()}}}

\item{sec.name}{name of secondary axis}
}
\description{
Draw precipitation bar on the top of the panel
}
\section{Aesthetics}{

\itemize{
\item \code{x}: date or continuous variable
\item \code{y}: runoff
\item \code{prcp}: precipitation
}
}

\examples{
library(ggplot2)

col_prcp = "blue"  #"#3e89be"
col_runoff = "black"  # "darkorange"

my_theme <-
  theme_dual_axis(col_runoff, col_prcp) +
  theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    # axis.ticks = element_blank(),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0)
  )

## Visualization ---------------------------------------------------------------
dat <- runoff_data
prcp.qmax <- max(dat$Q) * 1.1
prcp.coef <- guess_prcp_coef(prcp.qmax, dat$prcp, ratio = 0.5)
# prcp.qmax <- NULL

ggplot(dat, aes(x = time, Q)) +
  # theme_test() +
  geom_prcpRunoff(
    aes(prcp = prcp, color = flood_type),
    params_prcp = list(color = col_prcp, fill = col_prcp),
    prcp.coef = prcp.coef,
    prcp.qmax = prcp.qmax,
    color = col_runoff, linewidth = 0.5
  ) +
  facet_wrap(~flood_type, scales = "free") +
  # scale_y_precipitation(sec.name = "Precipitation (mm)", coef = set_coef) +
  scale_x_datetime(date_labels = "\%m/\%d") +
  my_theme +
  labs(x = "Date", y = expression("Runoff (m"^"3" * "/s)")) -> p
print(p)
# write_fig(p, "d:/Rplot.pdf", 10, 5)
}
\author{
Xie YuXuan and Dongdong Kong
}
