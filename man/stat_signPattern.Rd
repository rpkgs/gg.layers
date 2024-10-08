% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_signPattern.R
\name{stat_signPattern}
\alias{stat_signPattern}
\title{stat_signPattern}
\usage{
stat_signPattern(
  mapping = NULL,
  data = NULL,
  geom = "sf_pattern",
  position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...
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

\item{geom}{Use to override the default connection between \code{\link[=geom_signPattern]{geom_signPattern()}}
and \link{stat_signPattern}.}

\item{position}{A position adjustment to use on the data for this layer. This
can be used in various ways, including to prevent overplotting and
improving the display. The \code{position} argument accepts the following:
\itemize{
\item The result of calling a position function, such as \code{position_jitter()}.
This method allows for passing extra arguments to the position.
\item A string naming the position adjustment. To give the position as a
string, strip the function name of the \code{position_} prefix. For example,
to use \code{position_jitter()}, give the position as \code{"jitter"}.
\item For more information and other ways to specify the position, see the
\link[ggplot2:layer_positions]{layer position} documentation.
}}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}

\item{...}{other parameters to \code{\link[ggpattern:geom-docs]{ggpattern::geom_sf_pattern()}}}
}
\description{
stat_signPattern
}
\examples{
library(gg.layers)
library(ggplot2)
data("d_trendPerc")

d_mask <- mutate(d_trendPerc, mask = perc <= 0.99) # \%>\% as.data.frame()

# significant part; geom_sf_pattern still has bug unsolved.
ggplot() +
  geom_raster(data = d_trendPerc, aes(x, y, fill = perc)) +
  stat_signPattern(data = d_mask, aes(x, y, mask = mask),
                   fill = "transparent", color = "red",
                   pattern_density = 0.02)

# insignificant
ggplot() +
    geom_raster(data = d_trendPerc, aes(x, y, fill = perc)) +
    stat_signPattern(data = d_mask, aes(x, y, mask = !mask),
                     fill = "transparent", color = "red",
                     pattern_density = 0.02) #-> p
# Ipaper::write_fig(p, "temp.pdf")
}
\seealso{
\code{\link[ggpattern:geom-docs]{ggpattern::geom_sf_pattern()}}
}
