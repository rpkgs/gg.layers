% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_signHatch.R
\name{st_df2hatch}
\alias{st_df2hatch}
\title{st_df2hatch}
\usage{
st_df2hatch(data, density = 1, angle = 45, hatch = TRUE)
}
\arguments{
\item{data}{A data.frame, with the columns of "x", "y", "mask"(optional), and
"PANEL"(optional), "group"(optional).}

\item{density}{the density of shading lines, in lines per inch. The default
value of NULL means that no shading lines are drawn. A zero value of density
means no shading nor filling whereas negative values and NA suppress shading
(and so allow color filling).}

\item{angle}{the slope of shading lines, given as an angle
     in degrees (counter-clockwise).}

\item{hatch}{boolean. If \code{FALSE}, a polygon will be returned, other than lines.
Meanwhile, the parameters \code{density} and \code{angle} will be ignored.}
}
\value{
A multi-line sf object
}
\description{
st_df2hatch
}
\keyword{internal}
