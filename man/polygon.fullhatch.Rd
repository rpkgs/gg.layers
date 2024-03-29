% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/polygon.fullhatch.R
\name{polygon.fullhatch}
\alias{polygon.fullhatch}
\alias{polygon.onehatch}
\title{Create hatch area of one polygon}
\usage{
polygon.fullhatch(
  x,
  y = NULL,
  density,
  angle,
  ..debug.hatch = FALSE,
  fillOddEven = FALSE,
  ...
)

polygon.onehatch(
  x,
  y,
  x0,
  y0,
  xd,
  yd,
  ..debug.hatch = FALSE,
  fillOddEven = FALSE,
  ...
)
}
\arguments{
\item{x, y}{vectors containing the coordinates of the vertices
    of the polygon.}

\item{density}{the density of shading lines, in lines per inch.  The
    default value of \code{NULL} means that no shading lines are drawn.
    A zero value of \code{density} means no shading nor filling whereas
    negative values and \code{NA} suppress shading (and so allow
    color filling).}

\item{angle}{the slope of shading lines, given as an angle
     in degrees (counter-clockwise).}

\item{..debug.hatch}{for drawing when debugging function}

\item{fillOddEven}{logical controlling the polygon shading mode:  see
      below for details.  Default \code{FALSE}.}

\item{...}{graphical parameters such as \code{xpd}, \code{lend},
    \code{ljoin} and \code{lmitre} can be given as arguments.}

\item{x0}{parameter as issued from \code{\link{polygon.fullhatch}}}

\item{y0}{parameter as issued from \code{\link{polygon.fullhatch}}}

\item{xd}{parameter as issued from \code{\link{polygon.fullhatch}}}

\item{yd}{parameter as issued from \code{\link{polygon.fullhatch}}}
}
\description{
Create hatch area of one polygon

Create one line for hatch area of one polygon
}
\examples{
\dontrun{
res <- polygon.fullhatch(x, density = 10, angle = 45)
arrows(res$lx1, res$ly1, res$lx2, res$ly2, col = "red", code = 0)
}
}
\keyword{internal}
