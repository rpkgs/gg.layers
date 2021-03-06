% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layer_gof.R, R/layer_reg.R
\name{stat_gof}
\alias{stat_gof}
\alias{stat_reg}
\title{\code{stat} of regression coefficient and good-of-fitting information}
\usage{
stat_gof(
  mapping = NULL,
  data = NULL,
 
    format = "NSE = {str_num(NSE,2)}, R^2 = {str_num(R2, 2)} \\n RMSE = {str_num(RMSE,2)}",
  x = 0,
  y = 1,
  hjust = 0,
  vjust = 1,
  mar = 0.02,
  show.bias = TRUE,
  show.line = FALSE
)

stat_reg(
  mapping = NULL,
  data = NULL,
  formula = y ~ x,
  digits = 2,
  units = "",
  format = paste0("Slope = {str_num(slope, digits)}{unit}",
    ", p-value = {str_num(pvalue, digits)}"),
  slope_FUN = NULL,
  x = 0,
  y = 1,
  hjust = 0,
  vjust = 1,
  mar = 0.03,
  height.factor = 1.2,
  family = "Times",
  color = NULL,
  position = "dodge",
  ...
)
}
\arguments{
\item{data}{an optional data frame, list or environment (or object
    coercible by \code{\link{as.data.frame}} to a data frame) containing
    the variables in the model.  If not found in \code{data}, the
    variables are taken from \code{environment(formula)},
    typically the environment from which \code{lm} is called.}

\item{x}{Unit objects specifying the location of the reference point.}

\item{y}{Unit objects specifying the location of the reference point.}

\item{hjust}{Numerical values specifying the justification
of the text boxes relative to \code{x} and \code{y}. These justification parameters
are specified in the internal reference frame of the text boxes, so that,
for example, \code{hjust} adjusts the vertical justification when the
text is rotated 90 degrees to the left or right.}

\item{vjust}{Numerical values specifying the justification
of the text boxes relative to \code{x} and \code{y}. These justification parameters
are specified in the internal reference frame of the text boxes, so that,
for example, \code{hjust} adjusts the vertical justification when the
text is rotated 90 degrees to the left or right.}

\item{formula}{an object of class \code{"\link[stats]{formula}"} (or one that
    can be coerced to that class): a symbolic description of the
    model to be fitted.  The details of model specification are given
    under \sQuote{Details}.}

\item{digits}{the number of \emph{significant} digits to be
    passed to \code{\link{format}(\link[stats]{coef}(x), .)} when
    \code{\link{print}()}ing.}

\item{position}{"dodge" or "identity"}

\item{...}{other parameters to \code{\link[gggrid:grid_panel]{gggrid::grid_panel()}}}
}
\description{
\code{stat} of regression coefficient and good-of-fitting information
}
\details{
\itemize{
\item \code{b}: the object returned by \code{\link[broom:reexports]{broom::tidy()}}
\item \code{s}: the object returned by \code{\link[broom:reexports]{broom::glance()}}
\itemize{
\item "\emph{R}^2 = {str_num(s$r.squared, digits)}"
}
\item \code{slope}:
\item \code{pvalue}:
\item \code{pcode}: significant code, e.g., \verb{**}, \code{*}, \code{-}
}
}
\examples{
library(ggplot2)
library(data.table)

dat <- data.table(mtcars)
dat$cyl <- as.factor(dat$cyl)
table(dat$cyl)

ggplot(dat, aes(wt, mpg, color = cyl)) +
    geom_point() +
    stat_reg(data = dat[cyl != 4], y = 1, mar = 0, position = "none") +
    facet_wrap(~cyl)

ggplot(dat, aes(wt, mpg, color = cyl)) +
  geom_point() +
  stat_reg(data = dat[cyl != 4], y = 1,
               position = "dodge",
               height.factor = 1.2,
               unit = "gC m^-2 d^-1")

ggplot(dat, aes(wt, mpg, color = cyl)) +
  geom_point() +
  stat_reg(data = dat[cyl != 4], y = 1,
               position = "none",
               height.factor = 1.2,
               unit = "gC m^-2 d^-1") +
  facet_wrap(~cyl)


ggplot(dat, aes(wt, mpg, color = cyl)) +
    geom_point() +
    stat_gof(x = 0, y = 1) +
    # stat_reg(data = subset(dat, cyl == 4), y = 1, color = "red") +
    # stat_reg(data = subset(dat, cyl == 6), y = 0.8) +
    facet_wrap(~cyl)
}
