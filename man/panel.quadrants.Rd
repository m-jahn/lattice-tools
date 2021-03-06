% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/panel_quadrants.R
\name{panel.quadrants}
\alias{panel.quadrants}
\title{Draw quadrants and quadrant statistics in lattice plots}
\usage{
panel.quadrants(
  x,
  y,
  h = NULL,
  v = NULL,
  labels = "percent",
  col = grey(0.5),
  margin = 0.1,
  lwd = trellis.par.get()$superpose.polygon$lwd[1],
  na.rm = FALSE,
  ...
)
}
\arguments{
\item{x, y}{(numeric) variables to be plotted}

\item{h, v}{(numeric) position of the horizontal or vertical threshold for dividing the
data into quadarants. Defaults to the median.}

\item{labels}{(character) One of 'percent', 'events', or 'none'. Controls what
type of summary is plottted per quadrant}

\item{col, lwd}{graphical parameters for lines and labels}

\item{margin}{(numeric) margin of labels to the plot edges in Normalised Parent 
Coordinates, default is 0.1}

\item{na.rm}{(logical) Should NA or Inf values be removed automatically? Default is FAlSE}

\item{...}{other arguments passed to the function}
}
\description{
This panel function allows to draw custom quadrants and display additional
quadrant statistics as often used in biomedical sciences. Grouping is ignored.
}
\examples{
library(lattice)
data(mtcars)

# Default behavior for quadrants is to split x and y data
# at the respective median, and plot percentage of points 
# per quandrant
xyplot(mpg ~ 1/wt | factor(vs), mtcars,
  groups = carb, pch = 19, cex = 1,
  panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.quadrants(x, y, ...)
  }
)

}
