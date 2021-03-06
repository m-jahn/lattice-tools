% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/custom_theme.R
\name{custom.colorblind}
\alias{custom.colorblind}
\title{Colorblind-safe grey lattice theme}
\usage{
custom.colorblind(
  col = c("#E7298A", "#66A61E", "#E6AB02", "#7570B3", "#666666", "#1B9E77", "#D95F02",
    "#A6761D"),
  symbol_type = 19,
  symbol_size = 1,
  line_type = 1,
  line_size = 1,
  text_type = 1,
  text_size = 1
)
}
\arguments{
\item{col}{default colors for the theme}

\item{symbol_type}{symbol(s) for the theme ('pch', defaults to 19)}

\item{symbol_size}{relative expansion factor (default 1)}

\item{line_type}{line type(s) for the theme ('lty', defaults to 1)}

\item{line_size}{relative expansion factor (default 1)}

\item{text_type}{the font face, a number form 1 to 4 for normal, bold, italic, bold + italic}

\item{text_size}{relative expansion factor (default 1)}
}
\description{
Custom theme for lattice plots. 
Colorblind-safe color scale with 7 colors was adapted from R color brewer, 
see \code{RColorBrewer::brewer.pal(8, "Dark2"))}. Color blind safe colors are
distinguishable for most common types of color blindness (deuterotopia, 
deuteroanomaly), yet still look good for non color blind people.
Some graphical parameters can be passed to override the defaults. 
Size parameters are relative.
}
\examples{
library(lattice)
data(mtcars)

xyplot(mpg ~ factor(carb) | gear, mtcars,
  groups = carb, auto.key = list(columns = 3),
  par.settings = custom.colorblind(),
  panel = function(x, y, ...) {
    panel.grid(h = -1, v = -1, col = grey(0.95))
    panel.xyplot(x, y, ...)
    panel.lmline(x, y, ...)
  }
)

}
