lattice-tools
================
Michael Jahn,
2020-11-20

<!-- badges start -->

[![R build
status](https://github.com/m-jahn/lattice-tools/workflows/R-CMD-check/badge.svg)](https://github.com/m-jahn/lattice-tools/actions)
[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://github.com/m-jahn)
![GitHub
issues](https://img.shields.io/github/issues/m-jahn/lattice-tools)
![GitHub last
commit](https://img.shields.io/github/last-commit/m-jahn/lattice-tools)
![Platform](https://img.shields.io/badge/platform-all-green)
<!-- badges end -->

-----

Panel functions and wrappers that extend the R lattice universe

## Description

This package contains panel functions and wrapper functions for the R
`lattice` package, a general purpose plotting package from Deepayan
Sarkar. Lattice’s functionality is comparable to the popular `ggplot2`
package but has a slightly different look and feel. The functions
collected here were added over time and might not always adhere
perfectly to the `lattice` conventions. However some care was taken to
replicate the original lattice behavior so that **users can use grouping
and paneling** as they are used to. Feel free to copy, fork or source
functions that you find useful. Contributions welcome\!

## Installation

To install the package directly from github, use this function from
`devtools` package in your R session:

``` r
require(devtools)
devtools::install_github("https://github.com/m-jahn/lattice-tools")
```

## Panel functions

These functions extend or simplify the `panel.function` landscape for
`lattice`.

### panel.arrowbox

Custom lattice panel function to draw boxes with arrow head from 2 XY
coordinates and direction argument. Each argument can be a vector of
same length so that multiple arrows can be drawn at once. Useful for
`panel.geneplot` function.

``` r
library(lattice)

xyplot(1:3 ~ 4:6, col = grey(0.7),
  panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.arrowbox(x0 = c(4, 5), y0 = c(2.5, 1),
      x1 = c(5, 6), y1 = c(3, 1.5),
      direction = c(1, -1), ...)
  }
)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### panel.barplot

Draw a barplot with error bars in lattice plots. This custom panel
function for lattice plots allows to draw barplots with error bars for
arbitrary groups of data points. Error bars will be drawn for groups of
identical x values with optional subsetting by grouping or paneling
variables.

``` r
library(latticetools)
library(lattice)
data(mtcars)

# mean and stdev error bars are drawn for
# common x values
xyplot(mpg ~ factor(cyl), mtcars, lwd = 2, 
  panel = function(x, y, ...) {
    panel.barplot(x, y, ...)
  }
)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# using the same variable for x and grouping will
# result in typical lattice behavior
xyplot(mpg ~ factor(cyl), mtcars, 
  groups = cyl, lwd = 2,
  panel = function(x, y, ...) {
    panel.barplot(x, y, ...)
  }
)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
# we can also use different variables for the x var, grouping,
# and paneling. As a visual control that error bars are drawn
# for the correct groups we overlay the single data points.
xyplot(mpg ~ factor(cyl) | factor(vs), mtcars,
  groups = gear, lwd = 2, auto.key = list(columns = 3),
  panel = function(x, y, ...) {
    panel.barplot(x, y, beside = TRUE, ...)
    panel.stripplot(x, y, jitter.data = TRUE,
      horizontal = FALSE, amount = 0.15, ...)
  }
)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
# alternatively, means and error margins can be supplied directly. 
# In this case means are supplied as unique combinations
# of y and x while error_margin is a separate vector with same length as y.
mtcars_means <- data.frame(
  cyl = sort(unique(mtcars$cyl)),
  mpg = with(mtcars, tapply(mpg, cyl, mean)),
  stdev = with(mtcars, tapply(mpg, cyl, sd))
)

# you might have to adjust the yscale as it is determined from the
# range of the y variable only, ignoring the extension through error bars.
xyplot(mpg ~ factor(cyl), mtcars_means,
  error_margin = mtcars_means$stdev,
  ylim = c(9, 36), groups = cyl,
  lwd = 2, pch = 19, cex = 1.5,
  panel = function(x, y, ...) {
    panel.barplot(x, y, ...)
  }
)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->

### panel.beeswarm

Panel function for beeswarm plots. This panel function works essentially
like a stripplot, but instead of randomly scattering a variable produces
a regular, grid-like pattern of points. Currently only the X variable is
transformed. However, X and Y variables can be discretized optionally.

``` r
# simple example
df <- data.frame(
  Y = sample(1:10, 60, replace = TRUE), 
  X = factor(rep(1:3, each = 20))
)

xyplot(Y ~ X, df, groups = X, panel = panel.beeswarm)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
# but with continuous Y variable, it doesn't work as expected
df$Y <- rnorm(60)
xyplot(Y ~ X, df, groups = X, panel = panel.beeswarm)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
# for this purpose we can bin the Y variable into groups
xyplot(Y ~ X, df, groups = X, 
  panel = function(x, y, ...) {
    panel.xyplot(x, y, col = grey(0.6), ...)
    panel.beeswarm(x, y, bin_y = TRUE, breaks_y = 10, ...)
})
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

### panel.directlabel

Point labels for scatterplots. Draw text labels for all points of a
scatterplot using functions from directlabels. In contrast to the
functionality of the original `directlabels` package, *every point* is
labeled instead of groups. Labels are also independent from the grouping
variable, so that e.g. colors indicate a grouping variable and labels
another. By default, labels adapt the graphical parameters of the higher
level plot, including coloring according to groups. However, many
parameters can be customized.

``` r
library(grid)
library(lattice)
library(directlabels)

data("mtcars")
mtcars$car <- rownames(mtcars)

# A standard example using lattice grouping and paneling;
# We can also draw boxes around labels and change label size
xyplot(mpg ~ wt | factor(cyl), mtcars,
  groups = cyl, pch = 19, labels = mtcars$car,
  as.table = TRUE, layout = c(3, 1), cex = 0.6,
  panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.directlabel(x, y, draw_box = TRUE, box_line = TRUE, ...)
  }
)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# A similar plot but without grouping. This requires explicit
# use of subscripts
xyplot(mpg ~ wt | factor(cyl), mtcars,
  pch = 19, labels = mtcars$car,
  as.table = TRUE, layout = c(3, 1),
  panel = function(x, y, subscripts, ...) {
    panel.xyplot(x, y, ...)
    panel.directlabel(x, y, subscripts = subscripts, 
      draw_box = TRUE, box_fill = "white", ...)
  }
)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
# An example without panels and more groups
xyplot(mpg ~ wt, mtcars,
  groups = hp, pch = 19, 
  labels = mtcars$wt, cex = 0.6,
  panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.directlabel(x, y, draw_box = TRUE, box_line = TRUE, ...)
  }
)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

### panel.errbars

Draw error bars in lattice plots. This custom panel function for lattice
plots allows to draw symbols with error bars for arbitrary groups of
data points. Error bars will be drawn for groups of identical x values
with optional subsetting by grouping or paneling variables. This
function is very similar to `panel.barplot` only with points instead of
bars.

``` r
library(lattice)
data(mtcars)

# mean and stdev error bars are drawn for
# common x values
xyplot(mpg ~ factor(cyl), mtcars, 
  lwd = 2, pch = 19, cex = 1.5,
  panel = function(x, y, ...) {
    panel.errbars(x, y, ...)
  }
)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# using the same variable for x and grouping will
# result in typical lattice behavior
xyplot(mpg ~ factor(cyl), mtcars,
  groups = cyl, lwd = 2, pch = 19, cex = 1.5,
  panel = function(x, y, ...) {
    panel.errbars(x, y, ...)
  }
)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
# we can also use different variables for the x var, grouping,
# and paneling. As a visual control that error bars are drawn 
# for the correct groups we overlay the single data points. 
xyplot(mpg ~ factor(cyl) | factor(vs), mtcars,
  groups = gear, lwd = 2, pch = 19, cex = 1.5,
  auto.key = list(columns = 3),
  panel = function(x, y, ...) {
    panel.stripplot(x, y, jitter.data = TRUE, 
      horizontal = FALSE, amount = 0.15, alpha = 0.3, ...)
    panel.errbars(x, y, beside = TRUE, ...)
  }
)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

``` r
# alternatively, means and error margins can be supplied directly. 
# In this case means are supplied as unique combinations
# of y and x while error_margin is a separate vector with same length as y.
mtcars_means <- data.frame(
  cyl = sort(unique(mtcars$cyl)),
  mpg = with(mtcars, tapply(mpg, cyl, mean)),
  stdev = with(mtcars, tapply(mpg, cyl, sd))
)

# you might have to adjust the yscale as it is determined from the
# range of the y variable only, ignoring the extension through error bars.
xyplot(mpg ~ factor(cyl), mtcars_means,
  error_margin = mtcars_means$stdev,
  ylim = c(9, 36), groups = cyl,
  lwd = 2, pch = 19, cex = 1.5,
  panel = function(x, y, ...) {
    panel.errbars(x, y, ...)
  }
)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->

### panel.geneplot

Plot genes along a linear axis. Custom lattice panel function to draw
genes with start and end coordinates as main input. Optional vectors for
`gene_name` or `gene_strand` must have same length as `x`, `y`. Panel
function supports paneling and grouping (e.g. by gene functional
category) just as regular panel functions.

``` r
library(lattice)

# table with dummdy genetic loci
genes <- data.frame(
  gene_name = c("abc", "def", "ghi", "jkl"),
  gene_strand = c("+", "+", "+", "-"),
  gene_start = c(123, 178, 245, 310),
  gene_end = c(167, 233, 297, 354)
)
 
# plot genes on a linear map
xyplot(gene_end ~ gene_start, genes,
  groups = gene_strand,
  scales = list(y = list(draw = FALSE)),
  xlim = c(80, 380), ylim = c(-3,2),
  xlab = "", ylab = "",
  gene_strand = genes[["gene_strand"]],
  gene_name = genes[["gene_name"]],
  panel = function(x, y, ...) {
    panel.grid(h = -1, v = -1, col = grey(0.9))
    panel.geneplot(x, y, arrows = TRUE, ...)
  }
)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# same example with customized arrows
xyplot(gene_end ~ gene_start, genes,
  groups = gene_strand,
  scales = list(y = list(draw = FALSE)),
  xlim = c(80, 380), ylim = c(-3,2),
  xlab = "", ylab = "",
  gene_strand = genes[["gene_strand"]],
  gene_name = genes[["gene_name"]],
  panel = function(x, y, ...) {
    panel.grid(h = -1, v = -1, col = grey(0.9))
    panel.geneplot(x, y, arrows = TRUE, offset = 0.5,
      height = 0.6, rot_labels = 0, tip = 3, col_labels = 1, ...)
  }
)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

### panel.key

Draw custom keys in lattice plots. This custom panel function for
lattice plots allows to draw a key (legend) inside a lattice panel, with
more customization options than the lattice default.

``` r
library(lattice)
data(mtcars)

# Two examples for a custom lattice key
# inside a panel. The first with taking all arguments from the 
# top-level plotting function, the second with custom arguments.
xyplot(mpg ~ 1/wt | factor(vs), mtcars,
  groups = carb, pch = 19, cex = 1,
  panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.key(...)
    panel.key(labels = letters[1:5], which.panel = 2, 
      corner = c(0.9, 0.1), col = 1:5, pch = 3, cex = 1)
  }
)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### panel.piechart

Draw pie and ring charts in lattice plots. This custom panel function
for lattice plots allows to draw pie charts (or rings) while still being
able to use the typical lattice way of subsetting data. The function can
be used within xyplot() but only one variable needs to be supplied (x).
Grouping is supported in which the x variable is aggregated (summed up)
over each unique group.

``` r
library(grid)
library(lattice)

data("USMortality")

# A simple example using lattice paneling
xyplot( ~ Rate | Sex, USMortality,
  main = "US mortality rates by sex",
  scales = list(draw = FALSE),
  panel = function(x, ...) {
    panel.piechart(x, ...)
  }
)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
# A more advanced example using grouping and
# adjusting graphical parameters. The main variable 
# 'x' is now summed up for each value of 'groups'
xyplot( ~ Rate | Sex, USMortality,
  groups = gsub(" ", "\n", Cause), 
  col = heat.colors(10),
  border = grey(0.3), cex = 0.7,
  main = "US mortality rates by sex",
  scales = list(draw = FALSE),
  panel = function(x, ...) {
    panel.piechart(x, diameter_sector = 0.1, ...)
  }
)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

### panel.pvalue

Calculate and draw p-values in lattice plots.

``` r
library(lattice)
data(mtcars)

# p-values are calculated between groups of x, grouping variable is ignored
xyplot(mpg ~ factor(cyl), mtcars, groups = cyl, pch = 19, cex = 0.7,
  panel = function(x, y, ...) {
    panel.stripplot(x, y, jitter.data = TRUE, 
      horizontal = FALSE, amount = 0.15, ...)
    panel.pvalue(x, y, std = 1, symbol = TRUE, pvalue = TRUE)
})
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### panel.quadrants

Draw quadrants and quadrant statistics in lattice plots. This custom
panel function for lattice plots allows to draw custom quadrants and
display additional quadrant statistics as often used in biomedial
sciences. Groups are ignored.

``` r
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
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

### panel.symbols

Plot a grouping variable encoded by symbols. Custom lattice panel
function to plot one more grouping variable encoded by symbols (‘pch’),
instead of just the default encoded by color. Inspired by
`panel.bubbleplot` from package `tactile`.

``` r
library(lattice)
data(mtcars)

# first grouping variable for colors, second for symbols (z)
xyplot(mpg ~ factor(cyl), mtcars,
  groups = gear, z = mtcars$cyl,
  panel = function(x, y, z, ...) {
    panel.symbols(x, y, z, ...)
  }
)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### custom.ggplot

Custom theme for lattice plots. The function takes no arguments.

``` r
library(lattice)
data(mtcars)

# colors are more subtle than default lattice theme
xyplot(mpg ~ factor(cyl) | gear, mtcars,
  groups = cyl, auto.key = list(columns = 3),
  par.settings = custom.ggplot()
)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

### custom.lattice

Custom theme for lattice plots. The function takes no arguments.

``` r
library(lattice)
data(mtcars)

# colors are more subtle than default lattice theme
xyplot(mpg ~ factor(cyl) | gear, mtcars,
  groups = cyl, auto.key = list(columns = 3),
  par.settings = custom.lattice()
)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

### custom.colorblind

Colorblind-safe grey lattice theme. The function takes no arguments.
Colorblind-safe color scale with 7 colors was adapted from R color
brewer, see `RColorBrewer::brewer.pal(8, "Dark2"))`. Color blind safe
colors are distinguishable for most common types of color blindness
(deuterotopia, deuteroanomaly), yet still look good for non color blind
people.

``` r
library(lattice)
data(mtcars)

xyplot(mpg ~ factor(carb) | gear, mtcars,
 groups = carb, auto.key = list(columns = 3),
 par.settings = custom.colorblind()
)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

### custom\_splom

Custom scatterplot matrix (SPLOM). Custom wrapper function around
lattice ‘splom’ with different upper and lower panel. A scatterplot
matrix is a tiled plot where all variables of a data frame are plotted
against each other.

``` r
library(lattice)
data(mtcars)

# Draw a scatterplot matrix of all variables of a 
# data frame against each other.
custom_splom(mtcars[1:5])
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
# We can customize the scatterplot
custom_splom(
  mtcars[1:5],
  col_palette = rainbow(4),
  scales = 10, 
  xlab = "data points", ylab = "regression",
  pch = 1, col = 1, cex = 0.6
)
```

![](/home/michael/Documents/SciLifeLab/Resources/R_projects/lattice-tools/vignettes/README_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->
