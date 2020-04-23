lattice-tools
================
Michael Jahn,
2020-04-23

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
`lattice`

### panel.barplot

Draw barplot with error bars in lattice plots. Supports also grouping.

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

![](vignettes/README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

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

![](vignettes/README_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

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

![](vignettes/README_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

### panel.beeswarm

Panel function for beeswarm plots. This function builds heavily on the
`beeswarm` package.

``` r
# simple example
df <- data.frame(
  Y = sample(1:10, 60, replace = TRUE), 
  X = factor(rep(1:3, each = 20))
)

xyplot(Y ~ X, df, groups = X, panel = panel.beeswarm)
```

![](vignettes/README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
# but with continuous Y variable, it doesn't work as expected
df$Y <- rnorm(60)
xyplot(Y ~ X, df, groups = X, panel = panel.beeswarm)
```

![](vignettes/README_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
# for this purpose we can bin the Y variable into groups
xyplot(Y ~ X, df, groups = X, 
  panel = function(x, y, ...) {
    panel.xyplot(x, y, col = grey(0.6), ...)
    panel.beeswarm(x, y, bin_y = TRUE, breaks_y = 10, ...)
})
```

![](vignettes/README_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

### panel.directlabel

Point labels for scatterplots. This function depends on the directlabels
package, but instead of labeling only groups, it labels single points.

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

![](vignettes/README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

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

![](vignettes/README_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

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

![](vignettes/README_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

### panel.errbars

Draw error bars in lattice plots. This functions is very similar to
`panel.barplot` only with points instead of bars.

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

![](vignettes/README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

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

![](vignettes/README_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

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

![](vignettes/README_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

### panel.key

Draw custom keys in lattice plots. Allows to draw a key (legend) inside
a lattice panel, with more customization options than the lattice
default.

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

![](vignettes/README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

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

![](vignettes/README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

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

![](vignettes/README_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

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

![](vignettes/README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

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

![](vignettes/README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

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

![](vignettes/README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

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

![](vignettes/README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### custom\_splom

Custom scatterplot matrix (SPLOM)

``` r
library(lattice)
data(mtcars)

# Draw a scatterplot matrix of all variables of a 
# data frame against each other.
custom_splom(mtcars[1:5])
```

![](vignettes/README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

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

![](vignettes/README_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->
