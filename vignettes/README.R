## ---- eval = FALSE------------------------------------------------------------
#  require(devtools)
#  devtools::install_github("https://github.com/m-jahn/lattice-tools")

## ---- echo = FALSE------------------------------------------------------------
# set seed to obtain same pattern for randomly sampled values in strip-plots etc.
set.seed(123)

## ---- fig.height = 3, fig.width = 5-------------------------------------------
library(lattice)
library(latticetools)
data(mtcars)

# annotate mean values
xyplot(mpg ~ factor(cyl) | factor(vs), mtcars,
  lwd = 2, pch = 19, offset = 2, digits = 1,
  panel = function(x, y, ...) {
    panel.errbars(x, y, ...)
    panel.annotate(x, y, ...)
  }
)

# works also with grouping variable. Takes the same arguments
# "beside" and "ewidth" as panel.errbars() and panel.barplot()
# to plot labels for different groups aside of each other
xyplot(mpg ~ factor(cyl) | factor(vs), mtcars,
  lwd = 2, pch = 19, groups = gear, digits = 1,
  offset = 3, beside = TRUE,
  panel = function(x, y, ...) {
    panel.errbars(x, y, ...)
    panel.annotate(x, y, ...)
  }
)

## ---- fig.height = 3, fig.width = 5-------------------------------------------
library(lattice)

xyplot(1:3 ~ 4:6, col = "#0080ff",
  panel = function(x, y, ...) {
    panel.arrowbox(x0 = c(4, 5), y0 = c(2.5, 1),
      x1 = c(5, 6), y1 = c(3, 1.5),
      direction = c(1, -1), ...)
  }
)

## ---- fig.height = 3, fig.width = 5-------------------------------------------
# mean and stdev error bars are drawn for
# common x values
xyplot(mpg ~ factor(cyl), mtcars, lwd = 2, 
  panel = function(x, y, ...) {
    panel.barplot(x, y, ...)
  }
)

# using the same variable for x and grouping will
# result in typical lattice behavior
xyplot(mpg ~ factor(cyl), mtcars, 
  groups = cyl, lwd = 2,
  panel = function(x, y, ...) {
    panel.barplot(x, y, ...)
  }
)

# we can also use different variables for the x var, grouping,
# and paneling. As a visual control that error bars are drawn
# for the correct groups we overlay the single data points.
xyplot(mpg ~ factor(cyl) | factor(vs), mtcars,
  groups = gear, lwd = 2, auto.key = list(columns = 3),
  panel = function(x, y, ...) {
    panel.barplot(x, y, beside = TRUE, draw_points = TRUE, ...)
  }
)

# alternatively, means and error margins can be supplied directly. 
# In this case means are supplied as unique combinations
# of y and x while error_margin is a separate vector with same length as y.
mtcars_means <- data.frame(
  cyl = sort(unique(mtcars$cyl)),
  mpg = with(mtcars, tapply(mpg, cyl, mean)),
  stdev = with(mtcars, tapply(mpg, cyl, sd))
)

# you might have to adjust the y-scale as it is determined from the
# range of the y variable only, ignoring the extension through error bars.
xyplot(mpg ~ factor(cyl), mtcars_means,
  error_margin = mtcars_means$stdev,
  ylim = c(9, 36), groups = cyl,
  lwd = 2, pch = 19, cex = 1.5,
  panel = function(x, y, ...) {
    panel.barplot(x, y, ...)
  }
)

## ---- fig.height = 3, fig.width = 5-------------------------------------------
# simple example
df <- data.frame(
  Y = sample(1:10, 60, replace = TRUE), 
  X = factor(rep(1:3, each = 20))
)

# beeswarm plot
xyplot(Y ~ X, df, groups = X, panel = panel.beeswarm)

# but with continuous Y variable, it doesn't work as expected
df$Y <- rnorm(60)
xyplot(Y ~ X, df, groups = X, panel = panel.beeswarm)

# for this purpose we can bin the Y variable into groups
xyplot(Y ~ X, df, groups = X, 
  panel = function(x, y, ...) {
    panel.beeswarm(x, y, bin_y = TRUE, breaks_y = 10, ...)
})

# the breaks for Y bins are computed for each panel independently.
# we can also supply fixed bins via the 'breaks_y' argument
# to obtain the same binning for each panel
xyplot(Y ~ factor(rep(1, length(Y))) | X, df, groups = X,
  panel = function(x, y, ...) {
    panel.beeswarm(x, y, bin_y = TRUE,
      breaks_y = seq(-4, 4, length.out = 20), ...)
})

## ---- fig.height = 5, fig.width = 8-------------------------------------------
library(grid)
library(lattice)

data("mtcars")
mtcars$car <- rownames(mtcars)

# A standard example using lattice grouping and paneling;
# We can also draw boxes around labels and change label size
xyplot(mpg ~ wt | factor(cyl), mtcars,
  groups = cyl, pch = 19, labels = mtcars$car,
  as.table = TRUE, layout = c(3, 1), cex = 0.6,
  panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.directlabels(x, y, draw_box = TRUE, box_line = TRUE, ...)
  }
)

# A similar plot but without grouping. This requires explicit
# use of subscripts
xyplot(mpg ~ wt | factor(cyl), mtcars,
  pch = 19, labels = mtcars$car,
  as.table = TRUE, layout = c(3, 1), cex = 0.6,
  panel = function(x, y, subscripts, ...) {
    panel.xyplot(x, y, ...)
    panel.directlabels(x, y, subscripts = subscripts, 
      draw_box = TRUE, box_fill = "white", ...)
  }
)

# An example without panels and more groups
xyplot(mpg ~ wt, mtcars,
  groups = hp, pch = 19, 
  labels = mtcars$wt, cex = 0.6,
  panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.directlabels(x, y, draw_box = TRUE, box_line = TRUE, ...)
  }
)

## ---- fig.height = 3, fig.width = 5-------------------------------------------
library(lattice)
data(mtcars)

# vertical dumbbells
xyplot(mpg ~ factor(gear), mtcars,
  groups = gear,
  pch = 19, lwd = 2,
  panel = function(x, y, ...) {
    panel.dumbbell(x, y, ...)
  }
)

# horizontal dumbbells
xyplot(factor(gear) ~ mpg, mtcars,
  groups = gear,
  pch = 19, lwd = 2,
  panel = function(x, y, ...) {
    panel.dumbbell(x, y, ...)
  }
)

## ---- fig.height = 3, fig.width = 5-------------------------------------------
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

# using the same variable for x and grouping will
# result in typical lattice behavior
xyplot(mpg ~ factor(cyl), mtcars,
  groups = cyl, lwd = 2, pch = 19, cex = 1.5,
  panel = function(x, y, ...) {
    panel.errbars(x, y, ...)
  }
)

# we can also use different variables for the x var, grouping,
# and paneling.
xyplot(mpg ~ factor(cyl) | factor(vs), mtcars,
  groups = gear, lwd = 2, pch = 19, cex = 1.5,
  panel = function(x, y, ...) {
    panel.errbars(x, y, beside = TRUE, ...)
  }
)

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

## ---- fig.height = 2.5, fig.width = 5-----------------------------------------
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

## ---- fig.height = 3, fig.width = 5-------------------------------------------
library(lattice)
data(mtcars)

# Two examples for a custom lattice key inside a panel.
# The first takes all arguments from the 
# top-level plotting function.
# The second has custom arguments.
xyplot(mpg ~ 1/wt | factor(vs), mtcars,
  groups = carb, pch = 19,
  panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.key(...)
    panel.key(labels = letters[1:5], which.panel = 2, 
      corner = c(0.9, 0.1), col = 1:5, pch = 1:5)
  }
)

## ---- fig.height = 3, fig.width = 5-------------------------------------------
library(lattice)
data(mtcars)
mtcars$car <- rownames(mtcars)

xyplot(mpg ~ factor(car, car[order(mpg)]), mtcars[1:10, ],
  pch = 19, xlab = "", lwd = 2,
  scales = list(x = list(rot = 35)),
  panel = function(x, y, ...) {
    panel.lollipop(x, y, ...)
  }
)

# with grouping, and lollipops hanging from top
xyplot(mpg ~ factor(car, car[order(mpg)]), mtcars[1:10, ],
  groups = gear, pch = 19, xlab = "", lwd = 2,
  scales = list(x = list(rot = 35)),
  panel = function(x, y, ...) {
    panel.lollipop(x, y, origin = "top", ...)
  }
)

## ---- fig.height = 3.5, fig.width = 5.5---------------------------------------
library(grid)
library(lattice)

data("USMortality")

# A simple example using lattice paneling
xyplot( ~ Rate | Sex, USMortality,
  main = "US mortality rates by sex",
  scales = list(draw = FALSE), cex = 0.7,
  panel = function(x, ...) {
    panel.piechart(x, ...)
  }
)

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

## ---- fig.height = 4, fig.width = 5-------------------------------------------
library(lattice)
data(mtcars)

# p-values are calculated between groups of x, grouping is ignored
xyplot(mpg ~ factor(cyl), mtcars, groups = cyl, pch = 19, cex = 0.7,
  panel = function(x, y, ...) {
    panel.stripplot(x, y, jitter.data = TRUE, 
      horizontal = FALSE, amount = 0.15, ...)
    panel.pvalue(x, y, std = 1, symbol = TRUE,
      pvalue = TRUE, offset = 6)
})

## ---- fig.height = 3, fig.width = 5-------------------------------------------
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

## ---- fig.height = 5.5, fig.width = 9-----------------------------------------
library(grid)
library(lattice)

data("mtcars")
mtcars$car <- rownames(mtcars)

# A standard example using lattice grouping and paneling;
# We can also draw boxes around labels and change label size
xyplot(mpg ~ wt | factor(cyl), mtcars,
  groups = cyl, pch = 19, labels = mtcars$car,
  as.table = TRUE, layout = c(3, 1), cex = 0.6,
  panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.repellabels(x, y, draw_box = TRUE, box_line = TRUE, ...)
  }
)

# A similar plot with panels, but without grouping.
# This requires explicit use of subscripts
xyplot(mpg ~ wt | factor(cyl), mtcars,
  pch = 19, labels = mtcars$car,
  as.table = TRUE, layout = c(3, 1), cex = 0.6,
  panel = function(x, y, subscripts, ...) {
    panel.xyplot(x, y, ...)
    panel.repellabels(x, y, subscripts = subscripts,
      draw_box = TRUE, box_fill = "white", ...)
  }
)

# An example without panels and more groups
xyplot(mpg ~ wt, mtcars,
  groups = hp, pch = 19,
  labels = mtcars$wt, cex = 0.6,
  panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.repellabels(x, y, draw_box = TRUE, box_line = TRUE, ...)
  }
)

## ---- fig.height = 3, fig.width = 5-------------------------------------------
library(lattice)
data(mtcars)

# first grouping variable for colors, second for symbols (z)
xyplot(mpg ~ factor(cyl), mtcars,
  groups = gear, z = mtcars$cyl,
  panel = function(x, y, z, ...) {
    panel.symbols(x, y, z, ...)
  }
)

## ---- fig.height = 3, fig.width = 5-------------------------------------------
# use singer data from lattice
library(lattice)
data(singer)
singer$voice.part <- gsub(" [12]", "", as.character(singer$voice.part))
singer$voice.part <- factor(singer$voice.part, c("Soprano", "Alto", "Tenor", "Bass"))

# example with grouping
xyplot(height ~ voice.part, singer, groups = voice.part,
  horizontal = FALSE, pch = 19,
  panel = function(x, y, ...) {
    panel.violinscatter(x, y, ...)
})

# same plot but horizontal orientation
xyplot(voice.part ~ height, singer, groups = voice.part,
  horizontal = TRUE, pch = 19,
  panel = function(x, y, ...) {
    panel.violinscatter(x, y, ...)
})

# example with more and non-discrete data points
df <- data.frame(
  sample = factor(rep(c("A", "B", "C"), each = 300)),
  variable = c(rnorm(300, 0, 3), rnorm(300, 1, 2), rnorm(300, 3, 3))
)

xyplot(variable ~ sample, df,
  horizontal = FALSE, pch = 19, cex = 0.4,
  panel = function(x, y, ...) {
    panel.violinscatter(x, y, ...)
})

## ---- fig.height = 3, fig.width = 5-------------------------------------------
library(lattice)
data(mtcars)

xyplot(mpg ~ factor(carb) | gear, mtcars,
  groups = carb, auto.key = list(columns = 3),
  par.settings = custom.ggplot(),
  panel = function(x, y, ...) {
    panel.grid(h = -1, v = -1, col = "white")
    panel.xyplot(x, y, ...)
    panel.lmline(x, y, ...)
  }
)

## ---- fig.height = 3, fig.width = 5-------------------------------------------
library(lattice)
data(mtcars)

xyplot(mpg ~ factor(carb) | gear, mtcars,
  groups = carb, auto.key = list(columns = 3),
  par.settings = custom.lattice(),
  panel = function(x, y, ...) {
    panel.grid(h = -1, v = -1, col = grey(0.95))
    panel.xyplot(x, y, ...)
    panel.lmline(x, y, ...)
  }
)

## ---- fig.height = 3, fig.width = 5-------------------------------------------
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

## ---- fig.width = 6, fig.height = 6-------------------------------------------
library(lattice)
data(mtcars)

# Draw a scatterplot matrix of all variables of a 
# data frame against each other.
custom_splom(mtcars[1:5])

# We can customize the scatterplot
custom_splom(
  mtcars[1:5],
  col_palette = rainbow(4),
  pscales = 10, 
  xlab = "data points", ylab = "regression",
  pch = 1, col = 1, cex = 0.6
)

