#' Plot a grouping variable encoded by symbols
#'
#' This panel function allows to plot one additional grouping variable encoded
#' by symbols ('pch') instead of just the default grouping by color.
#' Inspired by `panel.bubbleplot` from package `tactile`.
#'
#' @param x,y (numeric) variables to be plotted
#' @param z (numeric, factor, character) variable encoding symbols. In not numeric, 
#'   will be coerced to factorand then numeric if possible
#' @param groups grouping variable passed down from xyplot (does not need to be specified)
#' @param subscripts subscripts passed down from xyplot (does not need to be specified)
#' @param pch not used in this context and set to NULL
#' @param ... other arguments passed to the function
#' @export
#' 
#' @examples
#' library(lattice)
#' data(mtcars)
#' 
#' # first grouping variable for colors, second for symbols (z)
#' xyplot(mpg ~ factor(cyl), mtcars,
#'   groups = gear, z = mtcars$cyl,
#'   panel = function(x, y, z, ...) {
#'     panel.symbols(x, y, z, ...)
#'   }
#' )
#' 
panel.symbols <- function(x, y, z, 
  groups = NULL, subscripts, 
  pch = NULL, ...)
{
  # coerce non numeric input to factor
  if (!is.numeric(z)) {
    z <- as.numeric(as.factor(z))
  }
  
  # call function recursively for each group
  if (!is.null(groups)){
    panel.superpose(x = x, y = y, z = z, 
      subscripts = subscripts, 
      groups = groups, panel.symbols, ...)
  } else {
    panel.xyplot(x, y, pch = z[subscripts], ...)
  }
}