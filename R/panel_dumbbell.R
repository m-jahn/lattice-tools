#' Make a dumbbell chart
#'
#' This panel function creates dumbbell charts. Similar to a lollipop chart that shows
#' a single point supported by a horizontal or vertical line, the dumbbell chart shows the
#' minimum and maximum of a distribution connected by a line. The direction of the dumbbell
#' (i.e. vertical or horizontal) can be specified manually, or it is determined from the data
#' automatically (the default).
#'
#' @param x,y (numeric) variables to be plotted
#' @param groups grouping variable passed down from xyplot (does not need to be specified)
#' @param subscripts subscripts passed down from xyplot (does not need to be specified)
#' @param group.number (numeric) internal variable for grouping, does not need to be specified
#' @param col_handle (character, numeric) color of the lollipop handle (defaults to superpose.symbol$col)
#' @param alpha,lty,lwd (numeric) graphical parameters
#' @param direction (character) axis along which the dumbbell is drawn. Possible values that enforce a direction are
#' "vertical" and "horizontal". Default (NULL) is to guess the direction from the input variables
#' @param ... other arguments passed to the function
#' @export
#' 
#' @examples
#' library(lattice)
#' data(mtcars)
#' 
#' # vertical dumbbells
#' xyplot(mpg ~ factor(gear), mtcars,
#'   groups = gear,
#'   pch = 19, lwd = 2,
#'   panel = function(x, y, ...) {
#'     panel.dumbbell(x, y, ...)
#'   }
#' )
#' 
#' # horizontal dumbbells
#' xyplot(factor(gear) ~ mpg, mtcars,
#'   groups = gear,
#'   pch = 19, lwd = 2,
#'   panel = function(x, y, ...) {
#'     panel.dumbbell(x, y, ...)
#'   }
#' )
#' 
panel.dumbbell <- function(x, y,
  groups = NULL, subscripts,
  group.number = NULL, col_handle = NULL,
  alpha = 1, lty = 1, lwd = 1,
  direction = NULL, ...)
{
  # call function recursively for each group
  if (!is.null(groups)){
    panel.superpose(x = x, y = y,
      subscripts = subscripts,
      groups = groups,
      col_handle = col_handle,
      alpha = alpha, lty = lty, lwd = lwd,
      direction = direction,
      panel.dumbbell, ...)
  
  } else {
    
    if (is.null(col_handle)) {
      cols <- lattice::trellis.par.get()$superpose.symbol$col
    } else {
      cols <- col_handle
    }
    if (is.null(group.number)) {
      col_handle <- cols[1]
    } else {
      col_handle <- cols[group.number]
    }
    
    # determine direction from input
    if (is.null(direction)) {
      if (is.factor(x)) {
        direction <- "vertical"
      } else if (is.factor(y)) {
        direction <- "horizontal"
      } else {
        direction <- ifelse(length(unique(x)) < length(unique(y)),
          "vertical", "horizontal")
      }
    }
    
    # obtain min and max
    if (direction == "vertical"){
      limits <- data.frame(
        x0 = sort(unique(x)),
        y0 = tapply(y, x, min),
        x1 = sort(unique(x)),
        y1 = tapply(y, x, max)
      )
    } else {
      limits <- data.frame(
        x0 = tapply(x, y, min),
        y0 = sort(unique(y)),
        x1 = tapply(x, y, max),
        y1 = sort(unique(y))
      )
    }
    
    with(limits, {
      # plot dumbbell handle
      panel.segments(x0 = x0, y0 = y0, x1 = x1, y1 = y1, col = col_handle,
        alpha = alpha, lty = lty, lwd = lwd)
      # plot extrema
      panel.xyplot(c(x0,x1), c(y0, y1), ...)
    })
    
  }
}