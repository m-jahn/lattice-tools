#' Make a lollipop chart
#'
#' This panel function creates lollipop charts. A lollipo chart is simply an XY-plot
#' where the single points are supported by a horizontal or vertical line originating
#' from one of the axes, or a specified threshold.
#'
#' @param x,y (numeric) variables to be plotted
#' @param groups grouping variable passed down from xyplot (does not need to be specified)
#' @param subscripts subscripts passed down from xyplot (does not need to be specified)
#' @param group.number (numeric) internal variable for grouping, does not need to be specified
#' @param col_handle (character, numeric) color of the lollipop handle (defaults to superpose.symbol$col)
#' @param alpha,lty,lwd (numeric) graphical parameters
#' @param origin (character) axis that serves as starting point for the lollipop handle.
#'   One of "top", "bottom", "left", "right" (default: "bottom")
#' @param custom_threshold (numeric) Option to specify a custom starting point instead of axis
#' @param ... other arguments passed to the function
#' @export
#' 
#' @examples
#' library(lattice)
#' data(mtcars)
#' mtcars$car <- rownames(mtcars)
#' 
#' xyplot(mpg ~ factor(car, car[order(mpg)]), mtcars[1:10, ],
#'   pch = 19, xlab = "", lwd = 2,
#'   scales = list(x = list(rot = 35)),
#'   panel = function(x, y, ...) {
#'     panel.lollipop(x, y, ...)
#'   }
#' )
#' 
#' # with grouping, and lollipops hanging from top
#' xyplot(mpg ~ factor(car, car[order(mpg)]), mtcars[1:10, ],
#'   groups = gear, pch = 19, xlab = "", lwd = 2,
#'   scales = list(x = list(rot = 35)),
#'   panel = function(x, y, ...) {
#'     panel.lollipop(x, y, origin = "top", ...)
#'   }
#' )
#' 
panel.lollipop <- function(x, y,
  groups = NULL, subscripts,
  group.number = NULL, col_handle = NULL,
  alpha = 1, lty = 1, lwd = 1,
  origin = "bottom",
  custom_threshold = NULL, ...)
{
  # call function recursively for each group
  if (!is.null(groups)){
    panel.superpose(x = x, y = y,
      subscripts = subscripts,
      groups = groups,
      col_handle = col_handle,
      alpha = alpha, lty = lty, lwd = lwd,
      origin = origin,
      custom_threshold = custom_threshold,
      panel.lollipop, ...)
  
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
    
    # plot lollipop handle
    limits <- current.panel.limits()
    if (origin == "bottom"){
      x0 <- x; y0 = min(limits$ylim)
    } else if (origin == "top") {
      x0 <- x; y0 = max(limits$ylim)
    } else if (origin == "left") {
      x0 <- min(limits$xlim); y0 = y
    } else if (origin == "right") {
      x0 <- max(limits$xlim); y0 = y
    }
    
    # set custom threshold
    if (!is.null(custom_threshold)) {
      if (origin %in% c("top", "bottom")) {
        y0 = custom_threshold
      } else {
        x0 = custom_threshold
      }
    }
    
    panel.segments(x0 = x0, y0 = y0, x1 = x, y1 = y, col = col_handle,
      alpha = alpha, lty = lty, lwd = lwd)
    
    # plot points
    panel.xyplot(x, y, ...)
  }
}