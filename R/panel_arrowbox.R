#' Panel function to draw box like arrows
#'
#' Custom lattice panel function to draw boxes with arrow head from 2 XY 
#' coordinates and direction argument. Each argument can be a vector of same legnth
#' so that multiple arrows can be dran at once.
#'
#' @param x0,x1,y0,y1 (numeric) coordinates of the two points determining the 
#'   rectangle (bottom left, top right).
#' @param direction (numeric) direction in which arrow should point, 
#'   1 for right, -1 for left side
#' @param tip (numeric) pointedness of the arrow head (in native X units. Defaults to a 30th of x scale)
#' @param ... other arguments passed to the function
#' @export
#' 
#' @examples
#' library(lattice)
#' 
#' xyplot(1:3 ~ 4:6, col = grey(0.5),
#'   panel = function(x, y, ...) {
#'     panel.xyplot(x, y, ...)
#'     panel.arrowbox(x0 = c(4, 5), y0 = c(2.5, 1), 
#'       x1 = c(5, 6), y1 = c(3, 1.5), 
#'       direction = c(1, -1), ...)
#'   }
#' )
#' 
panel.arrowbox =  function(x0, x1, y0, y1, 
  direction = NULL, tip = NULL, ...) {
  
  # default direction is right
  if (is.null(direction)) {
    direction = rep(1, length(x0))
  } else if (length(direction < length(x0))) {
    direction = rep_len(direction, length(x0))
  }
  
  # determine strength of inset arrow head automatically from xlimits,
  # or manually
  if (is.null(tip)) {
    tip = rep(abs(diff(current.panel.limits()$xlim))/30, length(x0))
  } else if (length(tip < length(x0))) {
    tip = rep_len(tip, length(x0))
  }
  
  for (i in seq_along(x0)) {
    # construct coordinates for each arrow from input
    # direction of arrow is defined as 1 (right), or -1 (left)
    if (direction[i] == 1) {
      xcoords = c(x0[i], x1[i]-tip[i], x1[i], x1[i]-tip[i], x0[i])
      ycoords = c(y0[i], y0[i], y0[i]+((y1[i]-y0[i])/2), y1[i], y1[i])
    } else {
      xcoords = c(x0[i]+tip[i], x0[i], x0[i]+tip[i], x1[i], x1[i])
      ycoords = c(y0[i], y0[i]+((y1[i]-y0[i])/2), y1[i], y1[i], y0[i])
    }
    panel.polygon(x = xcoords, y = ycoords, ...)
  }
}
