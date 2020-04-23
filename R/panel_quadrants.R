#' Draw quadrants and quadrant statistics in lattice plots
#' 
#' This custom panel function for lattice plots allows to draw
#' custom quadrants and display additional quadrant statistics
#' as often used in biomedial sciences. Groups are ignored.
#' 
#' @importFrom stats t.test
#' 
#' @param x,y (numeric) variables to be plotted
#' @param h,v (numeric) position of the horizontal or vertical threshold for dividing the
#'   data into quadarants. Defaults to the median.
#' @param labels (character) One of 'percent', 'events', or 'none'. Controls what
#'   type of summary is plottted per quadrant
#' @param col,lwd graphical parameters for lines and labels
#' @param margin (numeric) margin of labels to the plot edges in Normalised Parent 
#'   Coordinates, default is 0.1
#' @param na.rm (logical) Should NA or Inf values be removed automatically? Default is FAlSE
#' @param ... other arguments passed to the function
#' 
#' @examples
#' library(lattice)
#' data(mtcars)
#' 
#' # Default behavior for quadrants is to split x and y data
#' # at the respective median, and plot percentage of points 
#' # per quandrant
#' xyplot(mpg ~ 1/wt | factor(vs), mtcars,
#'   groups = carb, pch = 19, cex = 1,
#'   panel = function(x, y, ...) {
#'     panel.xyplot(x, y, ...)
#'     panel.quadrants(x, y, ...)
#'   }
#' )
#' 
#' @export
# ------------------------------------------------------------------------------
panel.quadrants <- function (x, y, h = NULL, v = NULL, 
  labels = "percent", col = grey(0.5), margin = 0.1,
  lwd = trellis.par.get()$superpose.polygon$lwd[1], 
  na.rm = FALSE, ...)
{ 
  
  # remove inf or NA values
  index = !(is.infinite(x*y) | is.na(x*y))
  if (!all(index)) {
    if (na.rm) {
      x = x[index]; y = y[index]
      cat(sum(!index), "NA / Inf values were removed\n")
    } else {
      cat(sum(!index), "NA / Inf values were not removed\n")
    }
  } 
  
  # drawing the horizontal and vertical lines
  if (is.null(h))
    h = median(y)
  if (is.null(v))
    v = median(x)
  panel.abline(h = h, v = v, lwd = lwd, col.line = col)
  
  # plot percentages of the 4 quadrants as text
  quadrant <- list(
    Q1 = sum(x <= v & y >  h),
    Q2 = sum(x >  v & y >  h),
    Q3 = sum(x >  v & y <= h),
    Q4 = sum(x <= v & y <= h)
  )
  
  # can either plot events or percentage, or no labels
  if (labels == "percent") {
    quadrant = sapply(quadrant, function(i) {
      paste0(round(i/length(x)*100, 1), "%")
    })
  } else if (labels == "events") {
    quadrant = paste0("n=", quadrant)
  } else if (labels == "none") {
    warning("no quadrant labels are plotted")
  } else {
    stop("labels must be one of 'percent', 'events', or 'none'")
  }
  
  # actual plotting of labels
  if (labels %in% c("percent", "events")) {
    with(current.panel.limits(), {
      xmargin = (xlim[2]-xlim[1])*margin
      ymargin = (ylim[2]-ylim[1])*margin
      panel.text(xlim[1]+xmargin, ylim[2]-ymargin, pos = 4, labels = quadrant[1], col = col)
      panel.text(xlim[2]-xmargin, ylim[2]-ymargin, pos = 2, labels = quadrant[2], col = col)
      panel.text(xlim[2]-xmargin, ylim[1]+ymargin, pos = 2, labels = quadrant[3], col = col)
      panel.text(xlim[1]+xmargin, ylim[1]+ymargin, pos = 4, labels = quadrant[4], col = col)
    })
  }
}
