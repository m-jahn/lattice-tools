#' Draw summary text labels in lattice plots
#' 
#' This panel function allows to draw text labels such as the mean on
#' arbitrary groups of data points. Text labels will be drawn for
#' groups of identical x values with optional subsetting 
#' by grouping or paneling variables. This function complements
#' `panel.errbars()` and `panel.barplot` as it supports drawing 
#' labels beside each other for different groups.
#' 
#' @param x,y (numeric, character) variables to be plotted. The x variable 
#'   is treated as a grouping variable, i.e. error bars are calculated between groups
#'   of unique x values.
#' @param groups grouping variable passed down from xyplot (does not need to be specified).
#' @param subscripts subscripts passed down from xyplot (does not need to be specified).
#' @param FUN_mean (function) the function that used to calculate summary statistics.
#' @param digits (numeric) number of digits for numeric text labels.
#' @param offset (numeric) offset in native plot units used to adjust the position of text labels.
#'   If offset is length 1, only the vertical position is adjusted; if it is length 2 the horizontal 
#'   and vertical position is adjusted.
#' @param ewidth (numeric) width of the error bar whiskers.
#' @param beside (logical) draw bars/points next to each other (default: FALSE).
#' @param col (character) color (vector) to be used for (groups of) labels.
#'   The default, NULL, uses colors supplied by the top level function.
#' @param group.number internal parameter only used when function is called as panel.groups
#'   argument from within panel.superpose. Does not need to be specified manually
#' @param ... other arguments passed to the function
#' @export
#' 
#' @examples
#' library(lattice)
#' data(mtcars)
#' 
#' # annotate mean values 
#' xyplot(mpg ~ factor(cyl) | factor(vs), mtcars,
#'   lwd = 2, pch = 19, offset = 3, digits = 1,
#'   panel = function(x, y, ...) {
#'     panel.errbars(x, y, ...)
#'     panel.annotate(x, y, ...)
#'   }
#' )
#' 
#' # works also with grouping variable. Takes the same arguments
#' # "beside" and "ewidth" as panel.errbars() and panel.barplot()
#' # to plot labels for different groups aside of each other
#' xyplot(mpg ~ factor(cyl) | factor(vs), mtcars,
#'   lwd = 2, pch = 19, groups = gear, digits = 1,
#'   offset = 3, beside = TRUE,
#'   panel = function(x, y, ...) {
#'     panel.errbars(x, y, ...)
#'     panel.annotate(x, y, ...)
#'   }
#' )
panel.annotate <- function(x, y,
  groups = NULL, subscripts,
  FUN_mean = mean, digits = NULL,
  offset = NULL, ewidth = 0.08,
  beside = FALSE, col = NULL,
  group.number = NULL, ...)
{
  # call function recursively for each group
  if (!is.null(groups)) {
    
    # determine graphical parameters from groups
    groups <- as.factor(groups)
    if (is.null(col)) {
      cols <- lattice::trellis.par.get()$superpose.symbol$col
    } else {
      cols <- col
    }
    col <- rep(cols, length.out = length(levels(groups)))
    
    # option to nudge text labels aside to comply with 
    # panel.errbars and panel.barplot
    if (beside) {
      beside <- seq_along(levels(groups)) * 2 * ewidth
      beside <- beside-max(beside)/2-ewidth
    }
    
    panel.superpose(x = x, y = y,
      groups = groups, 
      subscripts = subscripts,
      panel.groups = panel.annotate,
      FUN_mean = FUN_mean, digits = digits,
      offset = offset, ewidth = ewidth,
      beside = beside, col = col, ...)
    
  } else {
    
    # determine offset for X and Y position
    if (is.null(offset)) {
      offset <- c(0, 0)
    } else if (is.numeric(offset)) {
      if (length(offset) == 1) offset <- c(0, offset) else
      if (length(offset) == 2) offset <- offset
    } else {
      stop("Offset must be NULL or a numeric value of length 1 or 2")
    }
    
    # determine Y values for each unique variable of X
    if (is.factor(x)) x <- as.numeric(x)
    means <- tapply(y, x, FUN_mean)
    x_unique <- as.numeric(names(means))
    
    # nudge x values if beside was TRUE
    if (!is.logical(beside)) {
      x_unique <- x_unique+beside[group.number]
    }
    
    # optionally format values
    if (!is.null(digits)) {
      means <- round(means, digits)
    }
    
    # get default color if no color is specified
    if (is.null(col)) {
      col <- lattice::trellis.par.get()$plot.symbol$col
    }
    
    # plot text labels
    panel.text(
      x_unique+offset[1], 
      means+offset[2], labels = means, 
      col = col, ...)
  }
}
