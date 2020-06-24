#' Draw barplot with error bars in lattice plots
#' 
#' This custom panel function for lattice plots allows to draw
#' barplots with error bars for arbitrary groups of data points.
#' Error bars will be drawn for groups of identical x values with
#' optional subsetting by grouping or paneling variables.
#' 
#' @importFrom stats sd
#' 
#' @param x,y (numeric, character) variables to be plotted. The x variable 
#'   is treated as a grouping varibale, i.e. error bars are calculated between groups
#'   of unique x values.
#' @param groups grouping variable passed down from xyplot (does not need to be specified)
#' @param subscripts subscripts passed down from xyplot (does not need to be specified)
#' @param error_margin optional vector of error margins if errors are not to be computed, 
#'   but supplied directly. Needs to be of length(y), default is NULL. If supplied,
#'   FUN_mean and FUN_errb are ignored.
#' @param col (character) color (vector) to be used for points and lines. 
#'   The default, NULL, uses colors supplied by the top level function.
#' @param ewidth (numeric) width of the error bars and whiskers
#' @param beside (logical) draw bars/points next to each other (default: FALSE)
#' @param origin (numeric) Y coordinate where bars should originate (default NULL means bottom axis)
#' @param FUN_mean the function used to calculate group (x-variable) means
#' @param FUN_errb the function used to calculate group (x-variable) errors
#' @param ... other arguments passed to the function
#' 
#' @examples
#' library(lattice)
#' data(mtcars)
#' 
#' # mean and stdev error bars are drawn for
#' # common x values
#' xyplot(mpg ~ factor(cyl), mtcars, 
#'   lwd = 2, pch = 19, cex = 1.5,
#'   panel = function(x, y, ...) {
#'     panel.barplot(x, y, ...)
#'   }
#' )
#' 
#' # using the same variable for x and grouping will
#' # result in typical lattice behavior
#' xyplot(mpg ~ factor(cyl), mtcars, 
#'   groups = cyl, lwd = 2,
#'   panel = function(x, y, ...) {
#'     panel.barplot(x, y, ...)
#'   }
#' )
#' 
#' # we can also use different variables for the x var, grouping,
#' # and paneling. As a visual control that error bars are drawn 
#' # for the correct groups we overlay the single data points. 
#' xyplot(mpg ~ factor(cyl) | factor(vs), mtcars,
#'   groups = gear, lwd = 2, auto.key = list(columns = 3),
#'   panel = function(x, y, ...) {
#'     panel.barplot(x, y, beside = TRUE, ...)
#'     panel.stripplot(x, y, jitter.data = TRUE,
#'       horizontal = FALSE, amount = 0.15, ...)
#'   }
#' )
#' 
#' # alternatively, means and error margins can be supplied directly. 
#' # In this case means are supplied as unique combinations
#' # of y and x while error_margin is a separate vector with same length as y.
#' mtcars_means <- data.frame(
#'   cyl = sort(unique(mtcars$cyl)),
#'   mpg = with(mtcars, tapply(mpg, cyl, mean)),
#'   stdev = with(mtcars, tapply(mpg, cyl, sd))
#' )
#' 
#' # you might have to adjust the yscale as it is determined from the
#' # range of the y variable only, ignoring the extension through error bars.
#' xyplot(mpg ~ factor(cyl), mtcars_means,
#'   error_margin = mtcars_means$stdev,
#'   ylim = c(9, 36), groups = cyl,
#'   lwd = 2, pch = 19, cex = 1.5,
#'   panel = function(x, y, ...) {
#'     panel.barplot(x, y, ...)
#'   }
#' )
#' 
#' @export
# ------------------------------------------------------------------------------
panel.barplot <- function (x, y,
  groups = NULL, subscripts = NULL,
  error_margin = NULL,
  col = NULL, ewidth = 0.08, 
  beside = FALSE, origin = NULL,
  FUN_mean = function(x) mean(x, na.rm = TRUE),
  FUN_errb = function(x) sd(x, na.rm = TRUE), ...)
{
  # groups should be factor, otherwise coerce to it
  if (!is.null(groups)) {
    groups <- as.factor(groups)
    # determine graphical parameters from groups
    if (is.null(col))
      cols <- lattice::trellis.par.get()$superpose.symbol$col
    else cols <- col
    col <- rep(cols, length.out = length(levels(groups)))
  
  # default color if no groups is supplied
  } else {
    groups = factor(rep(1, length(x)))
    subscripts = seq_along(groups)
    if (is.null(col))
      col <- lattice::trellis.par.get()$plot.symbol$col
  }
  
  
  panel.rect <- function(
    xleft, ybottom, xright, ytop, 
    x = (xleft + xright)/2, 
    y = (ybottom + ytop)/2, 
    width = xright - xleft, height = ytop - ybottom, 
    col = NULL, fill = NULL, lty = 1, 
    lwd = 1, alpha = 1, font, fontface, ..., 
    identifier = NULL, name.type = "panel"
  ) {
    grid::grid.rect(x = x, y = y, 
      width = width, height = height, 
      default.units = "native",
      gp = grid::gpar(fill = fill, col = col, 
        lty = lty, lwd = lwd, alpha = alpha, ...
      )
    )
  }
  
  # define subset of groups and nudge margin
  subg <- as.numeric(groups[subscripts])
  nudge <- seq_along(levels(groups)) * 2 * ewidth
  nudge <- nudge-max(nudge)/2-ewidth
  
  # loop through different groups (at least one)
  for (val in unique(subg)) {
    
    x_sub <- x[subg %in% val]
    y_sub <- y[subg %in% val]
    
    if (is.null(error_margin)) {
      # aggregate values per group
      means <- tapply(y_sub, x_sub, FUN_mean)
      stdev <- tapply(y_sub, x_sub, FUN_errb)
    } else {
      # if error margins are supplied directly, use tapply
      # simply to emulate same behavior as standard
      error_margin <- error_margin[subscripts]
      means <- tapply(y_sub, x_sub, mean)
      stdev <- tapply(error_margin[subg %in% val], x_sub, mean)
    }
    
    x_sub <- unique(x_sub)
    if (is.factor(x_sub)) x_sub <- sort(as.numeric(x_sub))
    if (beside) x_pos <- x_sub + nudge[val] else x_pos <- x_sub
    if (is.null(origin)) ybottom <- current.panel.limits()$ylim[1]
    else ybottom <- origin
    
    Y <- as.matrix(cbind(means, means-stdev, means+stdev))
    y_sub <- Y[x_sub, 1]
    y0 <- Y[x_sub, 2]
    y1 <- Y[x_sub, 3]
    offs <- ewidth/2
    
    # plot line segments and bars
    panel.segments(x0 = x_pos, x1 = x_pos, y0 = y0, y1 = y1, 
      col = col[val], ...)
    panel.segments(x0 = x_pos - offs, x1 = x_pos + offs, y0 = y0, y1 = y0, 
      col = col[val], ...)
    panel.segments(x0 = x_pos - offs, x1 = x_pos + offs, y0 = y1, y1 = y1, 
      col = col[val], ...)
    panel.rect(xleft = x_pos - ewidth, ybottom = ybottom, xright = x_pos + ewidth, 
      ytop = y_sub, col = col[val], ...)
  }
}