#' Draw barplot with error bars in lattice plots
#' 
#' This panel function allows to draw barplots with error bars for arbitrary 
#' groups of data points. Error bars will be drawn for groups of identical `x`
#' values with optional subsetting by grouping or paneling variables. This 
#' function is very similar to `panel.errbars` only with bars instead of points.
#' 
#' @importFrom stats sd
#' @importFrom colorspace adjust_transparency
#' 
#' @param x,y (numeric, character) variables to be plotted. The x variable 
#'   is treated as a grouping varibale, i.e. error bars are calculated between groups
#'   of unique x values.
#' @param groups grouping variable passed down from xyplot (does not need to be specified)
#' @param subscripts subscripts passed down from xyplot (does not need to be specified)
#' @param error_margin optional input for error margins if errors are not to be computed, 
#'   but supplied directly. Can be a vector of length(y), or a two-column matrix with
#'   first column representing lower and second column upper bounds for each point.
#'   Default is NULL. If supplied, FUN_errb is ignored.
#' @param col (character) color (vector) to be used for points and lines. 
#'   The default, NULL, uses colors supplied by the top level function.
#' @param ewidth (numeric) width of the error bars and whiskers
#' @param fill (numeric, character) optionally specify custom fill for the bars (default NULL, takes colors from col)
#' @param fill_alpha (numeric) scalar setting the transparency of the bars (default: 0.5)
#' @param twoway (logical) draw both upper and lower boundaries (default: FALSE)
#' @param beside (logical) draw bars/points next to each other (default: FALSE)
#' @param draw_points (logical) overlay original points over barplot (default: FALSE)
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
#'     panel.barplot(x, y, beside = TRUE, draw_points = TRUE, ...)
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
#' # you might have to adjust the y-scale as it is determined from the
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
#' # if you supply a two column matrix as the error_margin argument,
#' # error bars with different lower and upper bounds can be drawn
#' error_mat <- matrix(ncol = 2, 1:6)
#' xyplot(mpg ~ factor(cyl), mtcars_means,
#'  error_margin = error_mat, twoway = TRUE, fill = NA,
#'  ylim = c(9, 36), groups = cyl,
#'  lwd = 2, pch = 19, cex = 1.5,
#'  panel = function(x, y, ...) {
#'    panel.barplot(x, y, ...)
#'  }
#' )
#' @export
# ------------------------------------------------------------------------------
panel.barplot <- function (x, y,
  groups = NULL, subscripts = NULL,
  error_margin = NULL,
  col = NULL, ewidth = NULL, fill = NULL,
  fill_alpha = 0.5, twoway = FALSE,
  beside = FALSE, draw_points = FALSE,
  origin = NULL,
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
  
  # determine width of bars if nothing is provided
  if (is.null(ewidth)) {
    ewidth <- 0.5/(1+length(levels(groups)))
  }
  
  panel.rect <- function(
    xleft, ybottom, xright, ytop, 
    x = (xleft + xright)/2, 
    y = (ybottom + ytop)/2, 
    width = xright - xleft, height = ytop - ybottom, 
    col = NULL, fill = NULL, fill_alpha = 1, lty = 1, 
    lwd = 1, alpha = 1, font, fontface, ..., 
    identifier = NULL, name.type = "panel"
  ) {
    if (is.null(fill)) {
      fill <- colorspace::adjust_transparency(col, fill_alpha)
    } else {
      fill <- colorspace::adjust_transparency(fill, fill_alpha)
    }
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
    means <- tapply(y_sub, x_sub, FUN_mean)
    
    if (is.null(error_margin)) {
      # aggregate values per group
      stdev <- tapply(y_sub, x_sub, FUN_errb)
      lower <- stdev; upper <- stdev
    } else {
      # if error margins are supplied directly,
      # differentiate between vector or matrix
      if (!is.matrix(error_margin)) {
        error_margin <- error_margin[subscripts]
        stdev <- tapply(error_margin[subg %in% val], x_sub, mean)
        lower <- stdev; upper <- stdev
      } else {
        error_margin <- error_margin[subscripts, ]
        lower <- tapply(error_margin[subg %in% val, 1], x_sub, mean)
        upper <- tapply(error_margin[subg %in% val, 2], x_sub, mean)
      }
    }
    
    x_s <- unique(x_sub)
    if (is.factor(x_s)) {
      x_s <- sort(as.numeric(x_s))
      if (beside) x_pos <- x_s + nudge[val] else x_pos <- x_s
    } else {
      if (beside) x_pos <- x_s + nudge[val] else x_pos <- x_s
      x_s <- order(x_s)
    }
    if (is.null(origin)) ybottom <- current.panel.limits()$ylim[1]
    else ybottom <- origin
    
    Y <- as.matrix(cbind(means, means-lower, means+upper))
    y_s <- Y[x_s, 1]
    y0 <- Y[x_s, 2]
    y1 <- Y[x_s, 3]
    offs <- ewidth/2
    
    # plot line segments and bars
    panel.segments(x0 = x_pos, x1 = x_pos, y0 = y_s, y1 = y1, 
      col = col[val], ...)
    panel.segments(x0 = x_pos - offs, x1 = x_pos + offs, y0 = y1, y1 = y1, 
      col = col[val], ...)
    if (twoway) {
      panel.segments(x0 = x_pos, x1 = x_pos, y0 = y0, y1 = y_s, 
        col = col[val], ...)
      panel.segments(x0 = x_pos - offs, x1 = x_pos + offs, y0 = y0, y1 = y0, 
        col = col[val], ...)
    }
    panel.rect(xleft = x_pos - ewidth, ybottom = ybottom, xright = x_pos + ewidth, 
      ytop = y_s, col = col[val], fill = fill, fill_alpha = fill_alpha, ...)
    
    # optionally plot original xy values
    if (draw_points) {
      if (beside) x_sub <- as.numeric(x_sub) + nudge[val]
      panel.xyplot(x_sub, y_sub, col = col[val], ...)
    }
  }
}