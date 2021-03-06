#' Panel function for beeswarm plots
#' 
#' Panel function for beeswarm plots. This panel function works essentially 
#' like a stripplot, but instead of randomly scattering a variable produces 
#' a regular, grid-like pattern of points. Currently only the X variable is 
#' transformed. Continuously distributed data points can be optionally discretized, 
#' both for X and for Y variable.
#' 
#' @author modified after a function from Walmes Zeviani, \email{walmes@ufpr.br},
#'   \code{panel.beeswarm} from package \code{wzRfun}. Idea based on the package
#'   \code{beeswarm}.
#' 
#' @param x,y (numeric, factor) X and Y variables passed to \code{panel.xyplot()}
#' @param groups Passed to \code{panel.xyplot()}.
#' @param subscripts Passed to \code{panel.xyplot()}.
#' @param bin_x (Logical) If X variable is to be binned or not (default FALSE).
#' @param bin_y  (Logical) If Y variable is to be binned or not (default FALSE).
#' @param breaks_x (numeric) Number of breaks for x variable if \code{bin_x = TRUE}.
#'   Alternatively a numeric vector supplying the breaks for bins.
#' @param breaks_y (numeric) Number of breaks for y variable if \code{bin_y = TRUE}.
#'   Alternatively a numeric vector supplying the breaks for bins.
#' @param spread (numeric) Scalar indicating how much values should be spread.
#' @param return_coords (logical) if TRUE returns a list with the calculated x- and y-coordinates 
#'   for each data point
#' @param ... Further arguments passed to \code{xyplot}
#' 
#' @importFrom lattice xyplot
#' @examples
#' 
#' library(lattice)
#' 
#' # simple example
#' df <- data.frame(
#'   Y = sample(1:10, 60, replace = TRUE), 
#'   X = factor(rep(1:3, each = 20))
#' )
#' 
#' xyplot(Y ~ X, df, groups = X, panel = panel.beeswarm)
#' 
#' # but with continuous Y variable, it doesn't work as expected
#' df$Y <- rnorm(60)
#' xyplot(Y ~ X, df, groups = X, panel = panel.beeswarm)
#' 
#' # for this purpose we can bin the Y variable into groups
#' xyplot(Y ~ X, df, groups = X, 
#'   panel = function(x, y, ...) {
#'     panel.beeswarm(x, y, bin_y = TRUE, breaks_y = 10, ...)
#' })
#' 
#' # the breaks for Y bins are computed for each panel independently.
#' # we can also supply fixed bins via the 'breaks_y' argument
#' # to obtain the same binning for each panel
#' xyplot(Y ~ factor(rep(1, length(Y))) | X, df, groups = X,
#'   panel = function(x, y, ...) {
#'     panel.beeswarm(x, y, bin_y = TRUE, 
#'       breaks_y = seq(-4, 4, length.out = 20), ...)
#' })
#' 
#' @export
panel.beeswarm <- function(x, y, 
  groups = NULL, subscripts = NULL,
  bin_x = FALSE, bin_y = FALSE,
  breaks_x = 10, breaks_y = 10, 
  spread = 0.1, return_coords = FALSE, ...
) {
  
  # function to bin original data if there are too many unique values
  # of X or Y variable (discretize data)
  bin_vector <- function(x, breaks) {
    if (length(breaks) == 1) {
      bins <- seq(
        min(x) + diff(range(x))/(2*breaks),
        max(x) - diff(range(x))/(2*breaks),
        length.out = breaks
      )
    } else {
      bins <- breaks
      if (min(x) < min(bins) || max(x) > max(bins))
        warning("Some values are outside of the range specified by breaks_x/y. These value are discarded.")
    }
    bins[cut(x, breaks = breaks, labels = FALSE)]
  }

  if (bin_x) {
    # discretize x
    x <- bin_vector(x, breaks = breaks_x)
  }
  if (bin_y) {
    # discretize y
    y <- bin_vector(y, breaks = breaks_y)
  }
  
  # if no groups are specified, subscripts are simply
  # filled in with a vector of length(x)
  if (is.null(subscripts)) {
    subscripts <- 1:length(x)
  }
  
  xx <- x
  yy <- y
  aux <- by(cbind(yy, xx, subscripts), xx, function(i) {
    or <- order(i[, 1])
    ys <- i[or, 1]
    yt <- table(ys)
    dv <- sapply(unlist(yt),
      FUN = function(j) {
        seq(1, j, l = j) - (j + 1)/2
      }
    )
    if (!is.list(dv)) {
      dv <- as.list(dv)
    }
    xs <- i[or, 2] + spread * do.call(c, dv)
    cbind(x = xs, y = ys, subscripts = i[or, 3])
  })
  
  # reorder by subscripts
  aux <- do.call(rbind, aux)
  aux <- aux[order(aux[, 3]), ]
  
  # call panel.xyplot
  lattice::panel.xyplot(aux[, 1], aux[, 2], 
    groups = groups, subscripts = subscripts, ...)
  
  # optionally return coordinates of points
  list(x = aux[, 1], y = aux[, 2])
}
