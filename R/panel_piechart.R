#' Draw pie and ring charts in lattice plots
#' 
#' This custom panel function for lattice plots allows to draw
#' pie charts (or rings) while still being able to use the typical
#' lattice way of subsetting data. The function can be used
#' within xyplot() but only one variable needs to be supplied (x).
#' Grouping is supported in which the x variable is aggregated (summed up) over
#' each unique group.
#' 
#' @importFrom grid grid.polygon
#' @importFrom grid grid.lines
#' @importFrom grid grid.text
#' @importFrom grid gpar
#' @importFrom utils tail
#' @importFrom lattice trellis.par.get

#' @param x (numeric, character) variable to be plotted
#' @param groups grouping variable passed down from xyplot (does not need to be specified)
#' @param subscripts subscripts passed down from xyplot (does not need to be specified)
#' @param fun (function) used to perform optional aggregation over groups (default: sum)
#' @param diameter_inner (numeric) diameter of the inner circle of the pie/ring (default: 0.1)
#' @param diameter_sector (numeric) diameter of the outer circle of the pie/ring (default: 0.2)
#' @param clockwise not impemented yet
#' @param start_angle not impemented yet
#' @param col,border,lty,lwd,cex (character, numeric) graphical parameters
#' @param ... other arguments passed to the function
#' 
#' @examples
#' library(grid)
#' library(lattice)
#' 
#' data("USMortality")
#' 
#' # A simple example using lattice paneling
#' xyplot( ~ Rate | Sex, USMortality,
#'   main = "US mortality rates by sex",
#'   scales = list(draw = FALSE),
#'   panel = function(x, ...) {
#'     panel.piechart(x, ...)
#'   }
#' )
#' 
#' # A more advanced example using grouping and
#' # adjusting graphical parameters. The main variable 
#' # 'x' is now summed up for each value of 'groups'
#' xyplot( ~ Rate | Sex, USMortality,
#'   groups = gsub(" ", "\n", Cause), 
#'   col = heat.colors(10),
#'   border = grey(0.3), cex = 0.7,
#'   main = "US mortality rates by sex",
#'   scales = list(draw = FALSE),
#'   panel = function(x, ...) {
#'     panel.piechart(x, diameter_sector = 0.1, ...)
#'   }
#' )
#' @export
# ------------------------------------------------------------------------------
panel.piechart <- function(
  x, groups = NULL, subscripts = NULL,
  fun = function(x) sum(x, na.rm = TRUE),
  diameter_inner = 0.1, diameter_sector = 0.2,
  clockwise = FALSE, start_angle = if (clockwise) 90 else 0,
  col = NULL, border = NULL, lty = NULL, 
  lwd = NULL, cex = NULL, ...)
{ 
  # Two possible user scenarios:
  # ----------------------------------------------------------------------------
  # 1. 'groups' is not supplied, 
  # then we simply plot all unique values of x individually
  if (is.null(groups)) {
    if (!is.numeric(x)) {
      x <- table(x)
      labels = names(x)
    } else {
      labels = as.character(x)
    }
    weights <- cumsum(x)/tail(cumsum(x), 1)
  }
  # 2. summarize x over groups by user defined function, e.g. the sum
  # the cell size is calculated as aggregated relative fraction of total
  else {
    x <- tapply(x, groups[subscripts], fun)
    labels = names(x)
    weights <- cumsum(x)/tail(cumsum(x), 1)
  }
  
  # graphical parameters
  # ----------------------------------------------------------------------------
  # get parameters from default plot options
  if (is.null(col)) {
    col <- lattice::trellis.par.get()$superpose.polygon$col
  }
  if (is.null(border)) {
    border <- lattice::trellis.par.get()$superpose.polygon$border
  }
  if (is.null(lty)) {
    lty <- lattice::trellis.par.get()$superpose.polygon$lty
  }
  if (is.null(lwd)) {
    lwd <- lattice::trellis.par.get()$superpose.polygon$lwd
  }
  if (is.null(cex)) {
    cex <- lattice::trellis.par.get()$superpose.symbol$cex
  }
  
  # for colors, we need to handle grouping so it's
  # consistent over all panels
  if (!is.null(groups)) {
    groups <- as.factor(groups)
    # assign colors to all possible groups 
    # (not just the ones in respective panel)
    col <- rep(col, length.out = length(levels(groups)))
    col <- col[sort(unique(as.numeric(groups)[subscripts]))]
    # assign colors to weights if no groups supplied
  } else {
    col <- rep(col, length.out = length(weights))
  }
  # repeat all other graphical pars
  border <- rep(border, length.out = length(weights))
  lty <- rep(lty, length.out = length(weights))
  lwd <- rep(lwd, length.out = length(weights))
  cex <- rep(cex, length.out = length(weights))
  
  
  # apply drawing function in a loop
  # ----------------------------------------------------------------------------
  lower_bound <- c(0, weights[-length(weights)])
  upper_bound <- weights
  
  for (i in seq_along(weights)) {
    draw_sector(
      lower_bound = lower_bound[[i]],
      upper_bound = upper_bound[[i]],
      diameter_inner = diameter_inner,
      diameter_sector = diameter_sector,
      label = labels[i],
      col = col[i], border = border[i], 
      lty = lty[i], lwd = lwd[i], cex = cex
    )
  }

}
