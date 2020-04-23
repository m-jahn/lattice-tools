#' Draw custom keys in lattice plots
#' 
#' This custom panel function for lattice plots allows to draw
#' a key (legend) inside a lattice panel, with more customization
#' options than the lattice default.
#' 
#' @param groups grouping variable passed down from xyplot (does not need to be specified)
#' @param labels (character) list of the labels to draw
#' @param which.panel (numeric) the panel(s) where key(s) should be drawn
#' @param points (logical) if points should be drawn
#' @param lines (logical) if lines should be drawn
#' @param rectangles (logical) if rectangles should be drawn
#' @param corner (numeric) vector of length 2 indicating the position of the key,
#'   in Normalised Parent Coordinates (0 to 1)
#' @param col,lwd,lty,pch,cex,point.cex graphical parameters to draw key labels
#'   and symbols
#' @param ... other arguments passed to the function
#' 
#' @examples
#' library(lattice)
#' data(mtcars)
#' 
#' # Two examples for a custom lattice key
#' # inside a panel. The first with taking all arguments from the 
#' # top-level plotting function, the second with custom arguments.
#' xyplot(mpg ~ 1/wt | factor(vs), mtcars,
#'   groups = carb, pch = 19, cex = 1,
#'   panel = function(x, y, ...) {
#'     panel.xyplot(x, y, ...)
#'     panel.key(...)
#'     panel.key(labels = letters[1:5], which.panel = 2, 
#'       corner = c(0.9, 0.1), col = 1:5, pch = 3, cex = 1)
#'   }
#' )
#' 
#' @export
# ------------------------------------------------------------------------------
panel.key <- function (
  groups = NULL, labels = NULL,
  which.panel = 1, pch = 1, cex = 0.8,
  point.cex = NULL, 
  points = TRUE, lines = FALSE, rectangles = FALSE,
  col = NULL,
  lwd = trellis.par.get()$superpose.line$lwd[1],
  lty = trellis.par.get()$superpose.line$lty[1],
  corner = c(0.1, 0.9), ...)
{
  # define colors
  if (is.null(col)) {
    cols <- lattice::trellis.par.get()$superpose.symbol$col
  } else {
    cols <- col
  }
  
  # map groups or labels to colors
  if (is.null(labels)) {
    if (!is.null(groups)) {
      
      # determine graphical parameters from groups
      groups <- as.factor(groups)
      labels <- levels(groups)
      col <- rep(cols, length.out = length(levels(groups)))
    
    } else {
      stop("Either 'groups' or 'labels' muste be supplied")
    }
  } else {
    # use labels if no groups is supplied
    col <- rep(cols, length.out = length(unique(labels)))
  }
  
  if (panel.number() %in% which.panel) {
    
    key <- simpleKey(labels, points = points, lines = lines, rectangles = rectangles,...)
    key$text$col <- col
    key$text$cex <- cex
    
    if (points == TRUE) {
      key$points$col <- col
      key$points$pch <- pch
      key$points$cex <- ifelse(!is.null(point.cex), point.cex, cex)
    }
    
    if (lines == TRUE) {
      key$lines$col <- col
      key$lines$lwd <- lwd
      key$lines$lty <- lty
    }
    
    key.gf <- draw.key(key, draw = FALSE)
    vp <- grid::viewport(
      x = grid::unit(corner[1], "npc") + grid::unit(0.5 - corner[1], "grobwidth", list(key.gf)), 
      y = grid::unit(corner[2], "npc") + grid::unit(0.5 - corner[2], "grobheight", list(key.gf))
    )
    
    grid::pushViewport(vp)
    grid::grid.draw(key.gf)
    grid::upViewport()
    
  }
}