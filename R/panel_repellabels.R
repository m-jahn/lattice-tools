#' Point labels for scatterplots (using ggrepel)
#' 
#' Draw text labels for all points of a scatterplot using internal functionality from the
#' \code{ggrepel} package. Note: an alternative panel function, \code{panel.directlabels},
#' carries most of the functionality of this function, but uses \code{directlabels} to
#' calculate label positions instead. The same behavior can be achieved by using
#' \code{panel.directlabels} with "ggrepel" for the \code{positioning} argument.
#' 
#' By default, labels adapt the graphical parameters of the 
#' higher level plot, including coloring according to groups. However, many parameters
#' can be customized.
#' 
#' @importFrom lattice trellis.par.get
#' @importFrom lattice panel.text
#' @importFrom lattice panel.segments
#' @importFrom lattice panel.rect
#' @importFrom directlabels smart.grid
#' @importFrom directlabels apply.method
#' @importFrom grid convertX
#' @importFrom grid convertY
#' @importFrom grid unit
#' @importFrom stats ave
#' 
#' @param x,y (numeric) vectors representing x and y coordinates of points
#' @param groups grouping variable passed down from xyplot (does not need to be specified)
#' @param subscripts subscripts passed down from xyplot (does not need to be specified)
#' @param labels (character) vector of labels to be plotted, if NULL, groups are used
#'   This resembles the behavior of the original \code{directlabels} functions
#' @param x_boundary (numeric) vector of length two, indicating the boundaries of
#'   x for which labels should be drawn (default: NULL) 
#' @param y_boundary (numeric) vector of length two, indicating the boundaries of
#'   y for which labels should be drawn (default: NULL)
#' @param selected (logical) vector of the same length as input data, indicating
#'   which labels should be drawn and which not (default: NULL)
#' @param col (character) color (vector) to be used for labels and lines. 
#'   The default, NULL, uses colors supplied by the top level function.
#' @param cex (numeric) size of text labels and corresponding boxes
#' @param positioning (character) One of "directlabels" or "ggrepel" (default),
#'   indicating the package used to calculate position of labels and bounding boxes
#' @param method (list) the positioning method, default is \code{directlabels::smart.grid}
#' @param draw_text (logical) whether to draw text labels or not (default: TRUE)
#' @param draw_line (logical) whether to draw a line to labels or not (default: TRUE)
#' @param draw_box (logical) whether to draw a box around labels or not (default: FALSE) 
#' @param box_fill (character) color of the box fill (default: light grey)
#' @param box_line (character or logical) color of the box border. This
#'   parameters takes as input a color, a logical such as TRUE (uses colors supplied by 
#'   top level function), or NULL (the default)
#' @param box_scale (numeric) scalar indicating how much boxes surrounding labels
#'   should be expanded or shrunken, in fractions of current panel limits (e.g. 
#'   0.01 of X or Y range). Defaults to NULL
#' @param ... other arguments passed to the function
#' 
#' @examples
#' library(grid)
#' library(lattice)
#' 
#' data("mtcars")
#' mtcars$car <- rownames(mtcars)
#' 
#' # A standard example using lattice grouping and paneling;
#' # We can also draw boxes around labels and change label size
#' xyplot(mpg ~ wt | factor(cyl), mtcars,
#'   groups = cyl, pch = 19, labels = mtcars$car,
#'   as.table = TRUE, layout = c(3, 1), cex = 0.6,
#'   panel = function(x, y, ...) {
#'     panel.xyplot(x, y, ...)
#'     panel.repellabels(x, y, draw_box = TRUE, box_line = TRUE, ...)
#'   }
#' )
#' 
#' # A similar plot with panels, but without grouping.
#' # This requires explicit use of subscripts
#' xyplot(mpg ~ wt | factor(cyl), mtcars,
#'   pch = 19, labels = mtcars$car,
#'   as.table = TRUE, layout = c(3, 1), cex = 0.6,
#'   panel = function(x, y, subscripts, ...) {
#'     panel.xyplot(x, y, ...)
#'     panel.repellabels(x, y, subscripts = subscripts, 
#'       draw_box = TRUE, box_fill = "white", ...)
#'   }
#' )
#' 
#' # An example without panels and more groups
#' xyplot(mpg ~ wt, mtcars,
#'   groups = hp, pch = 19, 
#'   labels = mtcars$wt, cex = 0.6,
#'   panel = function(x, y, ...) {
#'     panel.xyplot(x, y, ...)
#'     panel.repellabels(x, y, draw_box = TRUE, box_line = TRUE, ...)
#'   }
#' )
#' 
#' @export
# ------------------------------------------------------------------------------
panel.repellabels <- function(
  x, y, groups = NULL, subscripts = NULL,
  labels = NULL, col = NULL, cex = 0.8,
  x_boundary = NULL, y_boundary = NULL,
  selected = NULL,
  positioning = "ggrepel",
  method = directlabels::smart.grid,
  draw_text = TRUE, draw_line = TRUE,
  draw_box = FALSE, box_fill = grey(0.95),
  box_line = NULL, box_scale = NULL, ...
) {
  panel.directlabels(
    x, y, groups = groups, subscripts = subscripts,
    labels = labels, col = col, cex = cex,
    x_boundary = x_boundary, y_boundary = y_boundary,
    selected = selected,
    positioning = positioning,
    method = method,
    draw_text = draw_text, draw_line = draw_line,
    draw_box = draw_box, box_fill = box_fill,
    box_line = box_line, box_scale = box_scale, ...
  )
}