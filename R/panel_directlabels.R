#' Point labels for scatterplots
#' 
#' Draw text labels for all points of a scatterplot using functions from 
#' \code{directlabels}.
#' In contrast to the functionality of the original directlabels package,
#' every point is labelled instead of groups. Labels are also independent from
#' the grouping variable, so that e.g. colors indicate a grouping variable and 
#' labels another. By default, labels adapt the graphical parameters of the 
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
#' @param col (character) color (vector) to be used for labels and lines. 
#'   The default, NULL, uses colors supplied by the top level function.
#' @param cex (numeric) size of text labels and corresponding boxes
#' @param method (list) the positioning method, default is \code{directlabels::smart.grid}
#' @param draw_text (logical) whether to draw text labels or not (default: TRUE)
#' @param draw_line (logical) whether to draw a line to labels or not (default: TRUE)
#' @param draw_box (logical) whether to draw a box around labels or not (default: FALSE) 
#' @param box_fill (character) color of the box fill (default: light grey)
#' @param box_line (character or logical) color of the box border. This
#'   parameters takes as input a color, a logical such as TRUE (uses colors supplied by 
#'   top level function), or NULL (the default)
#' @param ... other arguments passed to the function
#' 
#' @examples
#' library(grid)
#' library(lattice)
#' library(directlabels)
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
#'     panel.directlabel(x, y, draw_box = TRUE, box_line = TRUE, ...)
#'   }
#' )
#' 
#' # A similar plot but without grouping. This requires explicit
#' # use of subscripts
#' xyplot(mpg ~ wt | factor(cyl), mtcars,
#'   pch = 19, labels = mtcars$car,
#'   as.table = TRUE, layout = c(3, 1),
#'   panel = function(x, y, subscripts, ...) {
#'     panel.xyplot(x, y, ...)
#'     panel.directlabel(x, y, subscripts = subscripts, 
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
#'     panel.directlabel(x, y, draw_box = TRUE, box_line = TRUE, ...)
#'   }
#' )
#' 
#' @export
# ------------------------------------------------------------------------------
panel.directlabel <- function(
  x, y, groups = NULL, subscripts = NULL,
  labels = NULL, col = NULL, cex = 0.8,
  x_boundary = NULL, y_boundary = NULL, 
  method = directlabels::smart.grid,
  draw_text = TRUE, draw_line = TRUE,
  draw_box = FALSE, box_fill = grey(0.95),
  box_line = NULL, ...
) {
  
  # Filtering
  # ----------
  # remove NAs if necessary
  valid <- !is.na(x) & !is.na(y)
  
  # remove values by user selection
  if (!is.null(x_boundary)) {
    if (length(x_boundary) == 2) {
      valid <- valid & x >= x_boundary[1] & x <= x_boundary[2]
    } else {
      stop("x_boundary or y_boundary must be length 2.")
    }
  }
  if (!is.null(y_boundary)) {
    if (length(y_boundary) == 2) {
      valid <- valid & y >= y_boundary[1] & y <= y_boundary[2]
    } else {
      stop("x_boundary or y_boundary must be length 2.")
    }
  }
  
  # apply filtering
  if (all(!valid)) return(NULL)
  x <- x[valid]; y <- y[valid]
  if (!is.null(subscripts)) subscripts <- subscripts[valid]

  # groups should be factor, otherwise coerce to it
  if (is.null(col)) {
    if (!is.null(groups)) {
      groups <- as.factor(groups)
      
      # determine graphical parameters from groups
      cols <- lattice::trellis.par.get()$superpose.symbol$col
      cols <- rep(cols, length.out = length(levels(groups)))
      col <- cols[as.numeric(groups)[subscripts]]  
      
    # default color if no groups is supplied
    } else {
      col <- lattice::trellis.par.get()$plot.symbol$col
    }
  }
  
  # if labels are not explicitly supplied, use group labels.
  # subscripts take care that correct labels per panel are selected
  if (is.null(labels)) {
    if (!is.null(groups)) {
      labels <- groups[subscripts]
    } else {
      stop("Neither labels nor groups supplied, no labels to draw.")
    }
  } else if (!is.null(subscripts)) {
    labels <- labels[subscripts]
  } else {
    stop("No subscripts argument supplied. Use 'panel = function(x, y, subscripts, ...)'")
  }
  
  # rename duplicated labels with incremental index numbers
  labels <- stats::ave(labels, labels, FUN = function(x) {
    if (length(x) == 1) return(x)
    else {
      index <- 1 + cumsum(as.numeric(duplicated(x)))
      paste0(x, "_", index)
    }
  })
  
  # convert user coordinate units to cm (see apply.method manual)
  x_cm <- grid::convertX(unit(x, "native"), unitTo = "cm", valueOnly = TRUE)
  y_cm <- grid::convertY(unit(y, "native"), unitTo = "cm", valueOnly = TRUE)
  
  # apply label placing algorithm
  coords <- directlabels::apply.method(
    method, d = data.frame(
      x = x_cm, y = y_cm, 
      x_orig = x_cm, y_orig = y_cm,
      groups = labels, 
      cex = rep(cex, length(x)), 
      index = 1:length(x)
    )
  )
  
  # sort not by length of character labels, but by original order
  coords <- coords[order(coords$index), ]
  # remove indexes from labels
  coords$groups <- gsub("\\_[0-9]*$", "", coords$groups)
  
  # convert back to native units
  coords[c("x", "x_orig", "w", "right", "left")] <-
    apply(coords[c("x", "x_orig", "w", "right", "left")], 2, function(x) {
      grid::convertX(unit(x, "cm"), unitTo = "native", valueOnly = TRUE)
    })
  
  coords[c("y", "y_orig", "h", "top", "bottom")] <-
    apply(coords[c("y", "y_orig", "h", "top", "bottom")], 2, function(x) {
      grid::convertY(unit(x, "cm"), unitTo = "native", valueOnly = TRUE)
    })
  
  # draw label lines
  if (draw_line) {
    with(coords,
      panel.segments(x_orig, y_orig, x, y, col = col, ...)
    )
  }
  
  # draw boxes
  if (draw_box) {
    box_line <- if (is.null(box_line)) NA else 
      if (is.logical(box_line)) {
        if (box_line) col else NA
      } else
      box_line
    
    with(coords,
      panel.rect(
        xleft = left + x - x_orig, 
        ybottom = bottom + y - y_orig, 
        xright = right + x - x_orig, 
        ytop = top + y - y_orig, 
        col = box_fill, border = box_line, ...)
    )
  }
  
  # draw text
  if (draw_text) {
    with(coords, 
      panel.text(x, y, labels = groups, 
        col = col, cex = cex, ...)
    )
  }
}
