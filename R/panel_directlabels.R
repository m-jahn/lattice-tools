#' Point labels for scatterplots (using directlabels)
#' 
#' Draw text labels for all points of a scatterplot using internal functionality from the
#' \code{directlabels} package. Note: an alternative panel function, \code{panel.repellabels},
#' is a wrapper using \code{ggrepel} calculated label positions instead. The same behavior can be
#' achieved by using "ggrepel" for the \code{positioning} argument.
#' 
#' In contrast to the functionality of the original \code{directlabels} package,
#' every point is labelled instead of groups of points. Labels are also independent from
#' the grouping variable, so that e.g. colors indicate one grouping variable and 
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
#' @param positioning (character) One of "directlabels" (default) or "ggrepel",
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
#'     panel.directlabels(x, y, draw_box = TRUE, box_line = TRUE, ...)
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
#'     panel.directlabels(x, y, subscripts = subscripts, 
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
#'     panel.directlabels(x, y, draw_box = TRUE, box_line = TRUE, ...)
#'   }
#' )
#' 
#' @export
# ------------------------------------------------------------------------------
panel.directlabels <- function(
  x, y, groups = NULL, subscripts = NULL,
  labels = NULL, col = NULL, cex = 0.8,
  x_boundary = NULL, y_boundary = NULL,
  selected = NULL,
  positioning = "directlabels",
  method = directlabels::smart.grid,
  draw_text = TRUE, draw_line = TRUE,
  draw_box = FALSE, box_fill = grey(0.95),
  box_line = NULL, box_scale = NULL, ...
) {
  
  # Filtering
  # ----------
  # remove NAs if necessary
  valid <- !is.na(x) & !is.na(y)
  
  # remove values by user selection
  if (!is.null(selected)) {
    if (!is.null(subscripts)) {
      valid <- valid & selected[subscripts]
    } else {
      valid <- valid & selected
    }
  }
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
  
  # color management
  if (!is.null(groups)) {
    # determine graphical parameters from groups
    groups <- as.factor(groups)
    if (is.null(col)) {
      cols <- lattice::trellis.par.get()$superpose.symbol$col
    } else {
      cols <- col
    }
    cols <- rep(cols, length.out = length(levels(groups)))
    col <- cols[as.numeric(groups)[subscripts]]
  } else {
    # default color if no groups is supplied
    if (is.null(col)) {
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
    labels <- labels[valid]
  }
  
  # rename duplicated labels with incremental index numbers
  label_groups <- stats::ave(labels, labels, FUN = function(x) {
    if (length(x) == 1) return(x)
    else {
      index <- 1 + cumsum(as.numeric(duplicated(x)))
      paste0(x, "_", index)
    }
  })
  
  # obtain coordinates either using 'directlabels' or 'ggrepel' packages
  if (positioning == "ggrepel") {
    coords <- get_repelled_labels(
      x = x, y = y,
      labels = labels,
      cex = cex, point_size = 0.001,
      box_padding_x = 0.005,
      box_padding_y = 0.005,
      point_padding_x = 0.01,
      point_padding_y = 0.01
    )
  } else if (positioning == "directlabels") {
    coords <- get_direct_labels(
      x = x, y = y,
      labels = labels,
      label_groups = label_groups,
      cex = cex,
      method = method, ...
    )
  }
  
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
    
    # apply optional shrinking factor for boxes in 
    # fraction of current X and Y axis limits
    if (!is.null(box_scale)) {
      x_scalebox <- diff(current.panel.limits()$xlim)*box_scale
      y_scalebox <- diff(current.panel.limits()$ylim)*box_scale
    } else {
      x_scalebox <- 0
      y_scalebox <- 0
    }
    
    with(coords,
      panel.rect(
        xleft = x1_box - x_scalebox,
        ybottom = y1_box - y_scalebox,
        xright = x2_box + x_scalebox,
        ytop = y2_box + y_scalebox,
        col = box_fill, border = box_line, ...)
    )
  }
  
  # draw text
  if (draw_text) {
    with(coords, 
      panel.text(x, y, labels = label, 
        col = col, cex = cex, ...)
    )
  }
}
