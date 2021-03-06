#' Plot genes along a linear axis
#'
#' This panel function allows to draw genes with start and end coordinates 
#' as main input. Optional vectors for `gene_name` or `gene_strand` must have 
#' same length as `x`, `y`. This function supports paneling and grouping 
#' (e.g. by gene functional category) just as regular panel functions.
#'
#' @param x,y (numeric) start and end positions e.g. in bp, respectively
#' @param groups grouping variable passed down from xyplot (does not need to be specified)
#' @param subscripts subscripts passed down from xyplot (does not need to be specified)
#' @param gene_name (character) optional names of the genes to be plotted.
#' @param gene_strand (character) optional strand information for each gene ('+' or '-').
#' @param draw_labels (logical) if gene name labels are drawn
#' @param arrows (logical) draw arrow like boxes (default) or plain or boxes
#' @param origin (numeric) the horizontal line that serves as origin for drawing genes
#' @param height (numeric) height of the boxes/arrows
#' @param offset (numeric) distance of boxes/arrows from origin (default: 0)
#' @param tip (numeric) pointedness of the arrow head (in native X units. Defaults to a 30th of x scale)
#' @param col (character) color (vector) to be used for boxes/arrows.
#'   The default, NULL, uses colors supplied by the top level function.
#' @param col_labels (character) color used for text labels and lines
#' @param rot_labels (numeric) degree of rotation for text labels (0 for no rotation at all)
#' @param dist_labels (numeric) relative distance of labels to origin, defaults to 1.5 times box height
#' @param group.number internal parameter only used when function is called as panel.groups
#'   argument from within panel.superpose. Does not need to be specified manually.
#' @param ... other arguments passed to the function
#' @export
#' 
#' @examples
#' library(lattice)
#' 
#' # table with dummdy genetic loci
#' genes <- data.frame(
#'   gene_name = c("abc", "def", "ghi", "jkl"),
#'   gene_strand = c("+", "+", "+", "-"),
#'   gene_start = c(123, 178, 245, 310),
#'   gene_end = c(167, 233, 297, 354)
#' )
#' 
#' # plot genes on a linear map
#' xyplot(gene_end ~ gene_start, genes,
#'   groups = gene_strand,
#'   scales = list(y = list(draw = FALSE)),
#'   xlim = c(80, 380), ylim = c(-3,2),
#'   xlab = "", ylab = "",
#'   gene_strand = genes[["gene_strand"]],
#'   gene_name = genes[["gene_name"]],
#'   panel = function(x, y, ...) {
#'     panel.grid(h = -1, v = -1, col = grey(0.9))
#'     panel.geneplot(x, y, arrows = TRUE, ...)
#'   }
#' )
#' 
#' # same example with customized arrows
#' xyplot(gene_end ~ gene_start, genes,
#'   groups = gene_strand,
#'   scales = list(y = list(draw = FALSE)),
#'   xlim = c(80, 380), ylim = c(-3,2),
#'   xlab = "", ylab = "",
#'   gene_strand = genes[["gene_strand"]],
#'   gene_name = genes[["gene_name"]],
#'   panel = function(x, y, ...) {
#'     panel.grid(h = -1, v = -1, col = grey(0.9))
#'     panel.geneplot(x, y, arrows = TRUE, offset = 0.5, 
#'       height = 0.6, rot_labels = 0, tip = 3, col_labels = 1, ...)
#'   }
#' )
#' 
panel.geneplot <- function (x, y,
  groups = NULL, subscripts = NULL,
  gene_name = NULL, gene_strand = NULL,
  draw_labels = TRUE, arrows = TRUE,
  origin = 0, height = 1, offset = 0,
  tip = NULL, col = NULL,
  col_labels = grey(0.3),
  rot_labels = 35,
  dist_labels = 1.5,
  group.number = NULL, ...) {
  
  # if groups are specified first call panel.superpose for each group,
  # otherwise plot directly
  if (!is.null(groups)) {
    
    # draw a 'bottom' line, one for all groups
    panel.abline(h = origin, lwd = 1.5, col = col_labels)
    
    panel.superpose(x = x, y = y,
      groups = groups, subscripts = subscripts,
      panel.groups = panel.geneplot,
      gene_name = gene_name, gene_strand = gene_strand,
      draw_labels = draw_labels, arrows = arrows,
      origin = origin, height = height, offset = offset, 
      tip = tip, col_labels = col_labels, 
      rot_labels = rot_labels, 
      dist_labels = dist_labels, ...)
    
  } else {
    
    # if paneling is used, subset arguments by subscripts
    if (!is.null(subscripts)) {
      gene_strand = gene_strand[subscripts]
      gene_name = gene_name[subscripts]
    }
    
    # determine graphical parameters if not passed, unfortunately default
    # color in panel.superpose is "black" not NULL
    if (is.null(col)) {
      col <- lattice::trellis.par.get()$superpose.symbol$col
    } else if (col == "black") {
      col <- lattice::trellis.par.get()$superpose.symbol$col
    }
    if (is.null(group.number)) {
      col <- col[1]
    } else {
      col <- col[group.number]
    }
    
    # set strand to only "+" if not provided
    if (!is.null(gene_strand)) {
      if (!all(unique(gene_strand) %in% c("+", "-"))) {
        stop("'gene_strand' must be a vector of '+' or '-'")
      }
    } else {
      gene_strand = rep_len("+", length(x))
    }
    
    # reformat strand
    strand = ifelse(gene_strand == "+", 1, -1)
    
    # determine Y coordinates depending on offset
    y0 <- origin-(0.5*height)+(offset*height*strand)
    y1 <- origin+(0.5*height)+(offset*height*strand)
    
    # draw arrows or boxes
    if (arrows) {
      panel.arrowbox(x0 = x, x1 = y,
        y0 = rep(y0, length(x)), y1 = rep(y1, length(y)),
        direction = strand, col = col, tip = tip, ...)
    } else {
      panel.rect(xleft = x, ybottom = y0, xright = y,
        ytop = y1, col = col, ...)
    }
    
    # draw text labels
    if (draw_labels) {
      # text labels
      panel.text(x+(y-x)/2, origin-(dist_labels*height)-(offset*height),
        gene_name, col = col_labels,
        srt = rot_labels, adj = c(1.0, 1.0), pos = NULL, ...)
      
      # label lines
      panel.segments(
        x0 = x+(y-x)/2, y0 = y0,
        x1 = x+(y-x)/2, y1 = origin-(dist_labels*height*0.9)-(offset*height),
        col = col_labels, lwd = 0.8)
    }
  }
}


