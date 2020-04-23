#' Custom scatterplot matrix (SPLOM)
#' 
#' Custom wrapper function around lattice `splom` with different upper and lower panel.
#' A scatterplot matrix is a tiled plot where all variables of a data frame are plotted against each other.
#' 
#' @importFrom lattice splom
#' @importFrom stats cor
#' @importFrom grDevices colorRampPalette
#' 
#' @param df (data.frame) data frame whose columns are plotted against each other
#' @param aspect (numeric) aspect ratio between height and width, default = 1.
#' @param between (list) list with two named vectors x and y indicating space between panels
#' @param col_palette (list) list of colors to use for colorRampPalette
#' @param scales (numeric or list) only numeric seems to work, than it controls the 
#'   number of ticks and tick labels to draw at axes
#' @param strip (list) controlling strip appearance, defaults to NULL.
#' @param groups (character) a grouping variable (untested feature)
#' @param main (character) title of the plot
#' @param xlab (character) label of X axis
#' @param ylab (character) label of y axis
#' @param pch (numeric) the plotting symbol to be used
#' @param col (character) the color to be used
#' @param cex (numeric) character size of the symbol
#' @param ... other arguments passed to the function
#' 
#' #' @examples
#' library(lattice)
#' data(mtcars)
#' 
#' # Draw a scatterplot matrix of all variables of a 
#' # data frame against each other.
#' custom_splom(mtcars[1:5])
#' 
#' # We can customize the scatterplot
#' custom_splom(
#'   mtcars[1:5],
#'   col_palette = rainbow(4),
#'   scales = 10, 
#'   xlab = "data points", ylab = "regression",
#'   pch = 1, col = 1, cex = 0.6
#' )
#' 
#' @export
# ------------------------------------------------------------------------------
custom_splom <- function(df, ...,
  aspect = 1, between = list(x = 0.5, y = 0.5),
  col_palette = c("steelblue", grey(0.95), "darkorange"),
  scales = 4, strip = NULL,
  groups = NULL, main = NULL,
  xlab = NULL, ylab = NULL,
  pch = 19, col = grey(0.5), cex = 0.8
) {

  splom(~ df,
    par.settings = custom.lattice,
    aspect = aspect, between = between,
    pscales = scales, strip = strip,
    groups = groups, main = main,
    xlab = xlab, ylab = ylab,
    pch = pch, col = col, cex = cex,
    lower.panel = panel.splom,
    upper.panel = function(x, y, ...) {
      common <- which(!is.na(x*y))
      x = x[common]; y = y[common]
      palette = grDevices::colorRampPalette(col_palette)(11)
      panel.fill(col = palette[1+abs(round(stats::cor(x, y)*10))])
      panel.lmline(x, y, fontfamily = "FreeSans")
      cpl <- current.panel.limits()
      panel.text(mean(cpl$xlim), mean(cpl$ylim), round(cor(x, y), 2), font=2)
    }
  )
  
}