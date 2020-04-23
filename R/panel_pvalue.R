#' Calculate and draw p-values in lattice plots
#' 
#' Custom panel functions for lattice plots
#' 
#' @import grid
#' @import lattice
#' @import latticeExtra
#' @importFrom grDevices grey
#' @importFrom methods new
#' 
#' @param x,y (numeric, character) variables to be plotted. The x variable 
#'   is treated as a grouping varibale, i.e. p-values are calculated between groups
#'   of unique x values.
#' @param std (character, numeric) the value of x that is used as standard. If NULL, the 
#'   first value is used as standard. If a numeric scalar i, std is determined 
#'   as the i'th element of unique(x). If character, it must be one of x. If 
#'   std = 'all_values', each unique group of x is compared to the total population
#' @param symbol (logical) if '*' symbols should be drawn for significance
#' @param pvalue (logical) if numeric p-values should be drawn for significance
#' @param pval_digits (numeric) how many p-value digits should be printed (default is 2)
#' @param cex (numeric) character size of the symbol
#' @param offset (numeric) offset added to the the vertical position of the p-value
#' @param fixed_pos (numeric) vertical position of the p-value, 
#'   if NULL determined from the data
#' @param verbose (logical) if a summary of the p-value calculation should be 
#'   printed to the terminal
#' @param alternative (character) Passed to t.test(), one of "two.sided", "less", or "greater"
#' @param col (character) the color to be used
#' @param ... other arguments passed to the function
#' 
#' @examples
#' library(lattice)
#' data(mtcars)
#' 
#' # p-values are calculated between groups of x, grouping variable is ignored
#' xyplot(mpg ~ factor(cyl), mtcars, groups = cyl, pch = 19, cex = 0.7,
#'   panel = function(x, y, ...) {
#'     panel.stripplot(x, y, jitter.data = TRUE, 
#'       horizontal = FALSE, amount = 0.15, ...)
#'     panel.pvalue(x, y, std = 1, symbol = TRUE, pvalue = TRUE)
#' })
#' 
#' @export
# ------------------------------------------------------------------------------
panel.pvalue <- function(x, y, std = NULL, 
  symbol = TRUE, pvalue = FALSE, 
  pval_digits = 2,
  cex = 0.8, offset = 1, 
  fixed_pos = NULL, verbose = FALSE, 
  alternative = "two.sided",
  col = trellis.par.get()$plot.symbol$col, ...
  )
{ 
  # if x is a factor, reorder x and y based on factor levels
  if (is.factor(x)) {
    ord <- order(as.numeric(x))
    x <- x[ord]; y <- y[ord]
  }

  # if no standard is passed to function, just take the first unique x
  if (is.null(std)) {
    std = unique(x)[1]
  } else if (is.numeric(std)) {
    std = unique(x)[std]
  } else if (std == "all_values") {
    std = x
  } else if (is.character(std)) {
    stopifnot(std %in% x)
  }
  
  # calculate p-value between all x-variable groups and standard
  pval <- tapply(y, x, function(z) {
    t.test(z, y[x == std], alternative = alternative)$p.value
  })
  if (verbose) {
    cat("p-value for comparison of ", std, "\n with ", 
      as.character(unique(x)), " = ", pval, "\n")
  }
  
  # determine Y position of p-value character
  if (is.null(fixed_pos)) {
    ypos = tapply(y, x, function(z) {
      s <- max(z, na.rm = TRUE)
      s <- s+abs(s/3)
      replace(s, s > max(y), max(y))
    })
    
  } else if (is.numeric(fixed_pos)){
    ypos = fixed_pos
  } else {
    stop("fixed_pos must be NULL for automatic y positioning, or a numeric value.")
  }
  
  # construct and print p-value symbols
  if (symbol) {
    pval_symbol <- sapply(pval, function(x) {
      if (is.na(x)) "" else
      if (x <= 0.001) "***" else
      if (x <= 0.01 & x > 0.001) "**" else
      if (x <= 0.05 & x > 0.01) "*" else ""
    })
    panel.text(1:length(pval), ypos, labels = pval_symbol, 
      cex = 2*cex, pos = 1, offset = offset, col = col, ...)
  }
  
  # construct and print numeric p-values
  if (pvalue) {
    pval = paste0(
      "\n\np = ", gsub("e", "x10^", format(pval, nsmall = 3, digits = pval_digits))
    )
    panel.text(1:length(pval), ypos, labels = pval, 
      cex = cex, pos = 1, offset = offset, col = col, ...)
  }
  
}
