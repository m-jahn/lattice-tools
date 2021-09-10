#' Calculate and draw p-values in lattice plots
#' 
#' This panel function allows to overlay p-values obtained from a statistical
#' significance test on plots, together with "significance" symbols such 
#' as stars as it is often in encountered in the scientific literature. It supports
#' Student's t-test and Wilcoxon rank sum test (also called Mann-Whitney U-test
#' for two-sample comparisons). The Wilcoxon test is used for non-parametric (non
#' normally distributed data). Different input options for the tests can be specified.
#' 
#' @import grid
#' @import lattice
#' @import latticeExtra
#' @importFrom grDevices grey
#' @importFrom methods new
#' 
#' @param x,y (numeric, character) variables to be plotted. The x variable 
#'   is treated as a grouping variable, i.e. p-values are calculated between groups
#'   of unique x values.
#' @param method (character) "t.test" for Student's t-test, or "wilcox.test" for Wilcoxon
#'   ranked sum test. The latter is also called Mann-Whitney U-test for two-sample comparisons (the default).
#' @param alternative (character) passed to t.test(), one of "two.sided", "less", or "greater"
#' @param paired (logical) if the different groups of y values are paired (default: FALSE)
#' @param two_sample (logical) if TRUE a two-sample is made, else a one-sample comparison (default: TRUE)
#' @param mu (numeric) a number indicating the true value of the mean (or difference in 
#'   means if you are performing a two sample test). Passed to the test indicated by 'method'
#' @param std (character, numeric) the value of x that is used as standard. If NULL, the 
#'   first value is used as standard. If a numeric scalar i, std is determined 
#'   as the i'th element of unique(x). If character, it must match one of x. If 
#'   std = 'all_values', each unique group of x is compared to the total population.
#'   Ignored for one sample tests, that are compared against 'mu'
#' @param symbol (logical) if '*' symbols should be drawn for significance
#' @param pvalue (logical) if numeric p-values should be drawn for significance
#' @param pval_digits (numeric) how many p-value digits should be printed (default is 2)
#' @param cex (numeric) character size of the symbol
#' @param offset (numeric) offset in native plot units used to adjust the position of text labels.
#'   If offset is length 1, only the vertical position is adjusted; if it is length 2 the horizontal 
#'   and vertical position is adjusted
#' @param fixed_pos (numeric) fixed vertical position of the p-value, 
#'   if NULL determined from the data (the default)
#' @param verbose (logical) if a summary of the p-value calculation should be 
#'   printed to the terminal
#' @param col (character) the color to be used
#' @param ... other arguments passed to the function
#' 
#' @examples
#' library(lattice)
#' data(mtcars)
#' 
#' # p-values are calculated between groups of x, grouping is ignored
#' xyplot(mpg ~ factor(cyl), mtcars, groups = cyl, pch = 19, cex = 0.7,
#'   panel = function(x, y, ...) {
#'     panel.stripplot(x, y, jitter.data = TRUE, 
#'       horizontal = FALSE, amount = 0.15, ...)
#'     panel.pvalue(x, y, std = 1, symbol = TRUE, pvalue = TRUE)
#' })
#' 
#' @export
# ------------------------------------------------------------------------------
panel.pvalue <- function(x, y,
  method = "t.test",
  alternative = "two.sided",
  paired = FALSE,
  two_sample = TRUE,
  mu = 0,
  std = NULL,
  symbol = TRUE, pvalue = FALSE, 
  pval_digits = 2,
  cex = 0.8, offset = NULL, 
  fixed_pos = NULL, verbose = FALSE, 
  col = trellis.par.get()$plot.symbol$col, ...
  )
{ 
  # if x is a factor, reorder x and y based on factor levels
  if (is.factor(x)) {
    ord <- order(as.numeric(x))
    x <- x[ord]; y <- y[ord]
  }
  
  # two-sample test
  # ---------------
  if (two_sample) {
    # if no standard is passed to function, just take the first unique x
    if (is.null(std)) {
      std <- 1
    } else if (is.numeric(std)) {
      stopifnot(std %in% seq_along(unique(x)))
    } else if (std == "all_values") {
      std <- seq_along(unique(x))
    } else if (is.character(std)) {
      stopifnot(std %in% x)
      std <- which(unique(x) %in% std) 
    }
    std_val <- unique(x)[std]
    
    # calculate p-value between all x-variable groups and standard
    pval <- tapply(y, x, function(y_test) {
      result <- do.call(method, list(
        x = y_test, y = y[x == std_val], 
        alternative = alternative, 
        paired = paired,
        mu = mu)
      )
      result$p.value
    })
  } else {
  # one-sample test
  # ---------------
    # calculate p-value between all x-variable groups and mu
    std_val = "not applicable"
    pval <- tapply(y, x, function(y_test) {
      result <- do.call(method, list(
        x = y_test, alternative = alternative, 
        paired = paired,
        mu = mu)
      )
      result$p.value
    })
  }
  
  # construct and print p-value symbols
  pval_symbol <- sapply(pval, function(x) {
    if (is.na(x)) "" else
    if (x <= 0.001) "***" else
    if (x <= 0.01 & x > 0.001) "**" else
    if (x <= 0.05 & x > 0.01) "*" else ""
  })
    
  # collect all p-value data in a df
  df_pval <- data.frame(row.names = seq_along(unique(x)),
    Var_1 = rep(std_val, length(unique(x))),
    Var_2 = unique(x),
    p_value = pval,
    p_value_text = paste0("\np = ", gsub("e", "x10^", format(pval, nsmall = 3, digits = pval_digits))),
    p_sig = pval_symbol,
    test = rep(paste0(method, ", ", alternative), length(unique(x))),
    two_sample = two_sample,
    paired = paired
  )
  
  # determine offset for X and Y position
  if (is.null(offset)) {
    offset <- c(0, 0)
  } else if (is.numeric(offset)) {
    if (length(offset) == 1) offset <- c(0, offset) else
    if (length(offset) == 2) offset <- offset
  } else {
    stop("Offset must be NULL or a numeric value of length 1 or 2")
  }
  
  # determine X position of p-value text
  df_pval$x_pos <- seq_along(unique(x))+offset[1]
  
  # determine Y position of p-value text
  if (is.null(fixed_pos)) {
    ypos = tapply(y, x, function(z) {
      s <- max(z, na.rm = TRUE)
      s <- s+abs(s/3)
      replace(s, s > max(y), max(y))
    })
    df_pval$y_pos <- ypos+offset[2]
  } else if (is.numeric(fixed_pos)){
    df_pval$y_pos <- fixed_pos+offset[2]
  } else {
    stop("fixed_pos must be NULL for automatic y positioning, or a numeric value.")
  }
  
  # optionally print summary table
  if (verbose) {
    print(df_pval)
  }
  
  # finally print p-value text labels
  with(df_pval[-std, ], {
    if (symbol) {
      panel.text(x_pos, y_pos, labels = p_sig,
        cex = 2*cex, pos = 1, col = col, ...)
    }
    if (pvalue) {
      panel.text(x_pos, y_pos, labels = p_value_text,
        cex = cex, pos = 1, col = col, ...)
    }
  })
  
}
