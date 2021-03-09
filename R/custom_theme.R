#' Custom ggplot2 like theme
#' 
#' Custom theme for lattice plots. The function takes no arguments.
#' 
#' @import colorspace
#' @import lattice
#' @import latticeExtra
#' @examples
#' library(lattice)
#' data(mtcars)
#' 
#' # colors are more subtle than default lattice theme
#' xyplot(mpg ~ factor(cyl) | gear, mtcars,
#'   groups = cyl, auto.key = list(columns = 3),
#'   par.settings = custom.ggplot()
#' )
#' @export
# ------------------------------------------------------------------------------
custom.ggplot <- function() {
  theme <- latticeExtra::ggplot2like()
  theme$axis.line$col <- "white"
  theme$axis.line$lwd <- 2
  theme$strip.border$col <- "white"
  theme$strip.border$lwd <- 2
  theme
}


#' Custom grey lattice theme
#' 
#' Custom theme for lattice plots. The function takes no arguments.
#' 
#' @examples
#' library(lattice)
#' data(mtcars)
#' 
#' # colors are more subtle than default lattice theme
#' xyplot(mpg ~ factor(cyl) | gear, mtcars,
#'   groups = cyl, auto.key = list(columns = 3),
#'   par.settings = custom.lattice()
#' )
#' @export
# ------------------------------------------------------------------------------
custom.lattice <- function() {
  default_cols <- c("#00526D", colorspace::rainbow_hcl(n = 6, c = 90, l = 50))
  theme <- latticeExtra::custom.theme(
    symbol = default_cols,
    fill = default_cols,
    region = default_cols,
    reference = 1, bg = 0, fg = 1,
    pch = rep(19,7))
  theme$superpose.symbol$fill <- default_cols
  theme$strip.background$col <- c(grey(0.95), grey(0.85))
  theme$strip.shingle$col <- c(grey(0.75), grey(0.65))
  theme$superpose.line$lwd <- 1.5
  theme$reference.line$col <- default_cols[1]
  theme$add.line$col <- default_cols[1]
  theme$add.text$cex <- 0.8
  theme$par.main.text$cex <- 1
  theme$par.main.text$font <- 1
  theme$box.umbrella$lty <- 1
  theme$par.xlab.text$cex = 0.8
  theme$par.ylab.text$cex = 0.8
  theme$par.zlab.text$cex = 0.8
  theme
}

#' Colorblind-safe grey lattice theme
#' 
#' Custom theme for lattice plots. The function takes no arguments.
#' Colorblind-safe color scale with 7 colors was adapted from R color brewer, 
#' see \code{RColorBrewer::brewer.pal(8, "Dark2"))}. Color blind safe colors are
#' distinguishable for most common types of color blindness (deuterotopia, 
#' deuteroanomaly), yet still look good for non color blind people.
#' 
#' @examples
#' library(lattice)
#' data(mtcars)
#' 
#' xyplot(mpg ~ factor(carb) | gear, mtcars,
#'   groups = carb, auto.key = list(columns = 3),
#'   par.settings = custom.colorblind()
#' )
#' @export
# ------------------------------------------------------------------------------
custom.colorblind <- function() {
  default_cols <- c("#E7298A", "#66A61E", "#E6AB02", "#7570B3", "#666666", "#1B9E77", "#D95F02", "#A6761D")
  theme <- latticeExtra::custom.theme(
    symbol = default_cols,
    fill = default_cols,
    region = default_cols,
    reference = 1, bg = 0, fg = 1,
    pch = rep(19,7))
  theme$superpose.symbol$fill <- default_cols
  theme$strip.background$col <- c(grey(0.95), grey(0.85))
  theme$strip.shingle$col <- c(grey(0.75), grey(0.65))
  theme$superpose.line$lwd <- 1.5
  theme$reference.line$col <- default_cols[1]
  theme$add.line$col <- default_cols[1]
  theme$add.text$cex <- 0.8
  theme$par.main.text$cex <- 1
  theme$par.main.text$font <- 1
  theme$box.umbrella$lty <- 1
  theme$par.xlab.text$cex = 0.8
  theme$par.ylab.text$cex = 0.8
  theme$par.zlab.text$cex = 0.8
  theme
}