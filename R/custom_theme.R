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
  theme <- latticeExtra::custom.theme(
    symbol = c("#00526D", colorspace::rainbow_hcl(n = 6, c = 90, l = 50)),
    fill = c("#00526D", colorspace::rainbow_hcl(n = 6, c = 90, l = 50)),
    region = c("#00526D", colorspace::rainbow_hcl(n = 6, c = 90, l = 50)),
    reference = 1, bg = 0, fg = 1)
  theme$superpose.symbol$fill <- c("#00526D", colorspace::rainbow_hcl(n = 6, c = 90, l = 50))
  theme$strip.background$col <- c(grey(0.95), grey(0.85))
  theme$strip.shingle$col <- c(grey(0.75), grey(0.65))
  theme$superpose.line$lwd <- 1.5
  theme$reference.line$col <- "#00526D"
  theme$add.line$col <- "#00526D"
  theme$add.text$cex <- 0.8
  theme$par.main.text$cex <- 1
  theme$box.umbrella$lty <- 1
  theme$par.main.text$font <- 1
  theme
}
