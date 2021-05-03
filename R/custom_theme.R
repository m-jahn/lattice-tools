#' Custom ggplot2 like theme
#' 
#' Custom theme for lattice plots. Graphical parameters are passed down
#' to `ggplot2like()`, which passes them down to `simpleTheme()`.
#' 
#' @param ... graphical parameters passed down to `ggplot2like()` and `simpleTheme()`
#' 
#' @import colorspace
#' @import lattice
#' @import latticeExtra
#' @examples
#' library(lattice)
#' data(mtcars)
#' 
#' xyplot(mpg ~ factor(carb) | gear, mtcars,
#'   groups = carb, auto.key = list(columns = 3),
#'   par.settings = custom.ggplot(),
#'   panel = function(x, y, ...) {
#'     panel.grid(h = -1, v = -1, col = "white")
#'     panel.xyplot(x, y, ...)
#'     panel.lmline(x, y, ...)
#'   }
#' )
#' 
#' @export
custom.ggplot <- function(...) {
  theme <- latticeExtra::ggplot2like(...)
  theme$axis.line$col <- "white"
  theme$axis.line$lwd <- 2
  theme$strip.border$col <- "white"
  theme$strip.border$lwd <- 2
  theme
}

#' Custom grey lattice theme
#' 
#' Custom theme for lattice plots. Some graphical parameters can be passed
#' to override the defaults. Size parameters are relative.
#' 
#' @param col default colors for the theme
#' @param symbol_type symbol(s) for the theme ('pch', defaults to 19)
#' @param symbol_size relative expansion factor (default 1)
#' @param line_type line type(s) for the theme ('lty', defaults to 1)
#' @param line_size relative expansion factor (default 1)
#' @param text_type the font face, a number form 1 to 4 for normal, bold, italic, bold + italic
#' @param text_size relative expansion factor (default 1)
#' 
#' @examples
#' library(lattice)
#' data(mtcars)
#' 
#' xyplot(mpg ~ factor(carb) | gear, mtcars,
#'   groups = carb, auto.key = list(columns = 3),
#'   par.settings = custom.lattice(),
#'   panel = function(x, y, ...) {
#'     panel.grid(h = -1, v = -1, col = grey(0.95))
#'     panel.xyplot(x, y, ...)
#'     panel.lmline(x, y, ...)
#'   }
#' )
#' 
#' @export
custom.lattice <- function(
  col = c("#00526D", colorspace::rainbow_hcl(n = 6, c = 90, l = 50)),
  symbol_type = 19,
  symbol_size = 1,
  line_type = 1,
  line_size = 1,
  text_type = 1,
  text_size = 1
) {
  theme <- latticeExtra::custom.theme(
    symbol = col,
    fill = col,
    region = col,
    reference = 1, bg = 0, fg = 1,
    pch = symbol_type)
  theme$strip.background$col <- c(grey(0.95), grey(0.85))
  theme$strip.shingle$col <- c(grey(0.75), grey(0.65))
  theme$superpose.symbol$fill <- col
  theme$superpose.symbol$cex <- rep(0.8*symbol_size, length(col))
  theme$plot.symbol$cex <- 0.8*symbol_size
  theme$superpose.line$lty <- rep(line_type, length(col))
  theme$superpose.line$lwd <- rep(1.5*line_size, length(col))
  theme$reference.line$col <- col[1]
  theme$reference.line$lty <- line_type
  theme$reference.line$lwd <- 1.5*line_size
  theme$add.line$col <- col[1]
  theme$add.line$lty <- line_type
  theme$add.line$lwd <- 1.5*line_size
  theme$add.text$font <- text_type
  theme$add.text$cex <- 0.8*text_size
  theme$par.main.text$font <- text_type
  theme$par.main.text$cex <- 1*text_size
  theme$box.umbrella$lty <- 1*line_size
  theme$par.xlab.text$cex <- 0.8*text_size
  theme$par.ylab.text$cex <- 0.8*text_size
  theme$par.zlab.text$cex <- 0.8*text_size
  theme
}

#' Colorblind-safe grey lattice theme
#' 
#' Custom theme for lattice plots. 
#' Colorblind-safe color scale with 7 colors was adapted from R color brewer, 
#' see \code{RColorBrewer::brewer.pal(8, "Dark2"))}. Color blind safe colors are
#' distinguishable for most common types of color blindness (deuterotopia, 
#' deuteroanomaly), yet still look good for non color blind people.
#' Some graphical parameters can be passed to override the defaults. 
#' Size parameters are relative.
#' 
#' @param col default colors for the theme
#' @param symbol_type symbol(s) for the theme ('pch', defaults to 19)
#' @param symbol_size relative expansion factor (default 1)
#' @param line_type line type(s) for the theme ('lty', defaults to 1)
#' @param line_size relative expansion factor (default 1)
#' @param text_type the font face, a number form 1 to 4 for normal, bold, italic, bold + italic
#' @param text_size relative expansion factor (default 1)
#' 
#' @examples
#' library(lattice)
#' data(mtcars)
#' 
#' xyplot(mpg ~ factor(carb) | gear, mtcars,
#'   groups = carb, auto.key = list(columns = 3),
#'   par.settings = custom.colorblind(),
#'   panel = function(x, y, ...) {
#'     panel.grid(h = -1, v = -1, col = grey(0.95))
#'     panel.xyplot(x, y, ...)
#'     panel.lmline(x, y, ...)
#'   }
#' )
#' 
#' @export
custom.colorblind <- function(
  col = c("#E7298A", "#66A61E", "#E6AB02", "#7570B3", 
    "#666666", "#1B9E77", "#D95F02", "#A6761D"),
  symbol_type = 19,
  symbol_size = 1,
  line_type = 1,
  line_size = 1,
  text_type = 1,
  text_size = 1
) {
  theme <- latticeExtra::custom.theme(
    symbol = col,
    fill = col,
    region = col,
    reference = 1, bg = 0, fg = 1,
    pch = symbol_type)
  theme$strip.background$col <- c(grey(0.95), grey(0.85))
  theme$strip.shingle$col <- c(grey(0.75), grey(0.65))
  theme$superpose.symbol$fill <- col
  theme$superpose.symbol$cex <- rep(0.8*symbol_size, length(col))
  theme$plot.symbol$cex <- 0.8*symbol_size
  theme$superpose.line$lty <- rep(line_type, length(col))
  theme$superpose.line$lwd <- rep(1.5*line_size, length(col))
  theme$reference.line$col <- col[1]
  theme$reference.line$lty <- line_type
  theme$reference.line$lwd <- 1.5*line_size
  theme$add.line$col <- col[1]
  theme$add.line$lty <- line_type
  theme$add.line$lwd <- 1.5*line_size
  theme$add.text$font <- text_type
  theme$add.text$cex <- 0.8*text_size
  theme$par.main.text$font <- text_type
  theme$par.main.text$cex <- 1*text_size
  theme$box.umbrella$lty <- 1*line_size
  theme$par.xlab.text$cex <- 0.8*text_size
  theme$par.ylab.text$cex <- 0.8*text_size
  theme$par.zlab.text$cex <- 0.8*text_size
  theme
}