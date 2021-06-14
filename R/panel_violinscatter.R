#' Combine violin plot and scatter plot
#'
#' Custom lattice panel function that combines violin and scatter plot to a 
#' violin-shaped scatter plot. By default, the underlying violin is plotted 
#' with the generic `panel.violin` function. On top of that are the points drawn
#' from which the violin was calculated. To achieve this, each point is jittered
#' randomly on the X (Y) axis according to the density estimate of the Y (X) axis.
#' 
#' @param x,y (numeric, factor) X and Y variables passed to \code{panel.xyplot()}.
#' @param groups grouping variable passed down from xyplot (does not need to be specified).
#' @param subscripts subscripts passed down from xyplot (does not need to be specified).
#' @param box.ratio (numeric) ratio of the thickness of each violin and inter violin space.
#' @param box.width (numeric) thickness of the violins in absolute units; overrides box.ratio.
#'   See `panel.violin` for details.
#' @param horizontal (logical) If FALSE, the plot is ‘transposed’ in the sense that the behaviours
#'   of x and y are switched. See documentation of `bwplot` for a full explanation.
#' @param col,alpha,border,lty,lwd,varwidth graphical parameters controlling the violin.
#'   Defaults are taken from the "plot.polygon" settings.
#' @param bw,adjust,kernel,window,width,n,from,to,cut,na.rm arguments to `density`, passed on as appropriate.
#' @param identifier A character string that is prepended to the names of grobs that are created by this panel function.
#' @param group.number internal parameter only used when function is called as panel.groups
#'   argument from within panel.superpose. Does not need to be specified manually.
#' @param ... other arguments passed to the function
#' @export
#' 
#' @importFrom lattice panel.xyplot
#' @importFrom lattice panel.superpose
#' @importFrom lattice trellis.par.get
#' @importFrom stats density
#' @importFrom stats runif
#' @importFrom grid grid.polygon
#' @importFrom grid viewport
#' @importFrom grid pushViewport
#' @importFrom grid popViewport
#' @importFrom grid gpar
#' 
#' @examples
#' # use singer data from lattice
#' library(lattice)
#' data(singer)
#' singer$voice.part <- gsub(" [12]", "", as.character(singer$voice.part))
#' singer$voice.part <- factor(singer$voice.part, c("Soprano", "Alto", "Tenor", "Bass"))
#' 
#' # example with grouping
#' xyplot(height ~ voice.part, singer, groups = voice.part,
#'   horizontal = FALSE, pch = 19,
#'   panel = function(x, y, ...) {
#'     panel.violinscatter(x, y, ...)
#' })
#' 
#' # same plot but horizontal orientation
#' xyplot(voice.part ~ height, singer, groups = voice.part,
#'   horizontal = TRUE, pch = 19,
#'   panel = function(x, y, ...) {
#'     panel.violinscatter(x, y, ...)
#' })
#' 
#' # example with more and non-discrete data points
#' df <- data.frame(
#'   sample = factor(rep(c("A", "B", "C"), each = 300)),
#'   variable = c(rnorm(300, 0, 3), rnorm(300, 1, 2), rnorm(300, 3, 3))
#' )
#' 
#' xyplot(variable ~ sample, df,
#'   horizontal = FALSE, pch = 19, cex = 0.4,
#'   panel = function(x, y, ...) {
#'     panel.violinscatter(x, y, ...)
#' })
panel.violinscatter <- function(x, y,
  groups = NULL, subscripts = NULL,
  box.ratio = 1, box.width = box.ratio/(1 + box.ratio), 
  horizontal = TRUE, col = NULL,
  alpha = 0.3, border = "white",
  lty = NULL, lwd = NULL,
  varwidth = FALSE, bw = NULL, 
  adjust = NULL, kernel = NULL, 
  window = NULL, width = NULL, 
  n = 128, from = NULL, to = NULL, 
  cut = NULL, na.rm = TRUE,
  identifier = "violinscatter",
  group.number = NULL, ...
) {
  # if groups are specified first call panel.superpose for each group,
  # otherwise plot directly
  if (!is.null(groups)) {
    
    # obtain graphical parameters from default style
    plot.polygon <- lattice::trellis.par.get("plot.polygon")
    if (is.null(alpha)) alpha <- plot.polygon$alpha
    if (is.null(border)) border <- plot.polygon$border
    if (is.null(lty)) lty <- plot.polygon$lty
    if (is.null(lwd)) lwd <- plot.polygon$lwd
    
    panel.superpose(x = x, y = y,
      groups = groups, subscripts = subscripts,
      panel.groups = panel.violinscatter, 
      box.ratio = box.ratio, box.width = box.width, 
      horizontal = horizontal,
      alpha = alpha, border = border,
      lty = lty, lwd = lwd,
      varwidth = varwidth, bw = bw, 
      adjust = adjust, kernel = kernel, 
      window = window, width = width, 
      n = n, from = from, to = to, 
      cut = cut, na.rm = na.rm,
      identifier = identifier, ...
    )
    
  } else {
  
    if (all(is.na(x) | is.na(y))) 
      return()
    x <- as.numeric(x)
    y <- as.numeric(y)
    
    # determine graphical parameters if not passed
    if (is.null(col)) {
      col <- lattice::trellis.par.get("superpose.symbol")$col
    } else if (col == "black") {
      col <- lattice::trellis.par.get("superpose.symbol")$col
    }
    if (is.null(group.number)) {
      col <- col[1]
      group.number = 0
    } else {
      col <- col[group.number]
    }
    
    # parameters for kernel density function
    # this is directly taken from lattice' original panel.violin()
    darg <- list()
    darg$bw <- bw
    darg$adjust <- adjust
    darg$kernel <- kernel
    darg$window <- window
    darg$width <- width
    darg$n <- n
    darg$from <- from
    darg$to <- to
    darg$cut <- cut
    darg$na.rm <- na.rm
    my.density <- function(x) {
      ans <- try(do.call(stats::density, c(list(x = x), darg)), 
        silent = TRUE)
      if (inherits(ans, "try-error")) 
        list(x = rep(x[1], 3), y = c(0, 1, 0))
      else ans
    }
    numeric.list <- if (horizontal) 
      split(x, factor(y))
    else split(y, factor(x))
    levels.fos <- as.numeric(names(numeric.list))
    d.list <- lapply(numeric.list, my.density)
    dx.list <- lapply(d.list, "[[", "x")
    dy.list <- lapply(d.list, "[[", "y")
    max.d <- sapply(dy.list, max)
    if (varwidth) 
      max.d[] <- max(max.d)
    cur.limits <- current.panel.limits()
    xscale <- cur.limits$xlim
    yscale <- cur.limits$ylim
    height <- box.width
    
    # violin shaped scatterplot
    # -------------------------
    # function to find nearest neighbor of one point in
    # kernel density vector
    nearest_neighbor <- function(x, y){
      which.min(abs(x-y))
    }
    # calculate jitter for x/y based on density
    jitter_data <- function(x, y){
      ave(y, x, FUN = function(ys) {
        kdens <- my.density(ys)
        pos <- sapply(FUN = nearest_neighbor, ys, kdens$x)
        jitter <- kdens$y[pos]
        jitter <- jitter/max(jitter)
        mapply(FUN = stats::runif, 1, jitter*-0.5*box.width, jitter*0.5*box.width,
          SIMPLIFY = TRUE) 
      })
    }
    
    if (horizontal) {
      for (i in seq_along(levels.fos)) {
        if (is.finite(max.d[i])) {
          grid::pushViewport(grid::viewport(y = unit(levels.fos[i],
           "native"), height = unit(height, "native"),
           yscale = c(max.d[i] * c(-1, 1)), xscale = xscale))
          grid::grid.polygon(x = c(dx.list[[i]], rev(dx.list[[i]])),
           y = c(dy.list[[i]], -rev(dy.list[[i]])), default.units = "native",
           name = lattice::trellis.grobname(identifier, type = "panel", group = group.number),
           gp = grid::gpar(fill = col, col = border, lty = lty, lwd = lwd, alpha = alpha))
          grid::popViewport()
        }
      }
      # violin-shaped scatter plot
      lattice::panel.xyplot(x, y + jitter_data(y, x), ...)
    }
    else {
      # violin plot
      for (i in seq_along(levels.fos)) {
        if (is.finite(max.d[i])) {
          grid::pushViewport(grid::viewport(x = unit(levels.fos[i],
            "native"), width = unit(height, "native"),
            xscale = c(max.d[i] * c(-1, 1)), yscale = yscale))
          grid::grid.polygon(y = c(dx.list[[i]], rev(dx.list[[i]])),
            x = c(dy.list[[i]], -rev(dy.list[[i]])), default.units = "native",
            name = lattice::trellis.grobname(identifier, type = "panel", group = group.number),
            gp = grid::gpar(fill = col, col = border, lty = lty, lwd = lwd, alpha = alpha))
          grid::popViewport()
        }
      }
      # violin-shaped scatter plot
      lattice::panel.xyplot(x + jitter_data(x, y), y, ...)
    }
    invisible()
  }
}