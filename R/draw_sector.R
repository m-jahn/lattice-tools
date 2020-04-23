#' @importFrom grid grid.polygon
#' @importFrom grid grid.lines
#' @importFrom grid grid.text
#' @importFrom grid gpar
#' @importFrom stats median
#
# generalized function to draw sector polygon from boundary input
# ----------------------------------------------------------------------------
draw_sector <- function(
  lower_bound, upper_bound,
  diameter_inner, diameter_sector,
  label, col, border, lty, lwd, cex
) {
  # compute_sector from lower and upper bounds and diameter arguments
  segment <- c(lower_bound, upper_bound) * 2 * pi
  z <- seq(segment[1], segment[2], by = pi/400)
  diam <- diameter_inner + diameter_sector
  xx <- c(diameter_inner * cos(z), rev(diam * cos(z)))
  yy <- c(diameter_inner * sin(z), rev(diam * sin(z)))
  
  grid::grid.polygon(
    xx+0.5,
    yy+0.5,
    default = "native",
    gp = grid::gpar(fill = col, col = border, lty = lty, lwd = lwd)
  )
  
  # draw sector labels 
  sinz <- sin(median(z))
  cosz <- cos(median(z))
  
  # draw label lines
  grid::grid.lines(
    x = c(diam * cosz, (diam+0.1) * cosz)+0.5,
    y = c(diam * sinz, (diam+0.1) * sinz)+0.5,
    default.units = "native",
    gp = grid::gpar(col = border, lwd = lwd, lty = lty)
  )
  
  #draw label text
  grid::grid.text(
   label = label,
   x = ((diam+0.13) * cosz)+0.5,
   y = ((diam+0.13) * sinz)+0.5,
   just = "center",
   default.units = "native",
   gp = grid::gpar(cex = cex, col = border)
  )
}