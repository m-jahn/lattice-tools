#' Get bounding boxes for repelled text labels 
#' 
#' This function uses \code{repel_boxes()} from the 'ggrepel' package to obtain coordinates
#' and bounding boxes for repelled text labels. Internally, all units are converted to
#' relative plot coordinates ('npc', between 0 and 1) and before evoking the C++ function
#' \code{repel_boxes()} to carry out the repelling placement. This function is only intended
#' for internal use, but also works for any type of grid or base graphics plot, as long
#' as a graphics device is open, see examples.
#' 
#' @importFrom graphics strheight
#' @importFrom graphics strwidth
#' @importFrom Rcpp evalCpp
#' @importFrom Rcpp sourceCpp
#' @useDynLib latticetools
#' 
#' @param x,y (numeric) vectors representing x and y coordinates of points
#' @param labels (character) vector of labels
#' @param cex (numeric) size of text labels
#' @param xlim numeric vector representing the limits on the x axis like
#'   \code{c(xmin, xmax)}
#' @param ylim numeric vector representing the limits on the y axis like
#'   \code{c(ymin, ymax)}
#'  ‘force_push’ ‘force_pull’ ‘transform_coord’
#' @param point_size (numeric) approximate size of points in npc relative cooordinates. Can be small,
#'   is used in conjunction with "point_padding" options
#' @param box_padding_x (numeric) extra margin around text bounding boxes
#' @param box_padding_y (numeric) extra margin around text bounding boxes
#' @param point_padding_x (numeric) extra margin around data points
#' @param point_padding_y (numeric) extra margin around data points
#' @param force_push (numeric) magnitude of the push force (defaults to \code{1e-5})
#' @param force_pull (numeric) magnitude of the pull force (defaults to \code{1e-5})
#' @param transform_coord (character) the coordinate system used for calculating
#'   bounding boxes. The default, "npc", is to transfrom all cordinates to
#'   'Normalised Parent Coordinates'. When calling externally, use "native"
#'   instead, see examples.
#' @param ... other arguments passed to the function
#' 
#' @examples
#' data("mtcars")
#' mtcars$car <- rownames(mtcars)
#' 
#' # with base graphics
#' # ---------------------------
#' plot(mtcars$mpg ~ mtcars$wt, pch = 19)
#' 
#' coords <- get_repelled_labels(
#'   x = mtcars$wt, y = mtcars$mpg, 
#'   labels = mtcars$car,
#'   cex = 0.7, point_size = 0.01,
#'   xlim = range(mtcars$wt),
#'   ylim = range(mtcars$mpg),
#'   box_padding_x = 0.05,
#'   box_padding_y = 0.05,
#'   point_padding_x = 0.01,
#'   point_padding_y = 0.2,
#'   force_push = 1e-05,
#'   force_pull = 1e-05,
#'   transform_coord = "native"
#' )
#' 
#' with(coords, rect(x1_box, y1_box, x2_box, y2_box, col = "white"))
#' with(coords, text(x, y, labels = label, cex = 0.7))
#' 
#' # with ggplot2
#' # ---------------------------
#' 
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(y = mpg, x = wt)) +
#'   geom_point() +
#'   geom_label(aes(x = coords$x, y = coords$y, label = coords$label))
#' }
#' 
#' @export
get_repelled_labels <- function(
  x, y, labels, cex = 1,
  xlim = c(0, 1),
  ylim = c(0, 1),
  point_size = 0.001,
  box_padding_x = 0.005,
  box_padding_y = 0.005,
  point_padding_x = 0.01,
  point_padding_y = 0.01,
  force_push = 1e-05,
  force_pull = 1e-05,
  transform_coord = "npc",
  ...
) {
  # set unit for string dimensions
  str_unit <- ifelse(transform_coord == "npc", "inches", "user")
  
  # functions to convert units
  convert_x <- function(x, from = "inches", to = transform_coord) {
    if (from == "user") from = "native"
    grid::convertX(grid::unit(x, from), unitTo = to, valueOnly = TRUE)
  }
  convert_y <- function(y, from = "inches", to = transform_coord) {
    if (from == "user") from = "native"
    grid::convertY(grid::unit(y, from), unitTo = to, valueOnly = TRUE)
  }
  
  # convert all input units to desired coord system; default: "npc" (relative coordinates)
  x = convert_x(x, from = "native")
  y = convert_y(y, from = "native")
  
  # calculate extra margin around text for the bounding boxes
  box_margin_x <- convert_x(strwidth("M", str_unit, cex), from = str_unit)
  box_margin_y <- convert_y(strheight("M", str_unit, cex), from = str_unit)
  
  # calculate bounding box of text labels
  box_x = sapply(labels, function(i) {
    convert_x(strwidth(i, str_unit, cex), from = str_unit) + box_margin_x
  })
  box_y = sapply(labels, function(i) {
    convert_y(strheight(i, str_unit, cex), from = str_unit) + box_margin_y
  })
  
  # x and y positions as dataframe
  posdf <- data.frame(x = x, y = y)
  
  # bounding box coordinates for each label
  boxdf <- data.frame(
    x1 = x - box_x / 2 - box_padding_x,
    y1 = y - box_y / 2 - box_padding_y,
    x2 = x + box_x / 2 + box_padding_x,
    y2 = y + box_y / 2 + box_padding_y
  )
  
  moved <- latticetools::repel_boxes(
    data_points = as.matrix(posdf),
    point_size = rep(point_size, nrow(posdf)),
    point_padding_x = point_padding_x,
    point_padding_y = point_padding_y,
    boxes = as.matrix(boxdf),
    xlim = xlim, ylim = ylim,
    hjust = rep(0, nrow(posdf)),
    vjust = rep(0, nrow(posdf)),
    force_push = force_push,
    force_pull = force_pull,
    max_time = 3, 
    max_overlaps = 10, 
    max_iter = 100000,
    direction = "both", 
    verbose = FALSE
  )
  
  coords <- cbind(posdf, moved)
  names(coords) <- c("x_orig", "y_orig", "x", "y", "overlapping")
  coords$x1_box <- coords$x - box_x / 2
  coords$y1_box <- coords$y - box_y / 2
  coords$x2_box <- coords$x + box_x / 2
  coords$y2_box <- coords$y + box_y / 2
  
  # convert units back to native
  coords[c("x_orig", "x", "x1_box", "x2_box")] <- apply(
    coords[c("x_orig", "x", "x1_box", "x2_box")], 2, function(x) {
      convert_x(x, from = transform_coord, to = "native")
  })
  coords[c("y_orig", "y", "y1_box", "y2_box")] <- apply(
    coords[c("y_orig", "y", "y1_box", "y2_box")], 2, function(y) {
      convert_y(y, from = transform_coord, to = "native")
  })
  
  # add labels
  coords$label <- labels
  
  # return coordinates
  return(coords)
}
