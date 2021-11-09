#' Get repelled label bounding boxes
#' 
#' The function uses ggrepel to obtaine repelled bounding boxes
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
#' @param point_size (numeric) approximate size of points in npc relative cooordinates. Can be small,
#'   is used in conjunction with "point_padding" options
#' @param box_padding_x (numeric) extra margin around text bounding boxes
#' @param box_padding_y (numeric) extra margin around text bounding boxes
#' @param point_padding_x (numeric) extra margin around data points
#' @param point_padding_y (numeric) extra margin around data points
#' @param ... other arguments passed to the function
#' 
#' @export
get_repelled_labels <- function(
  x, y, labels, cex,
  point_size,
  box_padding_x,
  box_padding_y,
  point_padding_x,
  point_padding_y, ...
) {
  
  # functions to convert units
  convert_x <- function(x, from = "inches", to = "npc") {
    grid::convertX(grid::unit(x, from), unitTo = to, valueOnly = TRUE)}
  convert_y <- function(y, from = "inches", to = "npc") {
    grid::convertY(grid::unit(y, from), unitTo = to, valueOnly = TRUE)}
  
  # convert all input units to "npc" (relative coordinates)
  x = convert_x(x, from = "native")
  y = convert_y(y, from = "native")
  
  # calculate extra margin around text for the bounding boxes
  box_margin <- strheight("Test", "inches", cex)
  box_margin_x <- convert_x(box_margin, from = "inches")
  box_margin_y <- convert_y(box_margin, from = "inches")
  
  # calculate bounding box of text labels
  box_x = sapply(labels, function(i) {
    convert_x(strwidth(i, "inches", cex), from = "inches") + box_margin_x
  })
  box_y = sapply(labels, function(i) {
    convert_y(strheight(i, "inches", cex), from = "inches") + box_margin_y
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
    xlim = c(0, 1), ylim = c(0, 1),
    hjust = rep(0, nrow(posdf)),
    vjust = rep(0, nrow(posdf)),
    force_push = 1e-05,
    force_pull = 1e-05,
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
      convert_x(x, from = "npc", to = "native")
  })
  coords[c("y_orig", "y", "y1_box", "y2_box")] <- apply(
    coords[c("y_orig", "y", "y1_box", "y2_box")], 2, function(y) {
      convert_y(y, from = "npc", to = "native")
  })
  
  # add labels
  coords$label <- labels
  
  # return coordinates
  return(coords)
}