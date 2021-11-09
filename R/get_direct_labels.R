get_direct_labels <- function(
  x, y, labels,
  label_groups,
  cex,
  method, ...
) {
  
  # convert user coordinate units to cm (see apply.method manual)
  x_cm <- grid::convertX(grid::unit(x, "native"), unitTo = "cm", valueOnly = TRUE)
  y_cm <- grid::convertY(grid::unit(y, "native"), unitTo = "cm", valueOnly = TRUE)
  
  # apply label placing algorithm
  coords <- directlabels::apply.method(
    method, d = data.frame(
      x = x_cm, y = y_cm, 
      x_orig = x_cm, y_orig = y_cm,
      groups = label_groups,
      label = labels,
      cex = rep(cex, length(x)), 
      index = 1:length(x)
    )
  )
  
  # sort not by length of character labels, but by original order
  coords <- coords[order(coords$index), ]
  
  # convert back to native units
  coords[c("x", "x_orig", "w", "right", "left")] <-
    apply(coords[c("x", "x_orig", "w", "right", "left")], 2, function(x) {
      grid::convertX(grid::unit(x, "cm"), unitTo = "native", valueOnly = TRUE)
    })
  
  coords[c("y", "y_orig", "h", "top", "bottom")] <-
    apply(coords[c("y", "y_orig", "h", "top", "bottom")], 2, function(x) {
      grid::convertY(grid::unit(x, "cm"), unitTo = "native", valueOnly = TRUE)
    })
  
  coords$x1_box <- with(coords, left + x - x_orig)
  coords$y1_box <- with(coords, bottom + y - y_orig)
  coords$x2_box <- with(coords, right + x - x_orig)
  coords$y2_box <- with(coords, top + y - y_orig)
  
  # return coordinates
  return(coords)
}