#' Calculate Cartesian Coordinates of Hex Corners (cart_find)
#'
#' This function calculates the x,y coordinates of the center (corner=0), or the
#' corners of a hex
#' @param x A vector with two or three elements in the format c(ax, hor, [corner])
#' @keywords 
#' @export
#' @examples
#' cart_find(x)
#' 

#function to find x,y of a corner
cart_find <- function(x) {
  # x is an array (ax,hor,[corner]) of the hexagon corner in question
  
  # if corner not specified, assume center, i.e. angle 
  if (length(x) > 3 | length(x) < 2) {
    stop(paste0("Input vector length = ", length(x), ". It should be in the format c(ax,hor,[corner])"))
  }
  else if (length(x) == 2) {
    warning("corner not specified, assuming center.")
    x <- c(x,0)
  }
  
  # define the x,y coords of the center
  center_x <- x[1] * sqrt(3) - 0.5 * x[2] * sqrt(3)
  center_y <- 1.5 * x[2]
  
  # use a matrix to define the corners rather than a sin/cos
  # corn_x[1] corresponds to the hex center, then 2 corresponds to corner 1 and so forth
  corn_x <- c(0,1,0,-1,-1,0,1)
  corn_y <- c(0,1,2,1,-1,-2,-1)
  
  # return the center coords + adjustments for the corner
  return(c(center_x + sqrt(3)/2*corn_x[x[3]+1],
           center_y + 0.5*corn_y[x[3]+1]))
  
  # version with sin/cos
  # if (x[3] == 0) {
  #   return(x[1] * sqrt(3) - 0.5 * x[2] * sqrt(3), 1.5 * x[2])
  # }
  # else {
  #   return(x[1] * sqrt(3) - 0.5 * x[2] * sqrt(3) + cos(x[3] * pi/3 - pi/6)
  #          y <- 1.5 * x[2] + sin(x[3] * pi/3 - pi/6)
  # }
}