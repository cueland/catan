#' Calculate Cartesian Coordinates of Hex Corners (cart_find)
#'
#' This function calculates the x,y coordinates of a particular corner of a hex, if specified.
#' If corner = 0, the function returns the x,y coordinates of the center of the hex.
#' If x is an array with only 2 values, the function returns an array of x,y coordinates of all
#' the corners.
#' 
#' @param x A vector with two or three elements in the format c(axx, hor, [corner]). If corner
#' is omitted, function returns array of x,y coordinates for all corners.
#' @keywords
#' @export
#' @examples
#' cart_find(c(ax,hor,corner))
#' cart_find(c(ax,hor,0))
#' cart_find(c(ax,hor))
#' 

#function to find x,y of a corner
cart <- function(x) {
  # x is an array (axx,hor,[corner]) of the hexagon corner in question
  
  # if corner not specified, return all corners
  if (length(x) > 3 | length(x) < 2) {
    stop(paste0("Input vector length = ", length(x), ". It should be in the format c(axx,hor,[corner])"))
  }
  else if (length(x) == 2) {
    return(data.frame(xx = (x[1]*sqrt(3) + 0.5*x[2]*sqrt(3)) + sqrt(3)/2*c(1,0,-1,-1,0,1),
                      yy = 1.5*x[2] + 0.5*c(1,2,1,-1,-2,-1)))
  }
  else {
    # define the x,y coords of the center
    center_x <- x[1] * sqrt(3) + 0.5 * x[2] * sqrt(3)
    center_y <- 1.5 * x[2]
    
    # use a matrix to define the corners rather than a sin/cos
    # corn_x[1] corresponds to the hex center, then 2 corresponds to corner 1 and so forth
    corn_x <- c(0,1,0,-1,-1,0,1)
    corn_y <- c(0,1,2,1,-1,-2,-1)
    
    # return the center coords + adjustments for the corner
    return(c(center_x + sqrt(3)/2*corn_x[x[3]+1],
             center_y + 0.5*corn_y[x[3]+1]))
  }
  
  # version with sin/cos
  # if (x[3] == 0) {
  #   return(x[1] * sqrt(3) + 0.5 * x[2] * sqrt(3), 1.5 * x[2])
  # }
  # else {
  #   return(x[1] * sqrt(3) + 0.5 * x[2] * sqrt(3) + cos(x[3] * pi/3 - pi/6)
  #          y <- 1.5 * x[2] + sin(x[3] * pi/3 - pi/6)
  # }
}
