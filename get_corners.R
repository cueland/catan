#' Get (x,y) Coordinates of Hex Corners (get_corners)
#'
#' This function returns a data frame with 6 rows of x,y coordinates for each corner of a hex,
#' starting with the upper right corner (1) and progressing counter-clockwise
#' @param x A vector with two elements in the format c(axx, hor)
#' @keywords
#' @export
#' @examples
#' get_corners(x)
#' 

get_corners <- function(x) {
  return(data.frame(xx = (x[1]*sqrt(3) + 0.5*x[2]*sqrt(3)) + sqrt(3)/2*c(1,0,-1,-1,0,1),
                    yy = 1.5*x[2] + 0.5*c(1,2,1,-1,-2,-1)))
}