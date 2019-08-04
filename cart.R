#' Calculate Adjacent Tiles (calc_adj)
#'
#' This function calculates the two adjacent tiles for a given corner
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords 
#' @export
#' @examples
#' calc_adj(ax, hor, corner)
#' 

# Function to calculate the x,y cartesian coordinates from the hex coords and corner
cart <- function(ax, hor, corner) {
  if (corner == 0) {
    return(c(ax*sqrt(3)-0.5*hor*sqrt(3), 1.5*hor))
    y <- 2*hor
  }
  else {
    return(c(ax*sqrt(3)-0.5*hor*sqrt(3) + cos(corner*pi/3-pi/6), 1.5*hor+sin(corner*pi/3-pi/6)))
  }
}

# function to output an x,y array of coords for the corners of a polygon
corns <- function(x) {
  return(data.frame(xx = (x[1]*sqrt(3)-0.5*x[2]*sqrt(3))+sqrt(3)/2*c(1,0,-1,-1,0,1),
                    yy = 1.5*x[2]+0.5*c(1,2,1,-1,-2,-1)))
}

corns(c(0,0))

#function to find x,y of the center of a hex
centfind <- function(x) {
  # x is an array (ax,hor) of the hexagon in question
  return(c(x[1]*sqrt(3)-0.5*x[2]*sqrt(3), 1.5*x[2]))
}

#function to find x,y of a corner
cornfind <- function(x) {
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
  center_x <- x[1]*sqrt(3)-0.5*x[2]*sqrt(3)
  center_y <- 1.5*x[2]
  
  # return the center coords + adjustments for the corner
  return(c(center_x + sqrt(3)/2*c(0,1,0,-1,-1,0,1)[x[3]+1],
           center_y + 0.5*c(0,1,2,1,-1,-2,-1)[x[3]+1]))
}

cornfind(c(1,1,3,4,5,5))
