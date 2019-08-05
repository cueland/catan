#' Calculate Adjacent Tiles (calc_adj)
#'
#' This function calculates the two adjacent tiles for a given corner
#' @param x A vector with three elements in the format c(ax, hor, corner).
#' @keywords 
#' @export
#' @examples
#' calc_adj(ax, hor, corner)
#' 

calc_adj <- function(x) {
  # axial coordinate, horizontal coordinate, and corner numbered 1-6 starting at upper right corner
  
  # set up the matrices used to calculate the two (a1,a2) adjacent tiles
  a1 <- matrix(c(1,0,3,1,1,4,0,1,5,-1,0,6,-1,-1,1,0,-1,2), ncol = 3, byrow = T)
  a2 <- matrix(c(1,1,5,0,1,6,-1,0,1,-1,-1,2,0,-1,3,1,0,4), ncol = 3, byrow = T)
  
  # calculate a matrix of the 3 axial coordinates with their respective corners
  y <- matrix(c(x,
                x[1:2]+a1[x[3],1:2], a1[x[3],3],
                x[1:2]+a2[x[3],1:2], a2[x[3],3]),ncol=3,byrow=T)
  
  # set the column names for the array
  colnames(y) <- c("ax", "hor", "c")
  
  # order the corner coordinates by the corners, either 1,3,5 or 2,4,6
  y <- data.frame(y[order(y[,3]),])
  
  # reshape the matrix to an array (the reshape is done to create column names)
  y$time <- 1:3
  y$id <- 1
  y <- reshape(y, idvar = "id", timevar = "time", direction = "wide")
  
  # create an unique corner id for the corner that can always identify the three
  # hexes that correspond to the same corner
  y$id <- paste(y[,2:ncol(y)], collapse=",")
  
  return(y)
}