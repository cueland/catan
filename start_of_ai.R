dice_roll <- function() {
  x <- sample(1:6, 1)
  y <- sample(1:6, 1)
  return(c(x,y,x+y))
}

# initialize
x <- matrix(ncol = 3)
runs <- 100
for (n in 1:runs) {
  x <- rbind(x, dice_roll())
}
hist(x[,3], breaks = c(1:12)+.5, col = rep(c("#f5f5f5", "#c5c5c5", "#ff7383", "#00b5b5", "#ff7383", "#c5c5c5", "#f5f5f5"), times = c(3,1,1,1,1,1,3)))


#' priorities for selecting first pieces
#' 
#' 1. Best probability
#' 2. Diverse resources
#' 3. How many turns until your next pueblo?
#'    What are the next best options?
#' 4. What are the nearby resources?
#'    Will people be able to block you?
#' 5. Diversity of numbers