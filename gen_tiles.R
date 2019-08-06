#' Generate Randomized Catan Tiles
#'
#' This function generates a randomized set of tiles for the catan board.
#' @param ord A vector of length 18 that sets the order of the non-desert tiles
#' @param desert An integer between 1 and 19 for the location of the desert
#' @keywords
#' @export
#' @examples
#' gen_tiles()
#' gen_tiles(sample(1:18), sample(1:19,1))
#' gen_tiles(desert = sample(1:19,1))
#' 

gen_tiles <- function(ord = sample(1:18), desert = sample(1:19,1)) {
  # Input the order of number chits and their corresponding strengths
  chits <- data.frame(lett = LETTERS[1:18], value = c(5, 2, 6, 3, 8, 10, 9, 12, 11, 4, 8, 10, 9, 4, 5, 6, 3, 11))
  
  # calculate dice probabilities
  chits$strength <- 6-abs(7-chits$value)
  
  # Randomize the resource types, match with the coordinates(ax, hor) of the spiral sequence
  tiles <- data.frame(tile = c(1:19),
                      res = rep(c("Brick", "Ore", "Sheep", "Wheat", "Wood", "Desert"), times=c(3,3,4,4,4,1))[order(c(ord,desert-.5))],
                      ax = c(2, 1, 0, -1, -2, -2, -2, -1, 0, 1, 2, 2, 1, 0, -1, -1, 0, 1, 0),
                      hor = c(2, 2, 2, 1, 0, -1, -2, -2, -2, -1, 0, 1, 1, 1, 0, -1, -1, 0, 0))
  
  tiles$lett[tiles$res != "Desert"] <- as.character(chits$lett)
  tiles <- merge(tiles, chits, by = "lett", all=T, sort = F)

  # assign Desert strength to 0
  tiles$strength[tiles$res == "Desert"] <- 0
  tiles$lett[tiles$res == "Desert"] <- ""
  
  # reorder the tiles in original random order (undone by merge)
  tiles <- tiles[order(tiles$tile), ]
  row.names(tiles) <- NULL
  return(tiles)
}