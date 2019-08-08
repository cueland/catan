#' Generate Randomized Catan Tiles
#'
#' This function generates a randomized set of tiles for the catan board.
#' @param tile_order A vector of length 18 that sets the order of the non-desert tiles
#' @param desert An integer between 1 and 19 for the location of the desert
#' @keywords
#' @export
#' @examples
#' gen_tiles()
#' gen_tiles(sample(1:18), sample(1:19,1))
#' gen_tiles(desert = sample(1:19,1))
#' 

gen_tiles <- function(tile_order = sample(1:18), desert = sample(1:19,1),
                      port_offset = sample(0:5, 1), port_order = sample(1:9), original = F) {
  
  # set all random variables to original conditions if original = TRUE
  if (original == T) {
    # This is the original game setup, where everything is pre-determined
    tile_order <- c(4, 11, 15, 5, 12, 7, 13, 8, 16, 1, 2, 9, 10, 17, 3, 6, 18, 14)
    desert <- 11
    port_offset <- 3
    port_order <- c(1:9)
  }
  
    # Input the order of number chits and their corresponding strengths
  chits <- data.frame(lett = LETTERS[1:18],
                      value = c(5, 2, 6, 3, 8, 10, 9, 12, 11, 4, 8, 10, 9, 4, 5, 6, 3, 11))
  
  # calculate dice probabilities
  chits$strength <- 6-abs(7-chits$value)
  
  # Randomize the resource types, match with the coordinates(axx, hor) of the spiral sequence
  tiles <- data.frame(tile = c(1:19),
                      res = rep(c("Brick", "Ore", "Sheep", "Wheat", "Wood", "Desert"),
                                times=c(3,3,4,4,4,1)) [c(tile_order,19)[order(c(1:18,desert-.5))]],
                      axx = c(2, 1, 0, -1, -2, -2, -2, -1, 0, 1, 2, 2, 1, 0, -1, -1, 0, 1, 0),
                      hor = c(-2, -2, -2, -1, 0, 1, 2, 2, 2, 1, 0, -1, -1, -1, 0, 1, 1, 0, 0))
  
  # assign the chits to the tiles, less the desert tile
  tiles$lett[tiles$res != "Desert"] <- as.character(chits$lett)
  tiles <- merge(tiles, chits, by = "lett", all=T, sort = F)

  # assign Desert strength, chit to 0, ""
  tiles$strength[tiles$res == "Desert"] <- 0
  tiles$lett[tiles$res == "Desert"] <- ""
  
  # define the ports (to randomize, add sample to the port types)
  ports <- data.frame(tile = (c(2,3,4,6,7,8,10,11,12)+2*port_offset-1)%%12+1,
                      # add the 6 because you are subtracting
                      port_side = (6+c(6, 5, 4, 4, 3, 2, 2, 1, 6)-port_offset-1)%%6+1,
                      port_res = c("Sheep", "Random", "Ore", "Wheat", "Random",
                                   "Wood", "Brick", "Random", "Random") [port_order] )
  
  # merge the ports into the tiles frame (also reorders the tiles df by tile)
  tiles <- return(merge(tiles, ports, by = "tile", all.x = T))
}
