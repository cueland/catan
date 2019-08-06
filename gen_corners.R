#' Generate Randomized Port Values
#'
#' This function generates a randomized set of ports for the catan board.
#' @param ord A vector of length 18 that sets the order of the non-desert tiles
#' @param desert An integer between 1 and 19 for the location of the desert
#' @keywords
#' @export
#' @examples
#' gen_tiles()
#' gen_tiles(sample(1:18), sample(1:19,1))
#' gen_tiles(desert = sample(1:19,1))
#' 

gen_ports <- function(p_offset = sample(0:5, 1), port_random = F, all = F) {
  # determine randomization
  if (port_random) {
    port <- sample(1:9)
  } else {
    port <- c(1:9)
  }
  
  # define the ports (to randomize, add sample to the port types)
  ports <- data.frame(tile.orig = c(1, 2, 3, 3, 4, 4, 6, 6, 7, 7, 8, 8, 10, 10, 11, 11, 12, 12),
                      corner.orig = c(4, 5, 5, 4, 4, 3, 4, 3, 3, 2, 2, 1, 2, 1, 1, 6, 6, 5),
                      port = rep(c("Sheep", "Random", "Ore", "Wheat", "Random", "Wood", "Brick", "Random", "Random")[port],
                                 times = rep(2,9)))
  
  # randomize the offset for the ports
  # p_offset <- sample(0:5, 1) # can orient the board 6 different ways
  ports$tile <- (ports$tile.orig+2*p_offset-1)%%12+1
  ports$corner <- (6+ports$corner.orig-p_offset-1)%%6+1
  
  tiles <- data.frame(tile = c(1:19),
                      axx = c(2, 1, 0, -1, -2, -2, -2, -1, 0, 1, 2, 2, 1, 0, -1, -1, 0, 1, 0),
                      hor = c(-2, -2, -2, -1, 0, 1, 2, 2, 2, 1, 0, -1, -1, -1, 0, 1, 1, 0, 0))
  
  # convert tile number to tile coords
  ports <- merge(ports, tiles, by = "tile", all.x=T)
  
  # add unique id to each port corner
  ports$id <- do.call(rbind, apply(ports[,c("axx", "hor", "corner")], 1, FUN = calc_adj))$id
  if (all) {
    return(ports)
  }
  else {
    return(ports[,c("id", "port")])
  }
}