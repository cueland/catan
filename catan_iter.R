#' Iteration (catan_iter)
#'
#' This function calculates corner data from tile and port input.
#' The output is a complex data frame with a plethora of information for each corner.
#' @param n Number of iterations
#' @param ports A dataframe with id and port data
#' @keywords
#' @export
#' @examples
#' catan_iter(n)
#' 
catan_iter <- function(n) {
  time <- Sys.time() # initialize time
  y <- list() # initialize
  for (x in 1:n) {
    tile_order <- sample(1:18)
    desert <- sample(1:19,1)
    port_offset <- sample(0:5, 1)
    port_order <- sample(1:9)
    original <- F
    # Use the random tile generator to generate a randomized catan board
    tiles <- gen_tiles(tile_order, desert, port_offset, port_order, original)
    
    # # Join the tile and port data to calculate information for each corner of the game
    # corners <- calc_corners(tiles, ports)
    
    y[[x]] <- list(tile_order=tile_order, desert=desert, port_offset=port_offset,
                   port_order=port_order, original=original,
                   strength = aggregate(list(strength = tiles$strength),
                                        by = list(res = tiles$res),
                                        FUN = sum)$strength)
    if (x%%25 == 0) {
      print(paste0(round(x/n*100,0), "% : ", round(Sys.time() - time, 0)))
    }
  }
  Sys.time() - time
  return(y)
}