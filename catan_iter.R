#' Iteration (catan_iter)
#'
#' This function calculates corner data from tile and port input.
#' The output is a complex data frame with a plethora of information for each corner.
#' @param tiles A dataframe with tile data
#' @param ports A dataframe with id and port data
#' @keywords
#' @export
#' @examples
#' catan_iter(n)
#' 
catan_iter <- function(n) {
  time <- Sys.time()
  y <- list()
  for (x in 1:n) {
    ord <- sample(1:18)
    desert <- sample(1:19,1)
    p_offset <- sample(0:5, 1)
    # Use the random tile generator to generate a randomized catan board
    tiles <- gen_tiles(ord, desert)
    # tiles <- gen_tiles(ord = sample(1:18), desert = sample(1:11,1))
    
    # # Use the random port generator to generate a set of randomized ports
    # ports <- gen_ports(p_offset)
    # 
    # # Join the tile and port data to calculate information for each corner of the game
    # corners <- calc_corners(tiles, ports)
    
    y[[x]] <- list(ord=ord, desert=desert, p_offset=p_offset,
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