#' Calculate Corner Data (calc_corners)
#'
#' This function calculates corner data from tile and port input.
#' The output is a complex data frame with a plethora of information for each corner.
#' @param tiles A dataframe with tile data
#' @param ports A dataframe with id and port data
#' @keywords
#' @export
#' @examples
#' gen_tiles(tiles, ports)
#' 

calc_corners <- function(tiles, ports) {
  # create a data frame to house all the relevant corners of the game
  corners <- merge(expand.grid(tile = 1:nrow(tiles), corner = 1:6),
                   tiles[,c("tile", "axx", "hor")],
                   by = "tile", all.x = T)
  
  # get relevant corner codes with corner id's, then strip off duplicates
  cornerids <- unique(do.call(rbind, apply(corners[,c("axx", "hor", "corner")], 1, FUN = calc_adj))$id)
  
  # get the adjacent resources for each corner
  corners <- data.frame(do.call(rbind, lapply(cornerids, function(x) get_res(tiles, id = x))))
  
  # strip out the list format for some variables
  for (n in c("id", "tot_prob", "res_count", "uniq_res")) {
    corners[,n] <- unlist(corners[,n])
  }
  
  # merge in port data
  corners <- merge(corners, ports[,c("id", "port")], by = "id", all.x = TRUE)
  
  # order decreasing by best prob and then unique resources
  corners <- corners[order(corners$tot_prob, corners$uniq_res, decreasing=T),]
  
  # calculate x,y position for corners using the first set of axial coordinates / corner
  corners1 <- t(apply(t(sapply(str_split(corners$id, ","), FUN = as.integer))[,1:3], 1, FUN = cart))
  # add colnames
  colnames(corners1) <- c("xx", "yy")
  # bind x,y data to corner data
  return(cbind(corners, corners1))
}