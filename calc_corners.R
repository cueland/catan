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

calc_corners <- function(tiles) {
  # create a data frame to house all the relevant corners of the game
  corners <- merge(expand.grid(tile = 1:nrow(tiles), corner = 1:6),
                   tiles,
                   by = "tile", all.x = T)
  
  # get relevant corner codes with corner id's
  corners$id <- do.call(rbind, apply(corners[,c("axx", "hor", "corner")], 1, FUN = calc_adj))$id
  
  # order by tile then corner
  corners <- corners[order(corners$tile, corners$corner),]
  
  # group corners and then sort by groups
  corners$group <- as.numeric(factor(corners$id, levels = unique(corners$id)))
  
  # clean up ports data - port = resource if corner = side or side-1
  corners$port <- apply(corners[,c("corner", "port_side", "port_res")], 1, FUN = function(x) {
    if (!is.na(as.numeric(x[2])) & (x[1] == as.numeric(x[2]) | x[1] == (6+as.numeric(x[2])-2)%%6+1)) {
      return(x[3])
    }
    else {
      return(NA)
    }
  })
  
  corners$port_side <- NULL
  corners$port_res <- NULL
  
  # apply port to all members of each corner group
  group_ports <- aggregate(list(port = corners$port), by = list(group = corners$group), FUN = function(x) x[order(x)][1])
  corners$port <- NULL
  corners <- merge(corners, group_ports, by = "group", all.x = T)
  
  # get and merge in adjacent resources for each corner
  corners <- merge(corners,
                   data.frame(do.call(rbind, lapply(unique(corners$id), function(x) get_res(tiles, id = x)))),
                   by = "id", all.x = TRUE)
  
  # order decreasing by best prob and then unique resources
  corners <- corners[order(corners$tot_prob, corners$uniq_res, decreasing=T),]
  
  # calculate x,y position for corners using the first set of axial coordinates / corner
  corners1 <- t(apply(t(sapply(str_split(corners$id, ","), FUN = as.integer))[,1:3], 1, FUN = cart))
  # add colnames
  colnames(corners1) <- c("xx", "yy")
  # bind x,y data to corner data
  
  return(cbind(corners, corners1))
}