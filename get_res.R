#' Get Resources (get_res)
#'
#' This function calculates the adjacent resources for a given corner id or corner identifier
#' @param tiles A dataframe with axial coordinates and their corresponding resources
#' @param id A string with nine coordinates axx,hor,corner for the 3 tiles corresponing to one corner
#' @param x A vector with three elements in the format c(axx, hor, corner).
#' @keywords
#' @export
#' @examples
#' get_res(tiles, id="0,0,1,1,0,3,0,1,5")
#' get_res(tiles, coords=c(1,-2,4))
#' 

get_res <- function(tiles, id=NULL, coords=NULL) {
  if(missing(id)) {
    if(missing(coords)) {
      stop("Must supply either id or coords.")
    }
    else {
      id <- calc_adj(coords)$id
    }
  }
  # parse the id
  dd <- data.frame(t(matrix(as.integer(str_split(id,",")[[1]]), nrow = 3, byrow = F, dimnames = list(c("axx", "hor", "corner")))))
  dd <- merge(dd,tiles[c("axx", "hor", "res", "value", "strength")], by = c("axx", "hor"))
  valuables <- c("Brick", "Ore", "Sheep", "Wheat", "Wood")
  res_mat <- data.frame(t(data.frame(row.names = valuables,
                        strength = unlist(lapply(valuables, function(x) sum(dd$strength[dd$res==x]))),
                        count = unlist(lapply(valuables, function(x) sum(dd$res==x))))))
  res_mat$id <- id
  res_mat$time <- row.names(res_mat)
  res_mat <- reshape(res_mat, idvar="id", timevar="time", direction = "wide")
  res_mat$tot_prob <- sum(dd$strength)
  res_mat$res_count <- sum(dd$res%in%valuables)
  res_mat$uniq_res <- length(unique(dd$res[dd$res%in%valuables]))
  # res_mat$res <- unique(as.character(dd$res[dd$res%in%valuables]))
  res_mat$sea <- ifelse(nrow(dd) < 3,1,0)
  res_mat$desert <- ifelse(sum(dd$res%in%"Desert")>0,1,0)
  
  return(res_mat)
}
