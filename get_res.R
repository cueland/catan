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
  res <- c("Brick", "Ore", "Sheep", "Wheat", "Wood", "Desert")
  # import port data
  return(list(id = id,
              tot_prob = sum(dd$strength),
              res_count = sum(!is.na(dd$res)),
              uniq_res = length(unique(dd$res)),
              res = unique(as.character(dd$res)),
              res_mat = data.frame(res = res,
                                   strength = unlist(lapply(res, function(x) sum(dd$strength[dd$res==x]))),
                                   count = unlist(lapply(res, function(x) sum(dd$res==x))))))
}