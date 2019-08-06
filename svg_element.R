#' Generate an SVG element (svg)
#'
#' This function creates an SVG item object
#' @param tiles A dataframe with a list axial coordinates and their corresponding resources
#' @param id A string with nine coordinates ax,hor,corner for the 3 tiles corresponing to one corner
#' @param x A vector with three elements in the format c(ax, hor, corner).
#' @keywords
#' @export
#' @examples
#' get_res(tiles, id="0,-3,2,1,-2,4,0,-2,6")
#' get_res(tiles, coords=c(1,-2,4))
#' 
#  setClass("svg_element", slots = list(tag = "character")
# 
#           
# svg_element <- function(type, attributes) {
#   x <- paste0("<",type,"")
# }