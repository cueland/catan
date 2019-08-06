#' Output SVG code for the Catan Board (svg_element)
#'
#' This function outputs a character array with SVG elements that plot the Catan board.
#' The output is a complex data frame with a plethora of information for each corner.
#' @param type Character string with polygon, text, cirle, etc
#' @param points Character string with the relevant x,y coordinate(s)
#' @param style Character string with the relevant style string
#' @keywords
#' @export
#' @examples
#' catan_svg(tiles, corners, width = 900)
#' 

catan_svg <- function(tiles, corners, width = 900) {
  
  # define the colors that go alongside the board resources
  hex_colors <- data.frame(res = c("Brick", "Ore", "Sheep", "Wheat", "Wood", "Desert"),
                           hex = c("#945337", "#b8a2c7","#c7e0ca", "#fff9b9", "#7aa86b", "#cccccc"),
                           stringsAsFactors = F)
  
  
}