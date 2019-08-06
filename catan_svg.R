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
  
  # create an index to preserve order after the merge
  tiles$ord <- 1:nrow(tiles)
  
  # merge in colors
  tiles <- merge(tiles, hex_colors, by = "res", all.x= T)
  # reoder to original order
  tiles <- tiles[order(tiles$ord),]
  
  tiles$stars <- sapply(tiles$strength, function(x){paste0(rep("*",x), collapse="")})
  tiles$desc <- as.expression(paste(tiles$res, tiles$value, tiles$stars, sep="\n"))
  
  # determine the scale variables
  margin <- .05
  coord_scale <- width/2 * (1-2*margin) / max(abs(unlist(tiles$corner_coords)))
  # transform the coordinates to account for SVG format
  corner_coords_SVG <- lapply(tiles$corner_coords, FUN = function(x) {width/2 + x * coord_scale})
  
  header <- c("<?xml version=\"1.0\" encoding=\"utf-8\"?>",
              paste0("<svg version=\"1.1\" id=\"Layer_1\" xmlns=\"http://www.w3.org/2000/svg\"",
                     " xmlns:xlink=\"http://www.w3.org/1999/xlink\" x=\"0px\" y=\"0px\""),
              paste0("\tviewBox=\"0 0 ", width, " ", width, ";\" style=\"enable-background:new 0 0 ",
                     width, " ", width, ";\" xml:space=\"preserve\">"))
  
  # generate the an array of strings formatted for SVG polygon points
  points <- lapply(
    lapply(corner_coords_SVG, FUN = function(x) {
      apply(x,1,FUN = function(y) {paste0(y,collapse = ",")} )
    }
    ),
    FUN = function(z) {paste0(z, collapse = " ")})
  
  # ---------------------------------------| Create the SVG file |--------------------------------------------- #
  
  # create the polygon text
  pgons <- unlist(mapply(FUN = function(x,y) {
    paste0("<polygon points = '",x, "' style = 'fill:",y,";stroke:#353535;stroke-width:", width/450, "' />")
  }, points, tiles$hex))
  
  # create the text in the center of the hexagons
  texxt <- unlist(mapply(FUN = function(x, y, res, val, sth) {
    paste0("<text dominant-baseline='middle' text-anchor='middle' transform='matrix(1 0 0 1 ",x, " ", y, ")' ",
           "style='font-family:\"AGaramondPro-Regular\";font-size:", width/30, "px;'>\n<tspan x='0' y='-1em'>", res, "</tspan>",
           "<tspan x='0' y='0em'>", val, "</tspan>",
           "<tspan x='0' y='1em'>", paste0(rep("*", sth), collapse = ""), "</tspan>",
           "\n</text>", sep = "")
  }, width/2 + tiles$xx * coord_scale, width/2 + tiles$yy * coord_scale, tiles$res, tiles$value, tiles$strength))
  
  # plot a circle for each corner that has access to a port
  port_corners <- corners[!is.na(corners$port),c("port", "xx", "yy")]
  port_corners <- merge(port_corners, hex_colors, by.x = "port", by.y = "res", all.x = T)
  port_corners$hex[port_corners$port == "Random"] <- "#c5c5c5"
  port_corners$xx <- port_corners$xx * coord_scale + width/2
  port_corners$yy <- port_corners$yy * coord_scale + width/2
  
  ports_svg <- unlist(apply(port_corners, 1, FUN = function(x) {
    paste0("<circle cx='",x[2] , "' cy='",x[3] , "' r='", width/45, "' stroke='black' stroke-width='", width/450, "' fill='", x[4], "' />")
  }))
  
  # highlight corners with good resources
  good_corners <- corners[1:10, c("tot_prob", "xx", "yy")]
  good_corners$xx <- good_corners$xx * coord_scale + width/2
  good_corners$yy <- good_corners$yy * coord_scale + width/2
  good_corners$tot_prob <- (good_corners$tot_prob - min(good_corners$tot_prob))/2 + 1
  good_corners$col <- "#4934eb"
  good_corners$col[good_corners$tot_prob == max(good_corners$tot_prob)] <- "#eb3467"
  
  corners_svg <- unlist(apply(good_corners, 1, FUN = function(x) {
    paste0("<circle cx='",x[2] , "' cy='",x[3] , "' r='", width / 90 * as.numeric(x[1]),
           "' stroke='black' stroke-width='", width/450, "' fill='", x[4], "' />")
  }))
  
  footer <- "</svg>"
  
  return(c(header,pgons,texxt, ports_svg, corners_svg, footer))
}