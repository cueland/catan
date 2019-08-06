# setup packages
library(stringr)
library(graphics)
library(tidyverse)
library(here)
library(svglite)
library(gridSVG)

# call function scripts
source(here("gen_tiles.R"))
source(here("gen_ports.R"))
source(here("calc_adj.R"))
source(here("cart.R"))
source(here("get_res.R"))
source(here("get_corners.R"))
source(here("svg_element.R"))

# Use the random tile generator to generate a randomized catan board
(tiles <- gen_tiles(ord = sample(1:18), desert = sample(1:19,1)))

# Use the random port generator to generate a set of randomized ports
(ports <- gen_ports(p_offset = sample(0:5, 1), port_random = F, all = F))

# create a data frame to house all the relevant corners of the game
corners <- merge(expand.grid(tile = 1:nrow(tiles), corner = 1:6), tiles[,c("tile", "axx", "hor")], by = "tile", all.x = T)

# get relevant corner codes with corner id's, then strip off duplicates
cornerids <- unique(do.call(rbind, apply(corners[,c("axx", "hor", "corner")], 1, FUN = calc_adj))$id)

# 
corners <- data.frame(do.call(rbind, lapply(cornerids, function(x) get_res(tiles, x))))
for (n in c("id", "tot_prob", "res_count", "uniq_res")) {
  corners[,n] <- unlist(corners[,n])
}

# merge in port data
corners <- merge(corners, ports[,c("id", "port")], by = "id", all.x = TRUE)
rm(ports)

# order decreasing by best prob and then unique resources
corners <- corners[order(corners$tot_prob, corners$uniq_res, decreasing=T),]

# calculate x,y position for corners
corners1 <- t(apply(t(sapply(str_split(corners$id, ","), FUN = as.integer))[,1:3], 1, FUN = cart_find))
colnames(corners1) <- c("xx", "yy")
corners <- cbind(corners, corners1)
rm(corners1)

colhx <- data.frame(res = c("Brick", "Ore", "Sheep", "Wheat", "Wood", "Desert"),
                    hex = c("#945337", "#b8a2c7","#c7e0ca", "#fff9b9", "#7aa86b", "#cccccc"),
                    stringsAsFactors = F)
tiles$ord <- 1:nrow(tiles)
colss <- merge(tiles, colhx, by = "res", all.x= T)
colss <- colss[order(colss$ord),]
colss$strength[is.na(colss$strength)] <- 0
colss$stars <- sapply(colss$strength, function(x){paste0(rep("*",x), collapse="")})

colss$desc <- as.expression(paste(colss$res, colss$value, colss$stars, sep="\n"))
xx <- lapply(colss, function(x){expression(paste(x$res, x$value, x$stars, sep="/n"))})

# generate the center x,y coordinates for each hex
hex_coords <- matrix(unlist(lapply(as.list(data.frame(t(tiles[,c("axx", "hor")]))),
                                   FUN = function(x) {cart_find(c(x, 0))})),
                     ncol = 2, byrow = T)
# transform the coordinates to account for SVG format
hex_coords_SVG <- 450 + hex_coords * 100

# generate the x,y coordinates of each corner for every tile
corner_coords <- lapply(as.list(data.frame(t(tiles[,c("axx", "hor")]))), FUN = function(x) get_corners(x))
# transform the coordinates to account for SVG format
corner_coords_SVG <- lapply(corner_coords, FUN = function(x) {450 + x * 100})

# ---------------------------------------| Create the SVG file |--------------------------------------------- #

header <- c("<?xml version=\"1.0\" encoding=\"utf-8\"?>",
            "<!-- Generator: Adobe Illustrator 23.0.4, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->",
            "<svg version=\"1.1\" id=\"Layer_1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" x=\"0px\" y=\"0px\"",
            "\tviewBox=\"0 0 900 900\" style=\"enable-background:new 0 0 900 900;\" xml:space=\"preserve\">")

# generate the an array of strings formatted for SVG polygon points
points <- lapply(
  lapply(corner_coords_SVG, FUN = function(x) {
    apply(x,1,FUN = function(y) {paste0(y,collapse = ",")} )
  }
  ),
  FUN = function(z) {paste0(z, collapse = " ")})

# create the polygon text
pgons <- unlist(mapply(FUN = function(x,y) {
  paste0("<polygon points = '",x, "' style = 'fill:",y,";stroke:#353535;stroke-width:2' />")
}, points, colss$hex))

# create the text in the center of the hexagons
texxt <- unlist(mapply(FUN = function(x, y, res, val, sth) {
  paste0("<text dominant-baseline='middle' text-anchor='middle' transform='matrix(1 0 0 1 ",x, " ", y, ")' ",
         "style='font-family:\"AGaramondPro-Regular\";font-size:24px;'>\n<tspan x='0' y='-1em'>", res, "</tspan>",
         "<tspan x='0' y='0em'>", val, "</tspan>",
         "<tspan x='0' y='1em'>", paste0(rep("*", sth), collapse = ""), "</tspan>",
         "\n</text>", sep = "")
      }, hex_coords_SVG[,1], hex_coords_SVG[,2], tiles$res, tiles$value, tiles$strength))

port_corners <- corners[!is.na(corners$port),c("port", "xx", "yy")]
port_corners <- merge(port_corners, colhx, by.x = "port", by.y = "res", all.x = T)
port_corners$hex[port_corners$port == "Random"] <- "#c5c5c5"
port_corners$xx <- port_corners$xx * 100 + 450
port_corners$yy <- port_corners$yy * 100 + 450

ports_svg <- unlist(apply(port_corners, 1, FUN = function(x) {
  paste0("<circle cx='",x[2] , "' cy='",x[3] , "' r='20' stroke='black' stroke-width='2' fill='", x[4], "' />")
}))

footer <- "</svg>"

# write the SVG code to a file
write(c(header,pgons,texxt, ports_svg, footer), file = "polygons.svg")

# ---------------------------------------| Plot using R |--------------------------------------------- #

dat <- split(c(corner_coords, colss$hex), rep(1:19,2))
x <- split(c(hex_coords), rep(1:19,2))
plot_size <- 4.5

plot(-plot_size:plot_size,-plot_size:plot_size, type='n', axes=F, xlab="", ylab="")
lapply(dat, function(x){polygon(x[[1]][,1], -x[[1]][,2], col = x[[2]], border = "black")})

text(hex_coords[,1], -hex_coords[,2], labels = lapply(colss$desc, function(x) paste0(x)))