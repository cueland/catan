# setup packages
library(stringr)
library(graphics)
library(tidyverse)
library(here)
library(svglite)
library(gridSVG)

# call function scripts
source(here("calc_adj.R"))
source(here("cart.R"))
source(here("get_res.R"))


# plot_polygon <- function(xx,yy,fill,stroke, strokew) {
#   # create the string to draw an SVG polygon with colors and borders
#   # check if xx and yy have same length
#     if(length(xx) != length(yy)) {
#       stop("XX and YY are different length vectors")
#     }
#   # check 
#   
#   #create array with xx and yy values
#   # x <- paste0("<polygon class=\"{fill:#", counter
# }

id <- paste(c(-1,-1,2,0,0,4,-1,0,6), collapse=",")


# force R to never use scientific notation
# options("scipen"=100, "digits"=4)

# Input the order of number chits and their corresponding strengths
chits <- data.frame(lett = LETTERS[1:18], value = c(5, 2, 6, 3, 8, 10, 9, 12, 11, 4, 8, 10, 9, 4, 5, 6, 3, 11))

# calculate dice probabilities
chits$strength <- 6-abs(7-chits$value)

# Randomize the resource types, match with the coordinates(ax, hor) of the spiral sequence
tiles <- data.frame(tile = c(1:19),
                    res = sample(rep(c("Brick", "Ore", "Sheep", "Wheat", "Wood", "Desert"), times=c(3,3,4,4,4,1))),
                    ax = c(2, 1, 0, -1, -2, -2, -2, -1, 0, 1, 2, 2, 1, 0, -1, -1, 0, 1, 0),
                    hor = c(2, 2, 2, 1, 0, -1, -2, -2, -2, -1, 0, 1, 1, 1, 0, -1, -1, 0, 0))


tiles$lett[tiles$res != "Desert"] <- as.character(chits$lett)
tiles <- merge(tiles, chits, by = "lett", all=T, sort = F)
rm(chits)
# assign Desert strength to 0
tiles$strength[tiles$res == "Desert"] <- 0

# reorder the tiles in original random order (undone by merge)
tiles <- tiles[order(tiles$tile), ]
row.names(tiles) <- NULL

# define the ports (to randomize, add sample to the port types)
ports <- data.frame(tile.orig = c(1, 2, 3, 3, 4, 4, 6, 6, 7, 7, 8, 8, 10, 10, 11, 11, 12, 12),
                    corner.orig = c(3, 2, 2, 3, 3, 4, 3, 4, 4, 5, 5, 6, 5, 6, 6, 1, 1, 2),
                    port = rep(c("Sheep", "Random", "Ore", "Wheat", "Random", "Wood", "Brick", "Random", "Random"),
                               times = rep(2,9)))

# randomize the offset for the ports
p_offset <- sample(0:5, 1) # can orient the board 6 different ways
ports$tile <- (ports$tile.orig+2*p_offset-1)%%12+1
ports$corner <- (ports$corner.orig+p_offset-1)%%6+1

# convert tile number to tile coords
ports <- merge(ports, tiles[,c("tile", "ax", "hor")], by = "tile", all.x=T)

# add unique id to each port corner
ports$id <- do.call(rbind, apply(ports[,c("ax", "hor", "corner")], 1, FUN = calc_adj))$id

# create a data frame to house all the relevant corners of the game
corners <- merge(expand.grid(tile = 1:nrow(tiles), corner = 1:6), tiles[,c("tile", "ax", "hor")], by = "tile", all.x = T)

# get relevant corner codes with corner id's, then strip off duplicates
cornerids <- unique(do.call(rbind, apply(corners[,c("ax", "hor", "corner")], 1, FUN = calc_adj))$id)

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
                    hex = c("#945337", "#b8a2c7","#c7e0ca", "#fff9b9", "#7aa86b", "#cccccc"))
tiles$ord <- 1:nrow(tiles)
colss <- merge(tiles, colhx, by = "res", all.x= T)
colss <- colss[order(colss$ord),]
colss$strength[is.na(colss$strength)] <- 0
colss$stars <- sapply(colss$strength, function(x){paste0(rep("*",x), collapse="")})

colss$desc <- as.expression(paste(colss$res, colss$value, colss$stars, sep="\n"))
xx <- lapply(colss, function(x){expression(paste(x$res, x$value, x$stars, sep="/n"))})
x <- split(unlist(lapply(as.list(data.frame(t(tiles[,c("ax", "hor")]))), FUN = function(x) {cart_find(c(x, 0))})), rep(1:2,19))

dat <- lapply(as.list(data.frame(t(tiles[,c("ax", "hor")]))), FUN = corns)
dat <- split(c(dat, as.character(colss$hex)), rep(1:19,2))


plot(-5:5,-5:5, type='n', axes=F, xlab="", ylab="")
lapply(dat, function(x){polygon(x[[1]]$xx, x[[1]]$yy, col = x[[2]], border = "black")})
text(x[[1]], x[[2]], labels = lapply(colss$desc, function(x) paste0(x)))
# symbols(corners$xx, corners$yy, circles = rep(1,length(corners$xx)))

svglite(file = "Rplots.svg", width = 10, height = 8, bg = "white",
        pointsize = 12)
