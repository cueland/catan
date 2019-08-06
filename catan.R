# setup packages
library(stringr)
library(graphics)
library(here)

# call function scripts
source(here("cart.R"))
source(here("get_res.R"))
source(here("calc_adj.R"))
source(here("gen_tiles.R"))
source(here("gen_ports.R"))
source(here("calc_corners.R"))
source(here("catan_svg.R"))

# Use the random tile generator to generate a randomized catan board
tiles <- gen_tiles(ord = sample(1:18), desert = sample(1:19,1))
# tiles <- gen_tiles(ord = sample(1:18), desert = sample(1:11,1))

# Use the random port generator to generate a set of randomized ports
ports <- gen_ports(p_offset = sample(0:5, 1), port_random = F, all = F)

# Join the tile and port data to calculate information for each corner of the game
corners <- calc_corners(tiles, ports)

# write the SVG code to a file
write(catan_svg(tiles, corners, width = 1000), file = "polygons.svg")

# ---------------------------------------| Plot using R |--------------------------------------------- #

dat <- split(c(tiles$corner_coords, colss$hex), rep(1:19,2))

plot_size <- 4.5

plot(-plot_size:plot_size,-plot_size:plot_size, type='n', axes=F, xlab="", ylab="")
lapply(dat, function(x){polygon(x[[1]][,1], -x[[1]][,2], col = x[[2]], border = "black")})

text(tiles$xx, -tiles$yy, labels = lapply(colss$desc, function(x) paste0(x)))
