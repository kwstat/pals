# italy.R
# Time-stamp: <04 Jun 2018 09:37:01 c:/x/rpack/pals/data-raw/test_images/italy.R>

# This Italy data is not part of 'pals'.

# Blog post
# http://nbviewer.jupyter.org/github/aadm/geophysical_notes/tree/master/

# Jupyter notebook on colormaps here:
# http://nbviewer.jupyter.org/github/aadm/geophysical_notes/blob/master/colormaps.ipynb

# Italy data
# https://github.com/aadm/geophysical_notes/blob/master/etopo1_southern_italy.nc?raw=true


library(pacman)
p_load(dplyr,fs,lattice,pals,RNetCDF,readxl,readr,reshape2,tibble)

nc <- open.nc("c:/x/rpack/pals/data-raw/test_images/etopo1_southern_italy.nc")
print.nc(nc)

alldat <- read.nc(nc)

# range(alldat$lon) # 11 17
# range(alldat$lat) # 39 43

image(x=alldat$lon, y=alldat$lat, z=alldat$Band1,
      xlab="Long", ylab="Lat")
image(x=alldat$lon, y=alldat$lat, z=alldat$Band1,
      xlab="Long", ylab="Lat", col=cubehelix(99))

library(maps)
map("world", xlim=range(alldat$lon), ylim=range(alldat$lat), 
    lwd = 2, add=TRUE)

map("world", xlim=range(alldat$lon), ylim=range(alldat$lat))
par()$usr
# [1] 10.98490 17.41510 39.56260 43.30407

range(alldat$Band1)
# [1] -3645  2529
brks = seq(from=-3650,to=2550, by=50)
which(brks==0) # 74
length(brks) # 125
# there are 74-1=73 bins less than 0, 
# 125-74=51 bins greater than 0 
# double each half, then take the left/right half respectively
col1 = ocean.delta(73*2)[1:73]
col2 = ocean.delta(51*2)[52:102]

# final version
# double each half, then take the left/right half respectively
mypal = ocean.delta
col1 = mypal(73*2)[1:73]
col2 = mypal(51*2)[52:102]
library(maps)
image(x=alldat$lon, y=alldat$lat, z=alldat$Band1,
      xlab="Long", ylab="Lat", col=c(col1, col2), breaks=brks)
map("world", xlim=range(alldat$lon), ylim=range(alldat$lat), 
    lwd = 2, add=TRUE, resolution=0)
