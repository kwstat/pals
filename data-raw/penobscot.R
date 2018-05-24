# penobscot.R

# prepare the penobscot data for the pals package

# http://nbviewer.jupyter.org/github/seg/tutorials-2014/blob/master/1402_Smoothing_surfaces/1402_Smoothing_surfaces.ipynb

# http://ahay.org/blog/2014/03/11/matt-halls-tutorial-on-smoothing/

libs(readr, reshape2)

dat <- read_tsv("https://github.com/agile-geoscience/notebooks/blob/master/data/Penobscot_HorB.txt?raw=true", col_names=FALSE)
head(dat)
mat <- t(acast(dat, X1~X2, value.var="X3"))

penobscot=mat
save(penobscot, file = "c:/x/rpack/pals/data/penobscot.rda", compress="xz")

# --------------------------------

# This Italy data is not part of 'pals'.

# Blog post

# http://nbviewer.jupyter.org/github/aadm/geophysical_notes/tree/master/

# Jupyter notebook on colormaps here:
# http://nbviewer.jupyter.org/github/aadm/geophysical_notes/blob/master/colormaps.ipynb

# Italy data
# https://github.com/aadm/geophysical_notes/blob/master/etopo1_southern_italy.nc?raw=true

libs(RNetCDF)
nc <- open.nc("c:/x/rpack/pals/data-raw/etopo1_southern_italy.nc")
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

R> range(alldat$Band1)
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

# -----------------------

# bivariate normal
lib(mvtnorm)
x0 = seq(from=-3, to=3, length=100)
y0 = seq(from=-2, to=2, length=100)
xy = expand.grid(x0,y0)
z0 = matrix(dmvnorm(xy, mean=c(1,0.5))^2 - 0.4*dmvnorm(xy, mean=c(-1,-0.5))^2,
            100,100) / 0.03
filled.contour(x=x0, y=y0, z=z0, color.palette=ocean.delta, nlevels=99)
filled.contour(x=x0, y=y0, z=z0, color.palette=cubehelix, nlevels=99)
filled.contour(x=x0, y=y0, z=z0, color.palette=parula, nlevels=99)
filled.contour(x=x0, y=y0, z=z0, color.palette=viridis, nlevels=99)

# oblique stripes
x1 = seq(from = -pi, to =  pi, length=100)
y1 = seq(from = -pi, to =  pi, length=100)
xy1 = expand.grid(x1,y1)
z1 = matrix( sin(xy1$Var1 + xy1$Var2/4), 100, 100)
filled.contour(x=x1, y=y1, z=z1, color.palette=ocean.delta, nlevels=99)
filled.contour(x=x1, y=y1, z=z1, color.palette=cubehelix, nlevels=99)
filled.contour(x=x1, y=y1, z=z1, color.palette=parula, nlevels=99)
filled.contour(x=x1, y=y1, z=z1, color.palette=viridis, nlevels=99)

