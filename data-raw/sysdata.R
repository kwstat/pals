# sysdata.R
# Time-stamp: <>

lib(readr)

# ----------------------------------------------------------------------------

# Matplotlib colormaps by Nathaniel J. Smith, Stefan van der Walt,
# and (in the case of viridis) Eric Firing.
# https://github.com/BIDS/colormap/blob/master/colormaps.py
# Released under CC0 license / public domain

# The file was edited in emacs to change from python to csv

setwd("c:/x/rpack/pals/data-raw")
colrs <- read.csv("colrs.csv")

colrs %>% filter(palette=="viridis") %>% select(red,green,blue) %>% rgb %>% list %>% pal.chips
colrs %>% filter(palette=="glasbey") %>% select(red,green,blue) %>% rgb(., max=255) %>% list %>% pal.chips

# ----------------------------------------------------------------------------


# ColorBrewer palettes
# http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_RGB.html
# Released under the Apache License, Version 2.0

library(dplyr)
library(tibble)
library(readxl)
dat <- read_excel("ColorBrewer_all_schemes_RGBonly3.xlsx")
dat <- dat[1:1689,] # drop license info at bottom
dat <- dat[,c(1,2,7:9)]
names(dat) <- c('palette','ncolors','red','green','blue')
# replace missing value of palette by previous
pp <- dat$palette
nn <- dat$ncol
for(ii in 1:length(pp)){
  if(is.na(pp[ii])) pp[ii] <- pp[ii-1]
  if(is.na(nn[ii])) nn[ii] <- nn[ii-1]
}
dat$palette <- pp
dat$ncol <- nn

dat$palette <- tolower(dat$palette)

# ----------------------------------------------------------------------------

brewer.purd <- function(n) {
  if(n < 3) warning("Only accepts n>2")
  if(n <= 9) {
    mat <- subset(dat, palette=="purd" & ncol==n)
    pal <- rgb(mat[,c('red','green','blue')], max=255)
  } else {
    mat <- subset(dat, palette=="purd" & ncol==9)
    pal <- colorRampPalette(rgb(mat[,c('red','green','blue')], max=255))(n)
  }
}
brewer.purd(1)
brewer.purd(2)
brewer.purd(3)
brewer.purd(9)
brewer.purd(25)
pal.chips(list(brewer.purd))
pal.chips(list(brewer.purd(3),brewer.purd(4),brewer.purd(5),brewer.purd(6),brewer.purd(7),brewer.purd(8),brewer.purd(9),brewer.purd(10),brewer.purd(11),brewer.purd(12),brewer.purd(13),brewer.purd(14),brewer.purd(15),brewer.purd(100)))

# ----------------------------------------------------------------------------

# Now save all objects into sysdata
devtools::use_data(brewer, matplot, internal=TRUE)

