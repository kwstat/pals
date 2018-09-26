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
# replace missing value of palette column by previous value
pp <- dat$palette
nn <- dat$ncol
for(ii in 1:length(pp)){
  if(is.na(pp[ii])) pp[ii] <- pp[ii-1]
  if(is.na(nn[ii])) nn[ii] <- nn[ii-1]
}
dat$palette <- pp
dat$ncol <- nn

dat$palette <- tolower(dat$palette)
