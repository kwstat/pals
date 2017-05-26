require(XML)
setwd("c:/x/rpack/pals/data-raw/paraview")

# The xml files were downloaded 12.14.2016 from
# http://www.paraview.org/Wiki/Colormaps

# Some of these are weird.  I'm not sure there's any value including
# these coloramps into 'pals'

# ----------------------------------------------------------------------------

# IDL

lib(dplyr)
lib(pals)
lib(XML)

# Get palette names
d1 <- xmlTreeParse("All_idl_cmaps.xml")
topx <- xmlRoot(d1)
nms <- as.vector(xmlSApply(topx, xmlAttrs)[1,])

# Get palettes
dat1 <- xmlParse("All_idl_cmaps.xml")
dat2 <- xmlToList(dat1)

for(i in 1:41){
  mm1 <- do.call("rbind", dat2[[i]][1:256]) %>% as.matrix %>% apply(MARGIN=2, FUN=as.numeric)
  pal.bands(rgb(mm1[,3:5]), main=nms[i])
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# Matplotlib

# Get palette names
d1 <- xmlTreeParse("All_mpl_cmaps.xml")
topx <- xmlRoot(d1)
nms <- as.vector(xmlSApply(topx, xmlAttrs)[1,])

# Get palettes
dat1 <- xmlParse("All_mpl_cmaps.xml")
dat2 <- xmlToList(dat1)

# This does not include all matplotlib colormaps, such as "summer"
for(i in 1:50){
  mm1 <- do.call("rbind", dat2[[i]][1:256]) %>% as.matrix %>% apply(MARGIN=2, FUN=as.numeric)
  pal.bands(rgb(mm1[,3:5]), main=nms[i])
}

# ----------------------------------------------------------------------------
