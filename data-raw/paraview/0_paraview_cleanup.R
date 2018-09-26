# 0_paraview_cleanup.R

library(pacman)
p_load(dplyr,pals,XML)

setwd("c:/x/rpack/pals/data-raw/paraview")

# The xml files were downloaded 12.14.2016 from
# http://www.paraview.org/Wiki/Colormaps

# Some of these palettes are weird.
# There are many palettes taken from colorbrewer.
# There are many duplicates in the 'idl' and 'mpl' sets.
# Conclusion: Not included in pals.

setwd("c:/x/rpack/pals/data-raw/paraview")

# ----------------------------------------------------------------------------

# idl

# Get palette names
d1 <- xmlTreeParse("All_idl_cmaps.xml")
topx <- xmlRoot(d1)
nms1 <- as.vector(xmlSApply(topx, xmlAttrs)[1,])

##  [1] "B-W_LINEAR"            "BLUE-WHITE"            "GRN-RED-BLU-WHT"      
##  [4] "RED_TEMPERATURE"       "BLUE-GREEN-RED-YELLOW" "STD_GAMMA-II"         
##  [7] "PRISM"                 "RED-PURPLE"            "GREEN-WHITE_LINEAR"   
## [10] "GRN-WHT_EXPONENTIAL"   "GREEN-PINK"            "BLUE-RED"             
## [13] "16_LEVEL"              "RAINBOW"               "STEPS"                
## [16] "STERN_SPECIAL"         "Haze"                  "Blue_-_Pastel_-_Red"  
## [19] "Pastels"               "Hue_Sat_Lightness_1"   "Hue_Sat_Lightness_2"  
## [22] "Hue_Sat_Value_1"       "Hue_Sat_Value_2"       "Purple-Red_Stripes"   
## [25] "Beach"                 "Mac_Style"             "Eos_A"                
## [28] "Eos_B"                 "Hardcandy"             "Nature"               
## [31] "Ocean"                 "Peppermint"            "Plasma"               
## [34] "Blue-Red"              "Rainbow"               "Blue_Waves"           
## [37] "Volcano"               "Waves"                 "Rainbow18"            
## [40] "Rainbow_white"         "Rainbow_black"        

# Get palettes
dat1 <- xmlParse("All_idl_cmaps.xml")
dat1 <- xmlToList(dat1)

for(i in seq(nms1)){
  mm1 <- do.call("rbind", dat1[[i]][1:256]) %>% as.matrix %>% apply(MARGIN=2, FUN=as.numeric)
  pals::pal.bands(rgb(mm1[,3:5]), main=nms1[i])
}

# ----------------------------------------------------------------------------

# mpl = Matplotlib

# Get palette names
d1 <- xmlTreeParse("All_mpl_cmaps.xml")
topx <- xmlRoot(d1)
nms2 <- as.vector(xmlSApply(topx, xmlAttrs)[1,])

##  [1] "Accent"       "Blues"        "BrBG"         "BuGn"         "BuPu"        
##  [6] "Dark2"        "GnBu"         "Greens"       "Greys"        "OrRd"        
## [11] "Oranges"      "PRGn"         "Paired"       "Pastel1"      "Pastel2"     
## [16] "PiYG"         "PuBu"         "PuBuGn"       "PuOr"         "PuRd"        
## [21] "Purples"      "RdBu"         "RdGy"         "RdPu"         "RdYlBu"      
## [26] "RdYlGn"       "Reds"         "Set1"         "Set2"         "Set3"        
## [31] "Spectral"     "YlGn"         "YlGnBu"       "YlOrBr"       "YlOrRd"      
## [36] "autumn"       "binary"       "bone"         "cool"         "copper"      
## [41] "flag"         "gist_earth"   "gist_gray"    "gist_heat"    "gist_ncar"   
## [46] "gist_rainbow" "gist_stern"   "gist_yarg"    "gray"         "hot"         

# Get palettes
dat2 <- xmlParse("All_mpl_cmaps.xml")
dat2 <- xmlToList(dat2)

# This does NOT include all matplotlib colormaps, such as "summer"
for(i in seq(nms2){
  mm1 <- do.call("rbind", dat2[[i]][1:256]) %>% as.matrix %>% apply(MARGIN=2, FUN=as.numeric)
  pals::pal.bands(rgb(mm1[,3:5]), main=nms2[i])
}

# ----------------------------------------------------------------------------
