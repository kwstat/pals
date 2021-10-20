# make_sysdata.R
# Time-stamp: <17 Jun 2021 22:19:03 c:/one/rpack/pals/data-raw/0_make_sysdata.R>

# Execute this entire file to create the sysdata file for the pals package
# The palettes are stored in a list, each item in the list is a separate palette

library(pacman)
p_load(dplyr)

# this function is used to compress a 255-color palette down to a small
# basis of colors 
pal.compress <- function(pal, n=5, thresh=2.5) {
  # the pal argument is a function
  pal255 <- pal(255)
  
  done <- FALSE
  while(!done) {
    palc <- colorRampPalette(pal(n))(255) # compressed palette ramp
    p1 <- convertColor(t(col2rgb(pal255)), from="sRGB",to="Lab",scale.in=255)
    p2 <- convertColor(t(col2rgb(palc)), from="sRGB",to="Lab",scale.in=255)
    delta <- max(apply((p1-p2), 1, function(x) sqrt(sum(x^2))))
    if(delta >  thresh) n=n+1 else done=TRUE
  }
  return(pal(n))
}

setwd("c:/one/rpack/pals/data-raw/")
colsr <- read.csv("colors_rgb.csv")
# Initialize list to hold palettes in sysdata
syspals <- list()

# ----------------------------------------------------------------------------

# coolwarm

# Values from Moreland Table 2

colsr %>% filter(palette=="coolwarm") %>% select(red,green,blue) %>%
  rgb(., max=255) %>% colorRampPalette %>% pal.compress -> syspals$coolwarm

syspals$warmcool <- rev(syspals$coolwarm)

# ----------------------------------------------------------------------------

# glasbey

colsr %>% filter(palette=="glasbey") %>% select(red,green,blue) %>% rgb(., max=255) -> syspals$glasbey

# ----------------------------------------------------------------------------

# jet
# Where did I get this?  I must have compressed it by hand.

syspals$jet <- c("#00007F", "#0000FF", "#007FFF", "#00FFFF",
                 "#7FFF7F", "#ffff00", "#FF7F00", "#ff0000", "#7F0000")

# ----------------------------------------------------------------------------

# kelly
# The color values used here are from
# http://hackerspace.kinja.com/iscc-nbs-number-hex-r-g-b-263-f2f3f4-242-243-244-267-22-1665795040

colsr %>% filter(palette=="kelly") %>% select(red,green,blue) %>% rgb(., max=255) -> syspals$kelly

# ----------------------------------------------------------------------------

# okabe
# The color values used here are from
# http://jfly.iam.u-tokyo.ac.jp/color/

colsr %>% filter(palette=="okabe") %>% select(red,green,blue) %>% rgb(., max=255) -> syspals$okabe

# ----------------------------------------------------------------------------

# parula

# mpl-colormaps have CC0 license.
# https://github.com/BIDS/colormap/blob/master/parula.py

colsr %>% filter(palette=="parula") %>% select(red,green,blue) %>%
  rgb %>% colorRampPalette %>% pal.compress -> syspals$parula

# Here is an almost identical map, rounded to 4 decimals
# http://www.gnuplotting.org/data/parula.pal
# But, gnuplotting license is not free

# ----------------------------------------------------------------------------

# stepped

# http://geog.uoregon.edu/datagraphics/color/StepSeq_25.txt
# Similar to the HSV codes from oregon.edu, but using 4 steps instead of 5

# Even though the columns in the file are called red, green, blue,
# the numbers are really HSV

# The gray colors were chosen by Kevin Wright with the help of
# http://alloyui.com/examples/color-picker/hsv/
# Type in HSV, get RGB colors, desaturate in R, type in RGB, extract HSV.
# 260,0.9,0.6   desaturate(rgb(61,15,154,max=255))  #363636 -> 0 0 21
# 260,0.65,0.7  desaturate(rgb(102,63,179,max=255))  #575757 -> 0 0 34
# 260,0.4,0.8   desaturate(rgb(150,123,205,max=255)) #898989 -> 0 0 54
# 260,0.2,0.9   desaturate(rgb(200,184,230,max=255)) #BFBFBF -> 0 0 75
# The gray values were tweaked slightly to shift toward brighter shades

colsr %>% filter(palette=="stepped") %>% select(red,green,blue) %>%
  mutate(red=red/360) %>% as.matrix -> stepcols
hsv(stepcols[,1], stepcols[,2], stepcols[,3]) -> syspals$stepped

colsr %>% filter(palette=="stepped2") %>% select(red,green,blue) %>% rgb(., max=255) -> syspals$stepped2
colsr %>% filter(palette=="stepped3") %>% select(red,green,blue) %>% rgb(., max=255) -> syspals$stepped3

# ----------------------------------------------------------------------------

# tol

myrgb <- function(x) rgb(x$red, x$green, x$blue, max=255)

colsr %>% filter(palette=="tol") ->  tol
syspals$tol <- split(tol, tol$ncolors) %>% lapply(., myrgb)

# These palettes are similar to other palettes, so 
# These tol functions are not documented, nor do they have accessor functions

# discrete
colsr %>% filter(palette=="tol.bright") %>% select(red,green,blue) %>% rgb(., max=255) -> syspals$tol.bright
colsr %>% filter(palette=="tol.highcontrast") %>% select(red,green,blue) %>% rgb(., max=255) -> syspals$tol.highcontrast
colsr %>% filter(palette=="tol.vibrant") %>% select(red,green,blue) %>% rgb(., max=255) -> syspals$tol.vibrant
colsr %>% filter(palette=="tol.muted") %>% select(red,green,blue) %>% rgb(., max=255) -> syspals$tol.muted
colsr %>% filter(palette=="tol.dark") %>% select(red,green,blue) %>% rgb(., max=255) -> syspals$tol.dark
colsr %>% filter(palette=="tol.light") %>% select(red,green,blue) %>% rgb(., max=255) -> syspals$tol.light

# diverging
colsr %>% filter(palette=="tol.sunset") %>% select(red,green,blue) %>% rgb(., max=255) -> syspals$tol.sunset
colsr %>% filter(palette=="tol.burd") %>% select(red,green,blue) %>% rgb(., max=255) -> syspals$tol.burd
colsr %>% filter(palette=="tol.prgn") %>% select(red,green,blue) %>% rgb(., max=255) -> syspals$tol.prgn

# sequential
colsr %>% filter(palette=="tol.ylorbr") %>% select(red,green,blue) %>% rgb(., max=255) -> syspals$tol.ylorbr
colsr %>% filter(palette=="tol.iridescent") %>% select(red,green,blue) %>% rgb(., max=255) -> syspals$tol.iridescent
colsr %>% filter(palette=="tol.smoothrainbow") %>% select(red,green,blue) %>% rgb(., max=255) -> syspals$tol.smoothrainbow

if(0){
library(pals)
with(syspals, pal.bands(tol.bright, tol.highcontrast, tol.vibrant, tol.muted,
                        tol.dark, tol.light, tol.sunset, tol.burd, tol.prgn,
                        tol.ylorbr, tol.iridescent, tol.smoothrainbow,
                        labels=c("tol.bright", "tol.highcontrast", "tol.vibrant", "tol.muted",
                                 "tol.dark", "tol.light", "tol.sunset", "tol.burd", "tol.prgn",
                                 "tol.ylorbr", "tol.iridescent", "tol.smoothrainbow")))
}
# ----------------------------------------------------------------------------

# tableau20
colsr %>% filter(palette=="tableau20") %>% select(red,green,blue) %>%
  rgb(., max=255) -> syspals$tableau20

# ----------------------------------------------------------------------------
# turbo

colsr %>% filter(palette=="turbo") %>% select(red,green,blue) %>%
       rgb %>% colorRampPalette %>% pal.compress -> syspals$turbo
# ----------------------------------------------------------------------------

# watlington

# http://alumni.media.mit.edu/~wad/color/palette.html

colsr %>% filter(palette=="watlington") %>% select(red,green,blue) %>%
  rgb(., max=255) -> syspals$watlington


# ----------------------------------------------------------------------------

# Matplotlib colormaps by Nathaniel J. Smith, Stefan van der Walt,
# and (in the case of viridis) Eric Firing.
# https://github.com/BIDS/colormap/blob/master/colormaps.py
# The file was edited in emacs to change from python to csv
# Released under CC0 license / public domain

# Matplotlib, viridis

colsr %>% filter(palette=="viridis") %>% select(red,green,blue) %>%
  rgb %>% colorRampPalette %>% pal.compress -> syspals$viridis
colsr %>% filter(palette=="inferno") %>% select(red,green,blue) %>%
  rgb %>% colorRampPalette %>% pal.compress -> syspals$inferno
colsr %>% filter(palette=="magma") %>% select(red,green,blue) %>%
  rgb %>% colorRampPalette %>% pal.compress -> syspals$magma
colsr %>% filter(palette=="plasma") %>% select(red,green,blue) %>%
  rgb %>% colorRampPalette %>% pal.compress -> syspals$plasma

# ----------------------------------------------------------------------------

# Niccoli palettes are already more-or-less compressed

colsr %>% filter(palette=="cubicyf") %>% select(red,green,blue) %>%
  rgb -> syspals$cubicyf
colsr %>% filter(palette=="isol") %>% select(red,green,blue) %>%
  rgb -> syspals$isol
colsr %>% filter(palette=="cubicl") %>% select(red,green,blue) %>%
  rgb -> syspals$cubicl
colsr %>% filter(palette=="linearl") %>% select(red,green,blue) %>%
  rgb -> syspals$linearl
colsr %>% filter(palette=="linearlhot") %>% select(red,green,blue) %>%
  rgb -> syspals$linearlhot

# ----------------------------------------------------------------------------

source("ocean/1_cmocean_colors.R")

ocean.algae %>% colorRampPalette %>% pal.compress -> syspals$ocean.algae
ocean.deep %>% colorRampPalette %>% pal.compress -> syspals$ocean.deep
ocean.dense %>% colorRampPalette %>% pal.compress -> syspals$ocean.dense
ocean.gray %>% colorRampPalette %>% pal.compress -> syspals$ocean.gray
ocean.haline %>% colorRampPalette %>% pal.compress -> syspals$ocean.haline
ocean.ice %>% colorRampPalette %>% pal.compress -> syspals$ocean.ice
ocean.matter %>% colorRampPalette %>% pal.compress -> syspals$ocean.matter
ocean.oxy %>% colorRampPalette %>% pal.compress -> syspals$ocean.oxy
ocean.phase %>% colorRampPalette %>% pal.compress -> syspals$ocean.phase
ocean.solar %>% colorRampPalette %>% pal.compress -> syspals$ocean.solar
ocean.thermal %>% colorRampPalette %>% pal.compress -> syspals$ocean.thermal
ocean.turbid %>% colorRampPalette %>% pal.compress -> syspals$ocean.turbid
ocean.balance %>% colorRampPalette %>% pal.compress -> syspals$ocean.balance
ocean.curl %>% colorRampPalette %>% pal.compress -> syspals$ocean.curl
ocean.delta %>% colorRampPalette %>% pal.compress -> syspals$ocean.delta
ocean.amp %>% colorRampPalette %>% pal.compress -> syspals$ocean.amp
ocean.speed %>% colorRampPalette %>% pal.compress -> syspals$ocean.speed
ocean.tempo %>% colorRampPalette %>% pal.compress -> syspals$ocean.tempo

# ----------------------------------------------------------------------------

# Brewer

colsr %>% filter(palette=="brewer.accent") -> accent
syspals$brewer.accent <- split(accent, accent$ncolors) %>% lapply(., myrgb)

colsr %>% filter(palette=="brewer.blues") -> blues
syspals$brewer.blues <- split(blues, blues$ncolors) %>% lapply(., myrgb)

colsr %>% filter(palette=="brewer.brbg") -> brbg
syspals$brewer.brbg <- split(brbg, brbg$ncolors) %>% lapply(.,myrgb)

colsr %>% filter(palette=="brewer.bugn") -> bugn
syspals$brewer.bugn <- split(bugn, bugn$ncolors) %>% lapply(.,myrgb)

colsr %>% filter(palette=="brewer.bupu") -> bupu
syspals$brewer.bupu <- split(bupu, bupu$ncolors) %>% lapply(.,myrgb)

colsr %>% filter(palette=="brewer.dark2") -> dark2
syspals$brewer.dark2 <- split(dark2     , dark2$ncolors) %>% lapply(., myrgb)

colsr %>% filter(palette=="brewer.gnbu") -> gnbu
syspals$brewer.gnbu <- split(gnbu, gnbu$ncolors) %>% lapply(.,myrgb)

colsr %>% filter(palette=="brewer.greens") -> greens
syspals$brewer.greens <- split(greens, greens$ncolors) %>% lapply(., myrgb)

colsr %>% filter(palette=="brewer.greys") -> greys
syspals$brewer.greys <- split(greys, greys$ncolors) %>% lapply(., myrgb)

colsr %>% filter(palette=="brewer.oranges") -> oranges
syspals$brewer.oranges <- split(oranges, oranges$ncolors) %>% lapply(., myrgb)

colsr %>% filter(palette=="brewer.orrd") -> orrd
syspals$brewer.orrd <- split(orrd, orrd$ncolors) %>% lapply(.,myrgb)

colsr %>% filter(palette=="brewer.paired") -> paired
syspals$brewer.paired <- split(paired, paired$ncolors) %>% lapply(., myrgb)

colsr %>% filter(palette=="brewer.pastel1") -> pastel1
syspals$brewer.pastel1 <- split(pastel1, pastel1$ncolors) %>% lapply(., myrgb)

colsr %>% filter(palette=="brewer.pastel2") -> pastel2
syspals$brewer.pastel2 <- split(pastel2, pastel2$ncolors) %>% lapply(., myrgb)

colsr %>% filter(palette=="brewer.piyg") -> piyg
syspals$brewer.piyg <- split(piyg, piyg$ncolors) %>% lapply(.,myrgb)

colsr %>% filter(palette=="brewer.prgn") -> prgn
syspals$brewer.prgn <- split(prgn, prgn$ncolors) %>% lapply(.,myrgb)

colsr %>% filter(palette=="brewer.pubu") -> pubu
syspals$brewer.pubu <- split(pubu, pubu$ncolors) %>% lapply(.,myrgb)

colsr %>% filter(palette=="brewer.pubugn") -> pubugn
syspals$brewer.pubugn <- split(pubugn, pubugn$ncolors) %>% lapply(., myrgb)

colsr %>% filter(palette=="brewer.puor") -> puor
syspals$brewer.puor <- split(puor, puor$ncolors) %>% lapply(.,myrgb)

colsr %>% filter(palette=="brewer.purd") -> purd
syspals$brewer.purd <- split(purd, purd$ncolors) %>% lapply(.,myrgb)

colsr %>% filter(palette=="brewer.purples") -> purples
syspals$brewer.purples <- split(purples, purples$ncolors) %>% lapply(., myrgb)

colsr %>% filter(palette=="brewer.rdbu") -> rdbu
syspals$brewer.rdbu <- split(rdbu, rdbu$ncolors) %>% lapply(.,myrgb)

colsr %>% filter(palette=="brewer.rdgy") -> rdgy
syspals$brewer.rdgy <- split(rdgy, rdgy$ncolors) %>% lapply(.,myrgb)

colsr %>% filter(palette=="brewer.rdpu") -> rdpu
syspals$brewer.rdpu <- split(rdpu, rdpu$ncolors) %>% lapply(., myrgb)

colsr %>% filter(palette=="brewer.reds") -> reds
syspals$brewer.reds <- split(reds, reds$ncolors) %>% lapply(.,myrgb)

colsr %>% filter(palette=="brewer.rdylbu") -> rdylbu
syspals$brewer.rdylbu <- split(rdylbu, rdylbu$ncolors) %>% lapply(., myrgb)

colsr %>% filter(palette=="brewer.rdylgn") -> rdylgn
syspals$brewer.rdylgn <- split(rdylgn, rdylgn$ncolors) %>% lapply(., myrgb)

colsr %>% filter(palette=="brewer.set1") -> set1
syspals$brewer.set1 <- split(set1, set1$ncolors) %>% lapply(.,myrgb)

colsr %>% filter(palette=="brewer.set2") -> set2
syspals$brewer.set2 <- split(set2, set2$ncolors) %>% lapply(.,myrgb)

colsr %>% filter(palette=="brewer.set3") -> set3
syspals$brewer.set3 <- split(set3, set3$ncolors) %>% lapply(., myrgb)

colsr %>% filter(palette=="brewer.spectral") -> spectral
syspals$brewer.spectral <- split(spectral, spectral$ncolors) %>% lapply(., myrgb)

colsr %>% filter(palette=="brewer.ylgn") -> ylgn
syspals$brewer.ylgn <- split(ylgn, ylgn$ncolors) %>% lapply(.,myrgb)

colsr %>% filter(palette=="brewer.ylgnbu") -> ylgnbu
syspals$brewer.ylgnbu <- split(ylgnbu, ylgnbu$ncolors) %>% lapply(., myrgb)

colsr %>% filter(palette=="brewer.ylorbr") -> ylorbr
syspals$brewer.ylorbr <- split(ylorbr, ylorbr$ncolors) %>% lapply(., myrgb)

colsr %>% filter(palette=="brewer.ylorrd") -> ylorrd
syspals$brewer.ylorrd <- split(ylorrd, ylorrd$ncolors) %>% lapply(., myrgb)

# ----------------------------------------------------------------------------

# kovesi
# Downloaded 12.2.2016
# http://peterkovesi.com/projects/colourmaps/CETperceptual_csv_0_1.zip

# 7040 bytes = object.size(as.data.frame(matrix(runif(3*256), ncol=3)))
# o1 <- read.csv("kovesi/cyclic_grey_15-85_c0_n256.csv", header=FALSE)
# o2 is 504 bytes
# o2 <- read.csv("kovesi/cyclic_grey_15-85_c0_n256.csv", header=FALSE) %>% rgb %>% colorRampPalette %>% pal.compress
# range(lapply(sysdata, length)) # c(5,32) # median 11
# compressing all kovesi palettes saves 87% memory, from 352000 to 46408 bytes

make.kovesi <- function(file){
  read.csv(paste0("kovesi/",file),header=FALSE) %>% rgb %>% colorRampPalette %>% pal.compress
}

syspals$kovesi.cyclic_grey_15_85_c0 <- make.kovesi("cyclic_grey_15-85_c0_n256.csv")
syspals$kovesi.cyclic_grey_15_85_c0_s25 <- make.kovesi("cyclic_grey_15-85_c0_n256_s25.csv")
syspals$kovesi.cyclic_mrybm_35_75_c68 <- make.kovesi("cyclic_mrybm_35-75_c68_n256.csv")
syspals$kovesi.cyclic_mrybm_35_75_c68_s25 <- make.kovesi("cyclic_mrybm_35-75_c68_n256_s25.csv")
syspals$kovesi.cyclic_mygbm_30_95_c78 <- make.kovesi("cyclic_mygbm_30-95_c78_n256.csv")
syspals$kovesi.cyclic_mygbm_30_95_c78_s25 <- make.kovesi("cyclic_mygbm_30-95_c78_n256_s25.csv")
syspals$kovesi.cyclic_wrwbw_40_90_c42 <- make.kovesi("cyclic_wrwbw_40-90_c42_n256.csv")
syspals$kovesi.cyclic_wrwbw_40_90_c42_s25 <- make.kovesi("cyclic_wrwbw_40-90_c42_n256_s25.csv")
syspals$kovesi.diverging_isoluminant_cjm_75_c23 <- make.kovesi("diverging-isoluminant_cjm_75_c23_n256.csv")
syspals$kovesi.diverging_isoluminant_cjm_75_c24 <- make.kovesi("diverging-isoluminant_cjm_75_c24_n256.csv")
syspals$kovesi.diverging_isoluminant_cjo_70_c25 <- make.kovesi("diverging-isoluminant_cjo_70_c25_n256.csv")
syspals$kovesi.diverging_linear_bjr_30_55_c53 <- make.kovesi("diverging-linear_bjr_30-55_c53_n256.csv")
syspals$kovesi.diverging_linear_bjy_30_90_c45 <- make.kovesi("diverging-linear_bjy_30-90_c45_n256.csv")
syspals$kovesi.diverging_rainbow_bgymr_45_85_c67 <- make.kovesi("diverging-rainbow_bgymr_45-85_c67_n256.csv")
syspals$kovesi.diverging_bkr_55_10_c35 <- make.kovesi("diverging_bkr_55-10_c35_n256.csv")
syspals$kovesi.diverging_bky_60_10_c30 <- make.kovesi("diverging_bky_60-10_c30_n256.csv")
syspals$kovesi.diverging_bwr_40_95_c42 <- make.kovesi("diverging_bwr_40-95_c42_n256.csv")
syspals$kovesi.diverging_bwr_55_98_c37 <- make.kovesi("diverging_bwr_55-98_c37_n256.csv")
syspals$kovesi.diverging_cwm_80_100_c22 <- make.kovesi("diverging_cwm_80-100_c22_n256.csv")
syspals$kovesi.diverging_gkr_60_10_c40 <- make.kovesi("diverging_gkr_60-10_c40_n256.csv")
syspals$kovesi.diverging_gwr_55_95_c38 <- make.kovesi("diverging_gwr_55-95_c38_n256.csv")
syspals$kovesi.diverging_gwv_55_95_c39 <- make.kovesi("diverging_gwv_55-95_c39_n256.csv")
syspals$kovesi.isoluminant_cgo_70_c39 <- make.kovesi("isoluminant_cgo_70_c39_n256.csv")
syspals$kovesi.isoluminant_cgo_80_c38 <- make.kovesi("isoluminant_cgo_80_c38_n256.csv")
syspals$kovesi.isoluminant_cm_70_c39 <- make.kovesi("isoluminant_cm_70_c39_n256.csv")
syspals$kovesi.linear_bgy_10_95_c74 <- make.kovesi("linear_bgy_10-95_c74_n256.csv")
syspals$kovesi.linear_bgyw_15_100_c67 <- make.kovesi("linear_bgyw_15-100_c67_n256.csv")
syspals$kovesi.linear_bgyw_15_100_c68 <- make.kovesi("linear_bgyw_15-100_c68_n256.csv")
syspals$kovesi.linear_blue_5_95_c73 <- make.kovesi("linear_blue_5-95_c73_n256.csv")
syspals$kovesi.linear_blue_95_50_c20 <- make.kovesi("linear_blue_95-50_c20_n256.csv")
syspals$kovesi.linear_bmw_5_95_c86 <- make.kovesi("linear_bmw_5-95_c86_n256.csv")
syspals$kovesi.linear_bmw_5_95_c89 <- make.kovesi("linear_bmw_5-95_c89_n256.csv")
syspals$kovesi.linear_bmy_10_95_c71 <- make.kovesi("linear_bmy_10-95_c71_n256.csv")
syspals$kovesi.linear_bmy_10_95_c78 <- make.kovesi("linear_bmy_10-95_c78_n256.csv")
syspals$kovesi.linear_gow_60_85_c27 <- make.kovesi("linear_gow_60-85_c27_n256.csv")
syspals$kovesi.linear_gow_65_90_c35 <- make.kovesi("linear_gow_65-90_c35_n256.csv")
syspals$kovesi.linear_green_5_95_c69 <- make.kovesi("linear_green_5-95_c69_n256.csv")
syspals$kovesi.linear_grey_0_100_c0 <- make.kovesi("linear_grey_0-100_c0_n256.csv")
syspals$kovesi.linear_grey_10_95_c0 <- make.kovesi("linear_grey_10-95_c0_n256.csv")
syspals$kovesi.linear_kry_5_95_c72 <- make.kovesi("linear_kry_5-95_c72_n256.csv")
syspals$kovesi.linear_kry_5_98_c75 <- make.kovesi("linear_kry_5-98_c75_n256.csv")
syspals$kovesi.linear_kryw_5_100_c64 <- make.kovesi("linear_kryw_5-100_c64_n256.csv")
syspals$kovesi.linear_kryw_5_100_c67 <- make.kovesi("linear_kryw_5-100_c67_n256.csv")
syspals$kovesi.linear_ternary_blue_0_44_c57 <- make.kovesi("linear_ternary-blue_0-44_c57_n256.csv")
syspals$kovesi.linear_ternary_green_0_46_c42 <- make.kovesi("linear_ternary-green_0-46_c42_n256.csv")
syspals$kovesi.linear_ternary_red_0_50_c52 <- make.kovesi("linear_ternary-red_0-50_c52_n256.csv")
syspals$kovesi.rainbow_bgyr_35_85_c72 <- make.kovesi("rainbow_bgyr_35-85_c72_n256.csv")
syspals$kovesi.rainbow_bgyr_35_85_c73 <- make.kovesi("rainbow_bgyr_35-85_c73_n256.csv")
syspals$kovesi.rainbow_bgyrm_35_85_c69 <- make.kovesi("rainbow_bgyrm_35-85_c69_n256.csv")
syspals$kovesi.rainbow_bgyrm_35_85_c71 <- make.kovesi("rainbow_bgyrm_35-85_c71_n256.csv")

# ----------------------------------------------------------------------------
# cividis

# Data downloaded from
# https://github.com/pnnl/cmaputil/tree/master/colormaps

cividis <- read.fwf("cividis.lut",c(6,6,6))
cividis <- rgb(cividis, max=255)
cividis %>% colorRampPalette %>% pal.compress -> syspals$cividis

# ----------------------------------------------------------------------------

library(purrr)
library(usethis)

# For a while, use_data would not work with a list, and this was needed
# https://stackoverflow.com/questions/49673667
# walk2(syspals, names(syspals), 
#       function(obj, name) {
#         assign(name, obj)
#           do.call("use_data",
#                 list(as.name(name),
#                      internal=TRUE,
#                      overwrite = TRUE))
#       })
use_data(syspals, internal=TRUE, overwrite=TRUE)

if(FALSE){
file.info("c:/x/rpack/pals/R/sysdata.rda")
file.size("c:/x/rpack/pals/R/sysdata.rda") # in bytes
# check rda to verify the best compression
tools::checkRdaFiles("c:/x/rpack/pals/R/sysdata.rda")
# resave rda with a better compression if available
# tools::resaveRdaFiles("R/sysdata.rda")
# tools::checkRdaFiles("R/sysdata.rda") # actually bigger!
}

# ----------------------------------------------------------------------------

