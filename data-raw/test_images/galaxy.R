# galaxy.R
# Time-stamp: <06 Sep 2018 17:27:18 c:/x/rpack/pals/data-raw/test_images/galaxy.R>

library(pacman)
p_load(dplyr,fs,lattice,pals,png,readxl,readr,reshape2,tibble)

# Inspiration from this site:
# http://yt-project.org/doc/visualizing/colormaps/index.html

# Downloaded the galaxy image from here:
# http://yt-project.org/doc/_images/cmap_images__Projection_B-W_LINEAR_r.png
setwd("c:/x/rpack/pals/data-raw/test_images/")
dat0 <- readPNG("cmap_images__Projection_B-W_LINEAR_r.png")
dat0 <- dat0[,,3]
dat0 <- t(dat0)[,920:1]
galaxy <- 1-dat0
galaxy <- galaxy[125:1010,95:885]

gal <- function(col, n=300, rev=FALSE) {
  if(rev) {
    image(galaxy, col=rev(col(199)), axes=FALSE)
  } else {
    image(galaxy, col=col(199), axes=FALSE)
  }
}
gal(brewer.accent)
gal(grey.colors)
gal(brewer.blues)
gal(brewer.brbg)
gal(brewer.bupu)
gal(brewer.dark2)
gal(brewer.gnbu)
gal(brewer.greens)
gal(brewer.greys)
gal(brewer.orrd)
gal(brewer.prgn)
gal(brewer.paired)
gal(brewer.spectral)
gal(cubehelix)
gal(coolwarm)
gal(warmcool)
gal(cubehelix) # good
gal(gnuplot) # poor
gal(jet)
gal(viridis)
gal(parula)
gal(viridis, rev=TRUE)
gal(parula, rev=TRUE)
