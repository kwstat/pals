# penobscot.R

# prepare the penobscot data for the pals package

# See:
# http://nbviewer.jupyter.org/github/seg/tutorials-2014/blob/master/1402_Smoothing_surfaces/1402_Smoothing_surfaces.ipynb
# http://ahay.org/blog/2014/03/11/matt-halls-tutorial-on-smoothing/

library(pacman)
p_load(readr,reshape2)

dat <- read_tsv("https://github.com/agile-geoscience/notebooks/blob/master/data/Penobscot_HorB.txt?raw=true", col_names=FALSE)
head(dat)
mat <- t(acast(dat, X1~X2, value.var="X3"))

penobscot=mat
save(penobscot, file = "c:/x/rpack/pals/data/penobscot.rda", compress="xz")
