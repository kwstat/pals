# cmocean.r
# Time-stamp: <10 Oct 2016 22:48:45 c:/x/rpack/pals/data-raw/ocean/cmocean.r>

# Color definitions downloaded 10.10.16 from
https://github.com/matplotlib/cmocean/tree/master/cmocean/rgb

setwd("c:/x/rpack/pals/data-raw/ocean/")

# These colors were stored in txt files
cmocean.amp = rgb(read.csv("amp-rgb.txt", sep=" ", header=FALSE))
cmocean.speed = rgb(read.csv("speed-rgb.txt", sep=" ", header=FALSE))
cmocean.tempo = rgb(read.csv("tempo-rgb.txt", sep=" ", header=FALSE))

# Other colors were stored in py files.
# Emacs macros were used to remove python code from the files
fil <- dir("c:/x/rpack/pals/data-raw/ocean/", pattern=".py$")
ocean.algae <- rgb(read.csv("algae.py", header=FALSE))
ocean.deep <- rgb(read.csv("deep.py", header=FALSE))
ocean.dense <- rgb(read.csv("dense.py", header=FALSE))
ocean.gray <- rgb(read.csv("gray.py", header=FALSE))
ocean.haline <- rgb(read.csv("haline.py", header=FALSE))
ocean.ice <- rgb(read.csv("ice.py", header=FALSE))
ocean.matter <- rgb(read.csv("matter.py", header=FALSE))
ocean.oxy <- rgb(read.csv("oxy.py", header=FALSE))
ocean.phase <- rgb(read.csv("phase.py", header=FALSE))
ocean.solar <- rgb(read.csv("solar.py", header=FALSE))
ocean.thermal <- rgb(read.csv("thermal.py", header=FALSE))
ocean.turbid <- rgb(read.csv("turbid.py", header=FALSE))

# The diverging colors have separate palettes for left/right
ocean.balance.blue <- rgb(read.csv("balance-blue.py", header=FALSE))
ocean.balance.red <- rev(rgb(read.csv("balance-red.py", header=FALSE)))
ocean.balance <- c(ocean.balance.blue, ocean.balance.red)
ocean.balance <- colorRampPalette(ocean.balance)(256) # downsample to 256
pal.sineramp(ocean.balance)

ocean.curl.pink <- rgb(read.csv("curl-pink.py", header=FALSE))
ocean.curl.turquoise <- rev(rgb(read.csv("curl-turquoise.py", header=FALSE)))
ocean.curl <- rev(c(ocean.curl.pink, ocean.curl.turquoise)) # To match Oceanography paper
ocean.curl <- colorRampPalette(ocean.curl)(256) # downsample to 256
pal.sineramp(ocean.curl)

ocean.delta.blue <- rgb(read.csv("delta-blue.py", header=FALSE))
ocean.delta.green <- rev(rgb(read.csv("delta-green.py", header=FALSE)))
ocean.delta <- c(ocean.delta.blue, ocean.delta.green)
ocean.delta <- colorRampPalette(ocean.delta)(256) # downsample to 256
pal.sineramp(ocean.delta)

pal.test(ocean.delta)

for(ff in fil){
  kk=read.csv(ff, header=FALSE)
  kk=as.matrix(kk)
  pal.sineramp(rgb(kk))
  title(ff)
}


# ----------------------------------------------------------------------------

# hist2d from here:
# https://github.com/matplotlib/viscm/blob/master/viscm/examples/hist2d.txt

hist2d <- read.csv("hist2d.txt", sep=" ", header=FALSE)
hist2d <- as.matrix(hist2d)
image(hist2d)
image(round(hist2d,2), col=viridis(100)) # Only slightly different
image(round(hist2d,2), col=jet(100)) # Only slightly different
image(round(hist2d,2), col=parula(100)) # Only slightly different
