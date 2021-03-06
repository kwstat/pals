# 0_cmocean_cleanup.R
# Time-stamp: <25 Sep 2018 11:24:03 c:/x/rpack/pals/data-raw/ocean/0_cmocean_cleanup.R>

# Color definitions downloaded 10.10.16 from
# https://github.com/matplotlib/cmocean/tree/master/cmocean/rgb
# Note, the presence of all these .py files caused githib to think that the
# pals repository was a python project, so I renamed the .py files to .pyth
# to prevent that and force github to see the pals repository as an R project

setwd("c:/x/rpack/pals/data-raw/ocean/")

# These colors were stored in txt files
cmocean.amp = rgb(read.csv("amp-rgb.txt", sep=" ", header=FALSE))
cmocean.speed = rgb(read.csv("speed-rgb.txt", sep=" ", header=FALSE))
cmocean.tempo = rgb(read.csv("tempo-rgb.txt", sep=" ", header=FALSE))

# Other colors were stored in py files.
# Emacs macros were used to remove python code from the files
fil <- dir("c:/x/rpack/pals/data-raw/ocean/", pattern=".pyth$")
ocean.algae <- rgb(read.csv("algae.pyth", header=FALSE))
ocean.deep <- rgb(read.csv("deep.pyth", header=FALSE))
ocean.dense <- rgb(read.csv("dense.pyth", header=FALSE))
ocean.gray <- rgb(read.csv("gray.pyth", header=FALSE))
ocean.haline <- rgb(read.csv("haline.pyth", header=FALSE))
ocean.ice <- rgb(read.csv("ice.pyth", header=FALSE))
ocean.matter <- rgb(read.csv("matter.pyth", header=FALSE))
ocean.oxy <- rgb(read.csv("oxy.pyth", header=FALSE))
ocean.phase <- rgb(read.csv("phase.pyth", header=FALSE))
ocean.solar <- rgb(read.csv("solar.pyth", header=FALSE))
ocean.thermal <- rgb(read.csv("thermal.pyth", header=FALSE))
ocean.turbid <- rgb(read.csv("turbid.pyth", header=FALSE))

# The diverging colors have separate palettes for left/right
ocean.balance.blue <- rgb(read.csv("balance-blue.pyth", header=FALSE))
ocean.balance.red <- rev(rgb(read.csv("balance-red.pyth", header=FALSE)))
ocean.balance <- c(ocean.balance.blue, ocean.balance.red)
ocean.balance <- colorRampPalette(ocean.balance)(256) # downsample to 256
# pals::pal.sineramp(ocean.balance)

ocean.curl.pink <- rgb(read.csv("curl-pink.pyth", header=FALSE))
ocean.curl.turquoise <- rev(rgb(read.csv("curl-turquoise.pyth", header=FALSE)))
ocean.curl <- rev(c(ocean.curl.pink, ocean.curl.turquoise)) # To match Oceanography paper
ocean.curl <- colorRampPalette(ocean.curl)(256) # downsample to 256
# pals::pal.sineramp(ocean.curl)

ocean.delta.blue <- rgb(read.csv("delta-blue.pyth", header=FALSE))
ocean.delta.green <- rev(rgb(read.csv("delta-green.pyth", header=FALSE)))
ocean.delta <- c(ocean.delta.blue, ocean.delta.green)
ocean.delta <- colorRampPalette(ocean.delta)(256) # downsample to 256
# pals::pal.sineramp(ocean.delta)

# I then used 'dput' and manually pasted the results into
# 1_cmocean_colors.R

