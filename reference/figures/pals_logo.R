# pals_logo.r
# Time-stamp: <14 Jun 2018 08:24:33 c:/x/rpack/pals/figure/pals_logo.R>

# outline text demo
libs(TeachingDemos)
plot(1:10, 1:10, bg='aliceblue')
      rect(3,3,5,8, col='navy')
      text(5,6, 'Test 1', col='lightsteelblue')
shadowtext(5,4, 'Test 2', col='lightsteelblue')

# hexagon demo
lib(plotrix)

par(pty="s")
plot(1:10, xlim=c(1,10), ylim=c(1,10), type='l')
points(10:1, type='l') # clearly not orthogonal
hexagon(2,2,unit=7)

# Another hexagon, better looking with equal-length sides
library(spatstat)
H <- hextess(square(1), 0.5)
H$tiles <- H$tiles[[1]]
plot(H[2]) # just the middle hexagon
box()
# ----------------------------------------------------------------------------

# Version 1, pointy bottom & non-equal sides

# Code for logo begins here
lib(pals)
lib(TeachingDemos)
lib(plotrix)

# Use this to make a small logo for github
windows(width=2, height=2)

op <- par(mar=c(0,0,0,0))
plot(5,type="n",xlim=c(0,10), ylim=c(0,10), axes=FALSE, xlab="", ylab="",
     xaxs = "i" # no extra space
     )
gradient.rect(0,0,10,10, col=parula(150), gradient="y")
shadowtext(5, 4, 'pals', col='white', bg="gray30", r=0.1,
           theta= seq(pi/4, 2*pi, length.out=16), cex=10)
hexagon(1,1,unit=8)
# locator()
# $x
# [1] 1 5 9 9 5 1
# $y
# [1] 2 0 2 8 10 8 
polygon(x=c(1, 5, 1), y=c(0, 0, 2), col="white",border="white")
polygon(x=c(5, 9, 9), y=c(0, 0, 2), col="white",border="white")
polygon(x=c(5, 9, 9), y=c(10, 10, 8), col="white",border="white")
polygon(x=c(1, 5, 1), y=c(10, 10, 8), col="white",border="white")
polygon(x=c(0, 1, 1, 0), y=c(0,0, 10,10), col="white",border="white")
polygon(x=c(9, 10, 10, 9), y=c(0,0, 10,10), col="white",border="white")

# Save as png
# Edit in Gimp. Crop, shrink to 150 pixels wide

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# Version 2, flat bottom, equal-length sides

lib(pals)
lib(TeachingDemos) # for shadowtext
lib(plotrix) # gradient.rect
lib(spatstat)
H <- hextess(square(1), 0.5)

op <- par(mar=c(0,0,0,0))
plot(H[2], xaxs="i", main="") # just the middle hexagon
#box()
gradient.rect(0,0.066987299,1,0.933012702,
              col=parula(150), gradient="y", border=NA)
#plot(H[2], xaxs="i", add=TRUE) # just the middle hexagon
bot=0.066987299; top=0.933012701
polygon(x=c(0, .25, 0), y=c(bot, bot, .5), col="white",border="white")
polygon(x=c(.75, 1, 1), y=c(bot, bot, .5), col="white",border="white")
polygon(x=c(1, 1, .75), y=c(.5, top, top), col="white",border="white")
polygon(x=c(0, .25, 0), y=c(.5, top, top), col="white",border="white")
shadowtext(.5, .4, 'pals', col='white', bg="gray30", r=0.15,
           theta= seq(pi/4, 2*pi, length.out=4), cex=10)

# Click Resize, Fit to window. Shrink window, save to png

savePlot("pals_logo.png", type="png",
