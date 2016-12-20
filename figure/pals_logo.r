# pals_logo.r
# Time-stamp: <20 Dec 2016 15:08:38 c:/x/rpack/pals/figure/pals_logo.r>

lib(pals)

# outline text demo
lib(TeachingDemos)
plot(1:10, 1:10, bg='aliceblue')
      rect(3,3,5,8, col='navy')
      text(5,6, 'Test 1', col='lightsteelblue')
shadowtext(5,4, 'Test 2', col='lightsteelblue')

# hexagon demo
lib(plotrix)
plot(1:10)
hexagon(2,2,unit=7)

# ----------------------------------------------------------------------------

# Code for logo begins here
windows(width=2, height=2)
op <- par(mar=c(0,0,0,0))
plot(5,type="n",xlim=c(0,10), ylim=c(0,10), axes=FALSE, xlab="", ylab="",
     xaxs = "i" # no extra space
     )
 # run across the three primaries
gradient.rect(0,0,10,10,
              col=parula(150),
              #reds=c(1,0),
              #greens=c(seq(0,1,length=10),seq(1,0,length=10)),
              #blues=c(0,1),
              gradient="y")
shadowtext(5, 4, 'pals', col='white', r=0.15,
           theta= seq(pi/4, 2*pi, length.out=16), cex=4)
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
