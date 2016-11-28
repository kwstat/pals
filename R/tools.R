# tools.R
# Time-stamp: <24 Nov 2016 07:25:32 c:/x/rpack/pals/R/tools.R>
# Copyright: Kevin Wright, 2016. License: GPL-3.

# ----------------------------------------------------------------------------

#' Show palettes as colored bands
#'
#' Show palettes as colored bands.
#' 
#' @param ... Palettes, either functions or vectors of colors.
#' 
#' @param n The number of colors to display for palette functions.
#' 
#' @param labels Labels for palettes
#'
#' @param title Title at top of page.
#' 
#' @examples
#' pal.bands(cubehelix, gnuplot, jet, tol.rainbow, inferno, magma, plasma, viridis, parula, n=51)
#' pal.bands(c('red','white','blue'), rainbow)
#'
#' @export
pal.bands <- function(..., n=100, labels=NULL, title=NULL){

  if(n < 3) warning("Using n=3")
  
  # Each argument in '...' is a palette function or palette vector.
  # if a function, use n colors
  # if a vector, use all colors in the palette

  pals <- list(...)
  isfun <- unlist(lapply(pals, is.function))
  npal <- length(pals)

  if(!is.null(labels)) {
    if(length(labels) != npal)
      stop("Length of labels needs to match number of palettes.")
  } else {
    # Get the palette function name, or blank
    # Once a function is passed as an argument, the name of the function is gone,
    # so we have to use 'match.call' to get the names
    mc <- match.call()
    labels <- unlist(lapply(mc, deparse))
    labels <- labels[-1] # first item is 'pal.bands'
    labels <- labels[1:npal] # other arguments n, labels
    labels <- ifelse(isfun, labels, "")
  }
  
  # Now convert the palette functions to palette vectors
  for(i in 1:npal) {
    if(isfun[i]) pals[[i]] <- pals[[i]](n)
  }
  # Count the number of boxes for each palette
  nc <- unlist(lapply(pals, length))
  
  maxn <- max(nc)
  ylim <- c(0, npal)
  oldpar <- par(mgp = c(2, 0.25, 0))
  on.exit(par(oldpar))
  plot(1, 1, xlim = c(0, maxn), ylim = ylim,
       type = "n", axes = FALSE, bty = "n", xlab = "", ylab = "")
  
  for (i in 1:npal) {
    # i goes bottom to top, npal+1-i goes top to bottom
    nj <- nc[npal+1 - i]
    shadi <- pals[[npal+1 - i]]
    brks <- seq(from=0, to=maxn, length=nj+1) # break points between colors
    rect(xleft = brks[1:nj], ybottom = i-1,
         xright = brks[2:(nj+1)],      ytop = i-0.2, col = shadi, border = NA)
    
    # If inidividual colors in a palette have names, add them
    nms <- names(shadi)
    if(!is.null(nms)) {
      text(brks[1:nj] + 0.5, i-.6, nms, srt=90, cex=.75)
    }
  }

  # Palette name along left side
  text(rep(-0.1, npal), (1:npal) - 0.6,
       labels = rev(labels),
       cex=0.6, xpd = TRUE, adj = 1)

  if(!is.null(title)) title(title)
  
}
if(FALSE){
pal.bands(c('red','white','blue'),c('blue','yellow'), c('black','red','gold'), labels=c('USA','Sweden','Germany'))
pal.bands(cm.colors, rainbow, topo.colors, heat.colors, c('red','blue'), n=31)
pal.bands(alphabet)
pal.bands(alphabet,n=25) # omit black
pal.bands(alphabet,n=26)
pal.bands(alphabet,n=27)
pal.bands(cubehelix)
pal.bands(alphabet,cubehelix)
pal.bands(cubehelix,alphabet)
pal.bands(warmcool, c('red','orange','yellow','green','blue','purple'), coolwarm, n=11)
pal.bands(alphabet,cols25,glasbey,kelly,stepped,tol)

}

# ----------------------------------------------------------------------------

#' Show a palette of colors in three dimensional RGB or LUV space
#'
#' The supplied color palette is converted to red,green,blue values
#' and plotted in a three-dimensional scatterplot.
#' 
#' @param pal A palette function or a vector of colors.
#' 
#' @param n The number of colors to display for palette functions.
#' 
#' @param label If TRUE, show color name/value on plot
#'
#' @param type Either "RGB" (default) or "LUV".
#' 
#' @return None
#' @import rgl
#' @export 
#' @examples
#' \dontrun{
#' pal.cube(cubehelix)
#' pal.cube(glasbey, n=32) # blues are too close to each other
#' pal.cube(glasbey, n=32, type="LUV")
#' pal.cube(cols25(25), type="LUV", label=TRUE)
#' # To open a second cube
#' rgl.open() # Open a new RGL device
#' rgl.bg(color = "white") # Setup the background color
#' pal.cube(colors()[c(1:152, 254:260, 362:657)]) # All R non-grey colors
#' }
#' 
#' @references
#' None
#'
#' @importFrom methods as
pal.cube <- function(pal, n=100, label=FALSE, type="RGB"){

  if(is.function(pal)) pal <- pal(n)

  if(type=="RGB") {
    x <- t(col2rgb(pal))
    xl="red"; yl="green"; zl="blue"
  } else if (type=="LUV") {
    luvmat <- methods::as(colorspace::hex2RGB(pal), "LUV")
    x <- luvmat@coords
    xl="L"; yl="U"; zl="V"
  }

  plot3d(x, col=pal,
         xlab=xl, ylab=yl,zlab=zl,
         lit=FALSE,
         size=1.5, type='s')
  if(label)
    text3d(x, texts=pal, cex=0.8)

}

# ----------------------------------------------------------------------------

#' Test a palette of colors with a heatmap
#'
#' A random heatmap is generated (with 5% missing values) and a key is added
#' to the heatmap by appending a blank column and then a column with the
#' palette colors.
#'
#' In an effective palette, the value of each cell can be correctly
#' interpreted using the key on the right side, and missing values
#' are identified.
#' 
#' @param pal A palette function or a vector of colors.
#' 
#' @param n The number of colors to display for palette functions.
#'
#' @param miss Fraction of squares with missing values, default .05.
#' 
#' @return None.
#' @author Kevin Wright
#' @export 
#' @examples
#' pal.heatmap(brewer.paired(12), n=12)
#' pal.heatmap(coolwarm(12), n=12)
#' pal.heatmap(tol, n=12)
#' pal.heatmap(glasbey)
#' pal.heatmap(kelly, n=22)
#' @references
#' None
#' @importFrom stats runif
pal.heatmap <- function(pal, n=25, miss=.05){

  if(miss >  1)
    warning("`miss` should be less than 1.")
  
  if(is.function(pal)) {
    pal <- pal(n)
  } else {
    n <- length(pal)
  }
  
  xdim <- 15
  ydim <- n
  cellvals <- sample(1:n, size=xdim*ydim, replace=TRUE)
  # Introduce random missing values
  cellvals[runif(xdim*ydim) < miss] <- NA
  mat <- matrix(cellvals, ncol=xdim)
  
  # Add a column of NA and a column for the palette
  mat <- cbind(mat, NA)
  mat <- cbind(mat, 1:n)
  image(t(mat), col=pal,axes=FALSE)
  axis(side=4)
}

# ----------------------------------------------------------------------------

#' Test a palette of colors with a scatterplot
#'
#' Details.
#' 
#' @param pal A palette function or a vector of colors.
#' 
#' @param n The number of colors to display for palette functions.
#' 
#' @return
#' None.
#' @author Kevin Wright
#' @export 
#' @examples
#' pal.scatter(glasbey, n=31)
#' @references
#' None.
pal.scatter <- function(pal, n){
  if(is.function(pal)) pal <- pal(n)
  
  plot(runif(100), runif(100), col=pal, pch=16,
       xlab="", ylab="",
       xlim=c(0,1), ylim=c(0,1))
  # Need to add a key
}

# ----------------------------------------------------------------------------

#' Test a palette of colors for black/white and colorblind safety
#'
#' .desc
#'
#' .details
#'
#' @param pal A palette function or a vector of colors.
#' 
#' @param n The number of colors to display for palette functions.
#' 
#' @param title Title to display at the top of the test image
#' 
#' @return
#' None.
#' @author Kevin Wright
#' @examples 
#' pal.safe(glasbey(31))
#' pal.safe(rainbow(100), title="rainbow")
#' pal.safe(cubicyf(100), title="cubicyf")
#' pal.safe(parula, title="parula")
#' 
#' @references 
#' None
#' @export
#' @importFrom colorspace desaturate
#' @importFrom dichromat dichromat
pal.safe <- function(pal, n=100, title=NULL){

  if(is.function(pal)) pal <- pal(n)

  ncolor <- length(pal)

  # pal is a single vector of colors, now make it a list
  pal <- list(pal,
              colorspace::desaturate(pal),
              dichromat::dichromat(pal, type="deutan"),
              dichromat::dichromat(pal, type="protan"),
              dichromat::dichromat(pal, type="tritan"))
  labs <- c("Original","Black/White","Deutan","Protan","Tritan")
  npal <- 5
  ylim <- c(0, npal)
  oldpar <- par(mgp = c(2, 0.25, 0))
  on.exit(par(oldpar))
  plot(1, 1, xlim = c(0, ncolor), ylim = ylim,
       type = "n", axes = FALSE, bty = "n", xlab = "", ylab = "")

  for (i in 1:npal) {
    shadi <- pal[[(npal+1) - i]] # Plot from bottom to top, reverse palette order
    rect(xleft = 0:(ncolor - 1), ybottom = i - 1, xright = 1:ncolor, 
         ytop = i - 0.2, col = shadi, border = NA)
  }
  text(rep(-0.1, npal), (1:npal) - 0.6,
       labels = rev(labs),
       cex=0.6, xpd = TRUE, adj = 1)

  if(!is.null(title)) title(title)
}


# ----------------------------------------------------------------------------

#' Test a palette of colors with multiple images
#'
#' 1. Z-order curve.  See: https://en.wikipedia.org/wiki/Z-order_curve
#' A good color palette should be able to resolve 4 sub-squares within
#' each of the 16 squares.
#' 
#' 2. Contrast Sensitivity Function. Also called a contrast-resolution grating.
#' Frequency increases from left to right, contrast decreases from bottom
#' to top.  You can also think of this as a sine wave with periodicity becoming
#' shorter at the right side and the amplitude decreasing to 0 at the top.
#' Poor colormaps hide variation at the top.
#' See: http://blog.kasson.com/?m=201404.
#'
#' 3. Frequency ramp. See: http://inversed.ru/Blog_2.htm
#' Are the vertical bands visible across the full vertical axis?
#'
#' 4. 5. Two images of the 'volcano' elevation data in R using forward/reverse
#' colors. Try to find the highest point on the volcano peak.  Many palettes
#' with dark colors at one end of the palette hide the peak (e.g. viridis).
#' Also try to decide if the upperleft and upperright corners are the same color.
#' 
#' @param pal A palette function or a vector of colors.
#'
#' @param title Title to display at the top of the test image
#' 
#' @return None.
#' @export 
#' @examples
#' pal.test(parula) # fails contrast 1
#' pal.test(viridis)
#' pal.test(coolwarm)
#' 
#' @references
#' # See links above.
#' 
pal.test <- function(pal, title=substitute(pal)){

  op <- par(mfrow=c(2,3),
            oma=c(0,0,2,0), # save space for title
            mar=c(2,2,1,1), bg="gray80")

  if(is.function(pal)) {
    # pal is a function
    n <- 150
    cols64 <- pal(64)
    cols <- pal(n)
  } else {
    n <- length(pal)
    cols <- pal
    cols64 <- colorRampPalette(pal)(64)
  }

  
  # Z-curve 64
  zval <- matrix(c( 0, 1, 4 ,5,16,17,20,21,
                    2, 3, 6 ,7,18,19,22,23,
                    8, 9,12,13,24,25,28,29,
                   10,11,14,15,26,27,30,31,
                   32,33,36,37,48,49,52,53,
                   34,35,38,39,50,51,54,55,
                   40,41,44,45,56,57,60,61,
                   42,43,46,47,58,59,62,63)+1, byrow=TRUE, ncol=8)
  # Use t() and 8:1 to match Peter Karpov
  image(t(zval[8:1,]), col=cols64, axes=FALSE)

  
  # Campbell-Robson Contrast Sensitivity Chart
  # Are the vertical bands visible across the full vertical axis?
  # Do the vertical bands blur together
  x <- seq(0,3*pi,length=200)
  y <- seq(0,2*pi,length=200)
  z <- outer(x,y, function(x,y) cos(x^2)/exp(y))
  image(z, col=cols, axes=FALSE)


  ## # Frequency ramp suggested by
  ## # http://inversed.ru/Blog_2.htm
  ## x <- seq(0,2*pi,length=200)
  ## y <- seq(0,2,length=200)
  ## z <- 5 + outer(x,y, function(x,y) y + x^2 * sin(64*y) / 12)
  ## image(z, col=cols, axes=FALSE)
  pal.sineramp(pal=cols, nx=400, wavelen=10)

  # Volcano
  #image(volcano, col=cols, xaxt="n", yaxt="n")
  #image(volcano, col=rev(cols), xaxt="n", yaxt="n")
  paltest.volcano(cols)
  paltest.volcano(rev(cols))

  
  # RGB curves
  x <- 1:n
  colrgb <- col2rgb(cols)
  yr <- colrgb['red',]
  yg <- colrgb['green',]
  yb <- colrgb['blue',]
  ygr <- col2rgb(colorspace::desaturate(cols))['red',]
  
  plot(x,yr,col="red",ylim=c(0,255),type="l",lwd=2,xlab="",ylab="")
  lines(x,yg,col="forestgreen",lwd=2)
  lines(x,yb,col="blue",lwd=2)
  lines(x,ygr,col="gray30",lwd=2)
  # Here I tried to show the luminosity, but it was almost the
  # same as the desaturated line.
  # Also, viridis() returns colors with alpha levels "#FDE725FF"
  # which failed in hex2RGB (doesn't like alpha level)
  # LUV scale is 0-100, so multiply by 2.55
  # luv <- as(colorspace::hex2RGB(cols), "LUV")
  # lines(x,luv@coords[,1] * 2.5 , col="white") 

  
  # Title. What to do if it is a vector instead???
  # browser()
  if(!is.null(title)) {
    title(title, outer=TRUE)
  }
  
  on.exit(op)
  par(op)
}

# ----------------------------------------------------------------------------

#' Test a palette of colors with a sineramp
#'
#' Test palette with a sineramp
#'
#' Interpretation: Ideally, the sine wave should be equally visible across the
#' entire image. Further, at the bottom of the image there should be
#' no identifiable features, only a smooth ramp.  The 'jet' palette fails both.
#'
#' The test image shows a sine wave superimposed on a ramp of the palette.  The
#' amplitude of the sine wave is dampened/modulated from full at the top to 0
#' at the bottom.
#' The ramp function that the sine wave is superimposed upon is adjusted slightly
#' for each row so that each row of the image spans the full data range of 0 to 255.
#' The wavelength is chosen to create a stimulus that is aligned with the
#' capabilities of human vision.  For the default amplitude of 12.5, the trough
#' to peak distance is 25, which is about 10 percent of the 256 levels of the ramp.
#' Some color palettes (like 'jet') have perceptual flat areas that can hide
#' fluctations/features of this magnitude.
#' 
#' @param pal A palette function or a vector of colors.
#' 
#' @param n The number of colors to display for palette functions.
#' 
#' @param nx Number of 'pixels' horizontally (approximate).
#' 
#' @param ny Number of 'pixels' vertically
#' 
#' @param amp Amplitude of sine wave, default 12.5
#' 
#' @param wavelen Wavelength of sine wave, in pixels, default 8.
#' 
#' @param pow Power for dampening the sine wave. Default 2. For no dampening, use 0.
#' For linear dampening, use 1.
#' 
#' @return None
#' @author Concept by Peter Kovesi. R code by Kevin Wright.
#' @examples 
#' pal.sineramp(jet) # Very poor in green areas
#' pal.sineramp(parula)
#' pal.sineramp(brewer.greys(100))
#' 
#' # Show Kovesi's colormaps
#' \dontrun{
#' library(spatstat)
#' data(Kovesi)
#' for(i in 1:41) { pal.sineramp(Kovesi$values[[i]]); title(i) }
#' pal.sineramp(gnuplot) ; title('gnuplot')
#' pal.sineramp(Kovesi$values[[29]]); title('Kovesi 29')
#' }
#' @references 
#' Peter Kovesi (2015). Good Colour Maps: How to Design Them.
#' http://arxiv.org/abs/1509.03700.
#'
#' Peter Kovesi. A set of perceptually uniform color map files.
#' http://peterkovesi.com/projects/colourmaps/index.html
#'
#' Peter Kovesi. CET Perceptually Uniform Colour Maps: The Test Image.
#' http://peterkovesi.com/projects/colourmaps/colourmaptestimage.html
#' 
#' Original Julia version by Peter Kovesi from:
#' https://github.com/peterkovesi/PerceptualColourMaps.jl/blob/master/src/utilities.jl
#' @export 
pal.sineramp <- function(pal, n=150, nx=512, ny=256,
                         amp=12.5, wavelen=8, pow=2) {

  if(is.function(pal)) pal <- pal(n)
  
  
  # Adjust width of image so there is an integer number of cycles of
  # the sinewave.  Helps for cyclic color palette.
  # May still be a slight discontinuity along the edge.
  cycles = round(nx/wavelen)
  nx = cycles*wavelen
  
  # Sine wave
  xval = 0:(nx-1)
  fx = amp*sin( 1.0/wavelen * 2*pi*xval)

  # Vertical dampening of the wave
  img = outer(fx, seq(0,1,length=ny), function(x,y) x*y^pow)

  # Add ramp across entire image
  img = img + outer(seq(0,1,length=nx), seq(1,1,length=ny), '*') * (255-2*amp)

  # Normalise each row (offset and rescale into [0,1]). Important for cyclic
  # color maps
  img <- apply(img, 2, function(x){
    x = x - min(x) # set smallest value to 0
    x = x/max(x) # set largest value to 1
    x
  })

  image(img, col=pal, axes=FALSE)
  invisible()
}

# ----------------------------------------------------------------------------

#' Compress a color palette function to fewer colors
#'
#' Compress a color palette function to fewer colors
#'
#' Many color palette functions are defined with more colors than needed.
#' This function compresses a color palette function down to a sample
#' of colors that can be passed into 'colorRampPalette' and re-create the
#' original palette with a just-noticeable-difference.
#'
#' Color palettes that are defined as a smoothly varying ramp between a set of
#' colors often compress quite well.
#' Color palettes that are defined by functions may not compress well.
#' See the examples.
#'
#' @param pal A palette function or a vector of colors.
#' 
#' @param n Initial number of colors to use.
#' 
#' @param thresh Maximum allowable Lab distance from original palette
#' 
#' @return A vector of colors
#' @author Kevin Wright
#' @examples
#' # The 'cm.colors' palette compresses to only 3 colors
#' cm2 <- pal.compress(cm.colors, n=3)
#' pal.bands(cm.colors(255), colorRampPalette(cm2)(255), cm2)
#' title("cm.colors")
#' 
#' # The 'heat.colors' palette needs 84 colors
#' heat2 <- pal.compress(heat.colors, n=3)
#' pal.bands(heat.colors(255), colorRampPalette(heat2)(255), heat2)
#' title("heat.colors")
#' 
#' # The 'topo.colors' palette needs 249 colors because of the discontinuity
#' # topo2 <- pal.compress(topo.colors, n=3)
#' # pal.bands(list(topo.colors(255), colorRampPalette(topo2)(255), topo2))
#' # title("topo.colors")
#' 
#' # palettes that are defined by functions are sometimes difficult to compress
#' # into a colorRampPalette with a small number of colors
#' pal.bands(terrain.colors(11), terrain.colors(21), terrain.colors(31),
#'           terrain.colors(41), terrain.colors(51),
#'           cm.colors(11), cm.colors(21), cm.colors(31), cm.colors(41), cm.colors(51),
#'           labels=c("11","21","31","41","51","11","21","31","41","51"))
#'
#' # smooth palettes are easier to compress
#' p1 <- coolwarm(255)
#' p2 <- colorRampPalette( coolwarm(15) )(255)
#' pal.bands(p1,p2,coolwarm(15), labels=c("coolwarm","colorRampPalette 15","15"))
#' pal.maxdist(p1,p2) # 2.33
#'  
#' @references 
#' None
#' @export 
pal.compress <- function(pal, n=5, thresh=2.5) {
  # pal is a function
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

# ----------------------------------------------------------------------------

#' Measure the pointwise distance between two palettes
#'
#' Measure the pointwise distance between two palettes
#'
#' The distance between two palettes (of equal length) is calculated pointwise using
#' the Lab color space.  A 'just noticeable difference' between colors is roughly 2.3.
#'
#' @param pal1 A color palette (function or vector)
#' 
#' @param pal2 A color palette (function or vector)
#' 
#' @param n Number of colors to use, default 255
#' 
#' @return A vector of n distances.
#' @author Kevin Wright
#' @examples 
#' pa0 <- c("#ff0000","#00ff00","#0000ff")
#' pa1 <- c("#fa0000","#00fa00","#0000fa") # 2.4
#' pa2 <- c("#f40000","#00f400","#0000f4") # 5.2
#' pal.dist(pa0,pa1) # 1.87, 2.36, 2.11
#' pal.dist(pa0,pa2) # 4.12 5.20 4.68
#' pal.bands(pa1,pa0,pa2, labels=c("1.87  2.36  2.11","0","4.12  5.20  4.68"))
#' title("Lab distances from middle palette")
#' @references
#' https://en.wikipedia.org/wiki/Color_difference
#' @export 
pal.dist <- function(pal1, pal2, n=255){
  
  # Convert from function to vector
  if(is.function(pal1) & is.function(pal2)) {
    pal1 <- pal1(n)
    pal2 <- pal2(n)
  }
  
  if(length(pal1) != length(pal2)) stop("Palettes must have same length")

  # https://en.wikipedia.org/wiki/Color_difference
  # Use CIE76 formula. Just noticeable difference is 2.3
  p1 <- convertColor(t(col2rgb(pal1)), from="sRGB",to="Lab",scale.in=255)
  p2 <- convertColor(t(col2rgb(pal2)), from="sRGB",to="Lab",scale.in=255)
  delta <- apply((p1-p2), 1, function(x) sqrt(sum(x^2)))
  return(delta)
}

# ----------------------------------------------------------------------------

#' Measure the maximum distance between two palettes
#'
#' Measure the maximum distance between two palettes
#'
#' The distance between two palettes (of equal length) is calculated pointwise using
#' the Lab color space.  A 'just noticeable difference' between colors is roughly 2.3.
#'
#' @param pal1 A color palette (function or vector)
#' 
#' @param pal2 A color palette (function or vector)
#' 
#' @param n Number of colors to use, default 255
#' 
#' @return Numeric value of the maximum distance.
#' @author Kevin Wright
#' @examples 
#' pa0 <- c("#ff0000","#00ff00","#0000ff")
#' pa1 <- c("#fa0000","#00fa00","#0000fa") # 2.4
#' pa2 <- c("#f40000","#00f400","#0000f4") # 5.2
#' pal.maxdist(pa0,pa1) # 2.36
#' pal.maxdist(pa0,pa2) # 5.20
#' pal.bands(pa1,pa0,pa2, labels=c("2.36","0","5.20"))
#' title("Maximum Lab distance from middle palette")
#' @references
#' https://en.wikipedia.org/wiki/Color_difference
#' @export 
pal.maxdist <- function(pal1, pal2, n=255) max(pal.dist(pal1, pal2, n))

# ----------------------------------------------------------------------------

#' Discrete palettes
#'
#' Color palettes designed for discrete, categorical data with a small number of
#' categories.
#'
#' The \code{alphabet} palette has 26 distinguishable colors that have logical names
#' starting with the letters A, B, ... Z.
#' This palette is based on the work by Green-Armytage (2010), but uses the
#' names 'orange' instead of 'orpiment', and 'magenta' instead of 'mallow'. 
#'
#' The \code{cols25} palette was created experimentally by Wright (unpublished)
#' to create a set of colors that are distinct.
#' 
#' The \code{glasbey} palette by Glasbey et al (2007) has 32 colors that are
#' maximally distinct. Glasbey has 'white' as the second color, but in this
#' version of the palette, the color 'white' is moved to the end, and is
#' actually light-gray, #F2F3F4.
#' There exists a 256-color palette palette of 'distinct' colors.
#' We think this is absurd--even 32 colors are hard to distinguish.
#'
#' The \code{kelly} palette of 22 colors maximize the contrast
#' between colors in a set if the colors are chosen in sequential order.
#' Kelly paid attention to the needs of people with color blindness.  The
#' first nine colors work well for such people and people with normal vision.
#' Kelly did not provide RGB color values, and the paper was in black-and-white.
#' A color image of the Kelly palette can be found in Green-Armytage (2010).
#' The color values used here are from
#' http://hackerspace.kinja.com/iscc-nbs-number-hex-r-g-b-263-f2f3f4-242-243-244-267-22-1665795040
#' The color 'white' has been re-defined as light-gray, #F2F3F4.
#' Commentary: We think the kelly palette has an over-abundance of orange-ish
#' colors, the purples are not very distinct, etc.  See the pal.heatmap
#' example.
#'
#' The \code{stepped} palette has 20 colors (5 hues, 4 levels within each hue)
#' that is useful for showing varying levels within categories.
#' In order to better separate these colors in RGB space, we
#' moved red hue 0 to hue 350, green hue 80 to hue 90 
#' 
#' The \code{tol} palette has 12 colors by Paul Tol.
#' Any printer that conforms to international standard ISO 12647-2 can
#' reproduce all colours.
#' 
#' @param n Number of colors to return.
#' @return A vector of colors.
#' @author Palette colors by various authors. R code by Kevin Wright.
#' @examples
#'
#' # ----- alphabet -----
#' alphabet()
#' alphabet()["jade"]
#' pal.cube(alphabet)
#' pal.bands(alphabet,n=26)
#' pal.map(alphabet)
#' pal.heatmap(alphabet)
#'
#' # ----- cols25 -----
#' pal.bands(cols25,n=25)
#' pal.heatmap(cols25)
#'
#' # ------ glasbey ------
#' pal.bands(glasbey,n=32)
#' pal.cube(glasbey, n=32) # Blues are close together
#' pal.heatmap(glasbey(32))
#'
#' # ----- kelly -----
#' pal.bands(kelly,n=22)
#' pal.heatmap(kelly(22))
#'
#' # ----- stepped -----
#' pal.bands(stepped,n=20)
#' pal.heatmap(stepped, n=20)
#' 
#' # ----- tol -----
#' pal.bands(tol,n=12)
#' pal.heatmap(tol, 12)
#' 
#' # ----- watlington -----
#' pal.bands(watlington,n=16)
#' pal.heatmap(watlington(16))

#' @references
#' 
#' Robert M. Boynton. (1989)
#' Eleven Colors That Are Almost Never Confused. 
#' Proc. SPIE 1077, \emph{Human Vision, Visual Processing, and Digital Display}, 322-332.
#' http://doi.org/10.1117/12.952730
#' 
#' Chris Glasbey, Gerie van der Heijden, Vivian F. K. Toh, Alision Gray (2007).
#' Colour Displays for Categorical Images.
#' \emph{Color Research and Application}, 32, 304-309.
#' http://doi.org/10.1002/col.20327
#'
#' P. Green-Armytage (2010): A Colour Alphabet and the Limits of Colour Coding.
#' \emph{Colour: Design & Creativity} (5) (2010): 10, 1-23.
#' www.aic-color.org/journal/v5/jaic_v5_06.pdf
#'
#' K. Kelly (1965): Twenty-two colors of maximum contrast.
#' \emph{Color Eng}., 3(6), 1965.
#' http://www.iscc.org/pdf/PC54_1724_001.pdf
#' 
#' Paul Tol (2012). Color Schemes. SRON technical note, SRON/EPS/TN/09-002.
#' https://personal.sron.nl/~pault/
#'
#' John Watlington.
#' An Optimum 16 Color Palette.
#' http://alumni.media.mit.edu/~wad/color/palette.html
#' 
#' Color Schemes Appropriate for Scientific Data Graphics
#' http://geog.uoregon.edu/datagraphics/color_scales.htm
#' 
#' @name discrete
NULL


#' @export
#' @rdname discrete
alphabet <- function(n=26) {
  
  if(n > 26){
    message("Only 26 colors are available with 'alphabet'")
    n <- 26
  }
  
  pal <- c(
    rgb(255, 204, 153, maxColorValue=255, names="honeydew"),
    rgb(148, 255, 181, maxColorValue=255, names="jade"),
    rgb( 94, 241, 242, maxColorValue=255, names="sky"),
    rgb(224, 255, 102, maxColorValue=255, names="uranium"),
    rgb(255, 255, 128, maxColorValue=255, names="xanthin"),
    rgb(240, 160, 255, maxColorValue=255, names="amethyst"),
    rgb(255, 225,   0, maxColorValue=255, names="yellow"),
    rgb(255, 168, 187, maxColorValue=255, names="pink"),
    rgb(157, 204,   0, maxColorValue=255, names="lime"),
    rgb(255, 164,   5, maxColorValue=255, names="orange"),
    rgb( 43, 206,  72, maxColorValue=255, names="green"),
    rgb(194,   0, 136, maxColorValue=255, names="magenta"),
    rgb(255,   0,  16, maxColorValue=255, names="red"),
    rgb(  0, 153, 143, maxColorValue=255, names="turquoise"),
    rgb(116,  10, 255, maxColorValue=255, names="violet"),
    rgb(255,  80,   5, maxColorValue=255, names="zinnia"),
    rgb(  0, 117, 220, maxColorValue=255, names="blue"),
    rgb(128, 128, 128, maxColorValue=255, names="iron"),
    rgb(153,  63,   0, maxColorValue=255, names="caramel"),
    rgb(143, 124,   0, maxColorValue=255, names="khaki"),
    rgb( 66, 102,   0, maxColorValue=255, names="quagmire"),
    rgb(153,   0,   0, maxColorValue=255, names="wine"),
    rgb(  0,  92,  49, maxColorValue=255, names="forest"),
    rgb(  0,  51, 128, maxColorValue=255, names="navy"),
    rgb( 76,   0,  92, maxColorValue=255, names="damson"),
    rgb( 25,  25,  25, maxColorValue=255, names="ebony"),
    rgb(255, 255, 255, maxColorValue=255, names="white"))
  
  return(pal[1:n])
}

#' @export
#' @rdname discrete
watlington <- function(n=16) {
  if(n > 16) {
    message("Only 1 colors are available with 'watlington'.")
    n <- 16
  }
  return(syspals$watlington[1:n])
}

#' @export
#' @rdname discrete
cols25 <- function(n=25) {
  if(n > 25) {
    message("Only 25 colors are available with 'cols25'.")
    n <- 25
  }
  pal <- c(rgb(31,120,200,maxColorValue=255), # "#1f78b4",
           "#ff0000", # red
           "#33a02c",
           rgb(106,51,194,maxColorValue=255), # "#6a3d9a", # mauve
           "#ff7f00",
           "#000000", # "black",
           rgb(255,215,0,maxColorValue=255), # "gold1",
           "#a6cee3",
           rgb(251,100,150,maxColorValue=255), # "#FB9A99", # lt pink
           "#b2df8a",
           "#CAB2D6", # lt purple
           "#FDBF6F", # lt orange
           rgb(153, 153, 153, maxColorValue=255), # "gray60",
           rgb(238, 230, 133, maxColorValue=255), # "khaki2",
           rgb(200,48,140,maxColorValue=255), #"maroon",
           rgb(255,131, 250, maxColorValue=255), # "orchid1",
           rgb(200,20,250,maxColorValue=255), # "#ff0ac8", # deeppink1
           rgb(0, 0, 255, maxColorValue=255), # "blue1",
           rgb(54, 100, 139, maxColorValue=255), # "steelblue4",
           rgb(0,226,229,maxColorValue=255), # "darkturquoise",
           rgb(0, 255, 0, maxColorValue=255), # "green1",
           rgb(119,139,0,maxColorValue=255), # "yellow4",
           rgb(190,190,0,maxColorValue=255), #"yellow3",
           rgb(139,59,0,maxColorValue=255), # "darkorange4",
           rgb(165,42,60,maxColorValue=255) # "brown"
           )
  
  return(pal[1:n])
}


#' @export
#' @rdname discrete
glasbey <- function(n=32) {
  if(n > 32) {
    message("Only 32 colors are available with 'glasbey'.")
    n <- 32
  }

  return(syspals$glasbey[1:n])
}

#' @export
#' @rdname discrete
kelly <- function(n=22) {

  if(n > 22) {
    message("Only 22 colors are available with 'kelly'")
    n <- 22
  }

  pal = c("#F2F3F4", # white
          "#222222", # black
          "#F3C300", # yellow/gold
          "#875692", # purple
          "#F38400", # orange
          "#A1CAF1", # light blue
          "#BE0032", # red
          "#C2B280", # buff
          "#848482", # grey
          "#008856", # green
          "#E68FAC", # purplish pink
          "#0067A5", # blue
          "#F99379", # yellowish pink
          "#604E97", # violet
          "#F6A600", # orange yellow
          "#B3446C", # purplish red
          "#DCD300", # greenish yellow
          "#882D17", # reddish brown
          "#8DB600", # yellow green
          "#654522", # yellowish brown
          "#E25822", # reddish orange
          "#2B3D26"  # olive green
          )

  return(pal[1:n])
}

# ----------------------------------------------------------------------------

#' @export
#' @rdname discrete
stepped <- function(n=20) {
  
  if(n > 20) {
    message("Only 20 colors are available with 'stepped'")
    n <- 20
  }

  return(syspals$stepped[1:n])
}

# ----------------------------------------------------------------------------

#' @export
#' @rdname discrete
tol <- function(n=12) {
  if(n > 12){
    message("Only 12 colors are available with 'tol'")
    n <- 12
  }

  return(syspals$tol[[n]])
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

paltest.volcano <- function(pal, n=100){

  # need to fix...
  # wonky things can happen with filled.contour because it uses 'pretty'
  # for the 'approximate' number of levels
  
  if(is.function(pal)) {
    pal <- pal(n)
  } else {
    n=length(pal)
  }
  
  #filled.contour(volcano, col=pal, color.palette = pal, n=n+1, asp = 1, axes=0)
  image(datasets::volcano, col=pal, axes=FALSE, asp=1)
  
}

