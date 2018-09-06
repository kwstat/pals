# tools.R
# Time-stamp: <06 Sep 2018 14:16:52 c:/x/rpack/pals/R/tools.R>
# Copyright: Kevin Wright, 2017. License: GPL-3.

# ----------------------------------------------------------------------------
# pal.bands

#' Show palettes and colormaps as colored bands
#'
#' Show palettes as colored bands.
#' 
#' What to look for:
#' 
#' 1. A good discrete palette has distinct colors.
#' 
#' 2. A good continuous colormap does not show boundaries between colors.
#' For example, the \code{rainbow()} palette is poor, showing bright lines at
#' yellow, cyan, pink.
#' 
#' @param ... Palettes/colormaps, each of which is either
#' (1) a vectors of colors or
#' (2) a function returning a vector of colors.
#' 
#' @param n The number of colors to display for palette functions.
#' 
#' @param labels Labels for palettes
#'
#' @param main Title at top of page.
#'
#' @param gap Vertical gap between bars, default is 0.1
#'
#' @param sort
#' If sort="none", palettes are not sorted.
#' If sort="hue", palettes are sorted by hue.
#' If sort="luminance", palettes are sorted by luminance.
#' 
#' @param show.names If TRUE, show color names
#' 
#' @examples
#' pal.bands(c('red','white','blue'), rainbow)
#' 
#' op=par(mar=c(0,5,3,1))
#' pal.bands(cubehelix, gnuplot, jet, tol.rainbow, inferno,
#'   magma, plasma, viridis, parula, n=200, gap=.05)
#' par(op)
#' 
#' # Examples of sorting
#' labs=c('alphabet','alphabet2', 'glasbey','kelly','polychrome', 'watlington')
#' op=par(mar=c(0,5,3,1))
#' pal.bands(alphabet(), alphabet2(), glasbey(), kelly(),
#'   polychrome(), watlington(), sort="hue",
#'   labels=labs, main="sorted by hue")
#' par(op)
#' pal.bands(alphabet(), alphabet2(), glasbey(), kelly(),
#'   polychrome(), watlington(), sort="luminance",
#'   labels=labs, main="sorted by luminance")
#' 
#' @export
pal.bands <- function(..., n=100, labels=NULL, main=NULL, gap=0.1, sort="none", show.names=TRUE){

  #if(n < 3) warning("Using n=3")
  if(!is.element(sort, c("none","hue","luminance")))
    stop("'sort' must be one of 'none','hue','luminance'")
    
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

  # Now convert the colormap functions to palette vectors
  for(i in 1:npal) {
    if(isfun[i]) pals[[i]] <- pals[[i]](n)
  }
  # Count the number of boxes for each palette
  nc <- unlist(lapply(pals, length))

  # AFTER functions are converted to vectors, we can sort if needed
  if(sort=="hue"){
    for(i in 1:npal){
        hsvcol <- methods::as(colorspace::hex2RGB(pals[[i]]), "HSV")@coords
        pals[[i]] <- pals[[i]][order(hsvcol[,1],hsvcol[,2])]
    }
  }
  if(sort=="luminance"){
    for(i in 1:npal){
        cols <- methods::as(colorspace::hex2RGB(pals[[i]]), "LUV")@coords
        pals[[i]] <- pals[[i]][order(cols[,1],cols[,2])]
    }
  }
  
  
  maxn <- max(nc)
  ylim <- c(0, npal)
  # mgp: The margin line (in mex units) for the axis title, axis labels and axis line.
  oldpar <- par(mgp = c(2, 0.25, 0))
  on.exit(par(oldpar))
  plot(1, 1, xlim = c(0, maxn), ylim = ylim,
       type = "n", axes = FALSE, bty = "n", xlab = "", ylab = "")
  
  for (i in 1:npal) {
    # i goes bottom to top, npal+1-i goes top to bottom
    nj <- nc[npal+1 - i]
    shadi <- pals[[npal+1 - i]]
    brks <- seq(from=0, to=maxn, length=nj+1) # horiz break points between colors
    # the vertical height of each bar is 1-gap, with 'gap' white fraction at the top
    rect(xleft = brks[1:nj], ybottom = i-1,
         xright = brks[2:(nj+1)], ytop = i-gap, col = shadi, border = NA)
    
    # If inidividual colors in a palette have names, add them
    nms <- names(shadi)
    if(show.names & !is.null(nms)) {
      textcol <- ifelse(col2rgb(colorspace::desaturate(shadi))['red',] < 128,
                        "white", "black")
      text(brks[1:nj] + 0.5, i-.6, nms, srt=90, cex=.75, col=textcol)
    }
  }

  # Palette name along left side
  text(rep(-0.2, npal), (1:npal) - 0.6,
       labels = rev(labels),
       cex=0.6, xpd = TRUE, adj = 1)

  # Or, we could overlay the labels on top of the bands, using shadowtext
  # http://stackoverflow.com/questions/29303480/text-labels-with-outline-in-r
  # http://blog.revolutionanalytics.com/2009/05/make-text-stand-out-with-outlines.html
  
  if(!is.null(main)) title(main)

  invisible()
}
## if(FALSE){
## pal.bands(c('red','white','blue'),c('blue','yellow'), c('black','red','gold'), labels=c('USA','Sweden','Germany'))
## pal.bands(cm.colors, rainbow, topo.colors, heat.colors, c('red','blue'), n=31)
## pal.bands(alphabet)
## # pal.bands(alphabet,n=25) # omit black
## # pal.bands(alphabet,n=26)
## # pal.bands(alphabet,n=27)
## pal.bands(cubehelix, parula)
## pal.bands(alphabet,cols25,glasbey,kelly,stepped,tol,watlington)

## invisible()
## }

# ----------------------------------------------------------------------------
# pal.channels

#' Show the red, green, blue, gray amount in colors of a palette
#'
#' The amount of red, green, blue, and gray in colors are shown.
#'
#' What to look for:
#'
#' 1. Sequential data should usually be shown with a colormap that is smoothly
#' increasing in lightness, as shown by the gray line.
#'
#' @param pal A palette function or a vector of colors.
#' 
#' @param n The number of colors to display for palette functions.
#' 
#' @param main Main title.
#' 
#' @return None
#' 
#' @examples
#' pal.channels(parula)
#' pal.channels(coolwarm)
#' # pal.channels(glasbey) # Nonsensical.
#' 
#' @author Kevin Wright
#' 
#' @references 
#' None
#' 
#' @export 
pal.channels <- function(pal,n=150,main=""){
  
  if(is.function(pal)) {
    # pal is a function
    pal <- pal(n)
    n <- length(pal)
  } else {
    n <- length(pal)
  }
  
  x <- 1:n
  colrgb <- col2rgb(pal)
  yr <- colrgb['red',]
  yg <- colrgb['green',]
  yb <- colrgb['blue',]
  ygr <- col2rgb(colorspace::desaturate(pal))['red',]
  
  plot(x,yr,col="red",ylim=c(0,255),type="l",lwd=2,xlab="",ylab="")
  lines(x,yg,col="forestgreen",lwd=2)
  lines(x,yb,col="blue",lwd=2)
  lines(x,ygr,col="gray30",lwd=2)
  # Here I tried to show the luminosity, but it was almost the
  # same as the desaturated line.
  # Also, viridis() returns colors with alpha levels "#FDE725FF"
  # which failed in hex2RGB (doesn't like alpha level)
  # LUV scale is 0-100, so multiply by 2.55
  # luv <- methods::as(colorspace::hex2RGB(pal), "LUV")
  # lines(x,luv@coords[,1] * 2.5 , col="white") 

  if(!is.null(main)) title(main)
  
  invisible()
}


# ----------------------------------------------------------------------------
# pal.cluster

#' Show a palette with hierarchical clustering
#'
#' The palette colors are converted to LUV coordinates before clustering.
#' (RGB coordinates are available, but not recommended.)
#' 
#' What to look for:
#'
#' Colors that are visually similar tend to be clustered together.
#'
#' @param pal A palette function or a vector of colors.
#' 
#' @param n The number of colors to display for palette functions.
#' 
#' @param type Either "LUV" (default) or "RGB".
#' 
#' @param main Title to display at the top of the test image
#' 
#' @return None
#' 
#' @examples
#' pal.cluster(alphabet(), main="alphabet")
#' pal.cluster(glasbey, main="glasbey") # two royal blues are very similar
#' pal.cluster(kelly, main="kelly") # two black-ish colors are very similar
#' # pal.cluster(watlington, main="watlington")
#' # pal.cluster(coolwarm(15), main="coolwarm") # curiously, grey clusters with blue
#' 
#' @author Kevin Wright
#'
#' @references 
#' None
#' @importFrom stats dist hclust
#' @export 
pal.cluster <- function(pal, n=50, type="LUV", main=""){
  
  if(is.function(pal)) pal <- pal(n)
  
  if(type=="RGB") {
    x <- t(col2rgb(pal))
  } else if (type=="LUV") {
    luvmat <- methods::as(colorspace::hex2RGB(pal), "LUV")
    x <- luvmat@coords
  }
  
  hd <- hclust(dist(x, "euclidean"))
  plot(hd, hang = 0, labels = rep("", length(pal)), xlab="", main = main)
  if(is.null(names(pal))){
    # use hex color
    #labs <- paste0(pal, " [", 1:length(pal), "]")
    labs <- paste0(" [", seq_along(pal), "]")
  } else {
    labs <- names(pal)
  }
  
  mtext(labs, side = 1, line = 0, at = order(hd$order), col = pal, las = 2)
  
  invisible()
}


# ----------------------------------------------------------------------------
# pal.csf

#' Show a colormap with a Campbell-Robson Contrast Sensitivity Chart
#' 
#' In a contrast sensitivity figure as drawn by this function, the 
#' spatial frequency increases from left to right and the contrast decreases
#' from bottom to top.  The bars in the figure appear taller in the middle 
#' of the image than at the edges, creating an upside-down "U" shape, which 
#' is the "contrast sensitivity function".  
#' Your perception of this curve depends on the viewing distance.
#' 
#' What to look for:
#' 
#' 1. Are the vertical bands visible across the full vertical axis?
#' 
#' 2. Do the vertical bands blur together?
#'  
#' @param pal A continuous colormap function
#' 
#' @param n The number of colors to display for palette functions.
#' 
#' @param main Main title.
#'
#' @return None
#'
#' @examples
#' pal.csf(brewer.greys) # Classic example from psychology
#' pal.csf(parula)
#' 
#' @author Kevin Wright
#' 
#' @references 
#' 
#' Izumi Ohzawa. Make Your Own Campbell-Robson Contrast Sensitivity Chart.
#' http://ohzawa-lab.bpe.es.osaka-u.ac.jp/ohzawa-lab/izumi/CSF/A_JG_RobsonCSFchart.html
#' 
#' Campbell, F. W. and Robson, J. G. (1968).
#' Application of Fourier analysis to the visibility of gratings. 
#' \emph{Journal of Physiology}, 197: 551-566.
#' 
#' @export
pal.csf <- function(pal, n=150, main=""){
  if(is.function(pal)) pal <- pal(n)

  x <- seq(0,5*pi,length=400)
  y <- seq(0,2*pi,length=400)
  z <- outer(x,y, function(x,y) cos(x^2)/exp(y))
  image(z, col=pal, axes=FALSE)

  if(!is.null(main)) title(main)
  
  invisible()
}

# ----------------------------------------------------------------------------
# pal.compress

#' Compress a colormap function to fewer colors
#'
#' Compress a colormap function to fewer colors
#'
#' Colormap functions are often defined with many more colors than needed.
#' This function compresses a colormap function down to a sample
#' of colors that can be passed into 'colorRampPalette' and re-create the
#' original palette with a just-noticeable-difference.
#'
#' Colormaps that are defined as a smoothly varying ramp between a set of
#' colors often compress quite well.
#' Colormaps that are defined by functions may not compress well.
#'
#' @param pal A colormap function or a vector of colors.
#' 
#' @param n Initial number of colors to use for the basis.
#' 
#' @param thresh Maximum allowable Lab distance from original palette
#' 
#' @return A vector of equally-spaced colors that form the 'basis' of a colormap.
#'
#' @examples
#' # The 'cm.colors' palette in R compresses to only 3 colors
#' cm2 <- pal.compress(cm.colors, n=3)
#' pal.bands(cm.colors(255), colorRampPalette(cm2)(255), cm2,
#' labels=c('original','compressed','basis'), main="cm.colors")
#' 
#' # The 'heat.colors' palette needs 84 colors
#' heat2 <- pal.compress(heat.colors, n=3)
#' pal.bands(heat.colors(255), colorRampPalette(heat2)(255), heat2,
#' labels=c('original','compressed','basis'), main="heat.colors")
#' 
#' # The 'topo.colors' palette needs 249 colors because of the discontinuity
#' # topo2 <- pal.compress(topo.colors, n=3)
#' # pal.bands(topo.colors(255), colorRampPalette(topo2)(255), topo2,
#' # labels=c('original','compressed','basis'), main="topo.colors")
#' 
#' # smooth palettes usually easy to compress
#' p1 <- coolwarm(255)
#' cool2 <- pal.compress(coolwarm)
#' p2 <- colorRampPalette(cool2)(255)
#' pal.bands(p1, p2, cool2,
#' labels=c('original','compressed', 'basis'), main="coolwarm")
#' pal.maxdist(p1,p2) # 2.33
#'  
#' @author Kevin Wright
#'
#' @references 
#' None. 
#' @export 
pal.compress <- function(pal, n=5, thresh=2.5) {
  # pal is a function

  # 255 equal-spaced colors from the original palette function
  pal255 <- pal(255)
  
  done <- FALSE
  while(!done) {
    
    # 255 colors expanded from n colors
    palc <- colorRampPalette(pal(n))(255)

    # Compare 255 colors from the original palette with
    #         255 colors using the n basis colors
    # If they are too far apart, increase n and try again
    p1 <- convertColor(t(col2rgb(pal255)), from="sRGB",to="Lab",scale.in=255)
    p2 <- convertColor(t(col2rgb(palc)), from="sRGB",to="Lab",scale.in=255)
    delta <- max(apply((p1-p2), 1, function(x) sqrt(sum(x^2))))
    if(delta >  thresh) n <- n+1 else done <- TRUE
  }
  return(pal(n))
}

# ----------------------------------------------------------------------------
# pal.cube

#' Show one palette/colormap in three dimensional RGB or LUV space
#'
#' The palette is converted to RGB or LUV coordinates
#' and plotted in a three-dimensional scatterplot.
#' The LUV space is probably better, but it is easier to tweak colors by
#' hand in RGB space.
#' 
#' What to look for:
#' 
#' A good palette has colors that are spread somewhat uniformly in 3D.
#' 
#' @param pal A palette/colormap function or a vector of colors.
#' 
#' @param n The number of colors to display for palette functions.
#' 
#' @param label If TRUE, show color name/value on plot
#'
#' @param type Either "RGB" (default) or "LUV".
#' 
#' @return None
#' @importFrom rgl plot3d text3d
#' @export 
#' @examples
#' \dontrun{
#' pal.cube(cubehelix)
#' pal.cube(glasbey, n=32) # RGB, blues are too close to each other
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
    xl <- "red"; yl <- "green"; zl <- "blue"
  } else if (type=="LUV") {
    luvmat <- methods::as(colorspace::hex2RGB(pal), "LUV")
    x <- luvmat@coords
    xl <- "L"; yl <- "U"; zl <- "V"
  }

  plot3d(x, col=pal,
         xlab=xl, ylab=yl,zlab=zl,
         lit=FALSE,
         size=1.5, type='s')
  if(label)
    text3d(x, texts=pal, cex=0.8)

  invisible()
}

# ----------------------------------------------------------------------------
# pal.dist

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
#' 
#' @examples 
#' pa0 <- c("#ff0000","#00ff00","#0000ff")
#' pa1 <- c("#fa0000","#00fa00","#0000fa") # 2.4
#' pa2 <- c("#f40000","#00f400","#0000f4") # 5.2
#' pal.dist(pa0,pa1) # 1.87, 2.36, 2.11
#' pal.dist(pa0,pa2) # 4.12 5.20 4.68
#' pal.bands(pa1,pa0,pa2, labels=c("1.87  2.36  2.11","0","4.12  5.20  4.68"))
#' title("Lab distances from middle palette")
#' 
#' @author Kevin Wright
#' 
#' @references
#' https://en.wikipedia.org/wiki/Color_difference
#' 
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

  return()
}

# ----------------------------------------------------------------------------
# pal.maxdist

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
#' 
#' @examples 
#' pa0 <- c("#ff0000","#00ff00","#0000ff")
#' pa1 <- c("#fa0000","#00fa00","#0000fa") # 2.4
#' pa2 <- c("#f40000","#00f400","#0000f4") # 5.2
#' pal.maxdist(pa0,pa1) # 2.36
#' pal.maxdist(pa0,pa2) # 5.20
#' pal.bands(pa1,pa0,pa2, labels=c("2.36","0","5.20"))
#' title("Maximum Lab distance from middle palette")
#'
#' # distance between colormap functions
#' pal.maxdist(coolwarm,warmcool)
#' 
#' @author Kevin Wright
#' 
#' @references
#' https://en.wikipedia.org/wiki/Color_difference
#' @export 
pal.maxdist <- function(pal1, pal2, n=255) max(pal.dist(pal1, pal2, n))

# ----------------------------------------------------------------------------
# pal.heatmap

#' Show a palette/colormap with a heatmap
#'
#' 
#' @param pal A palette function or a vector of colors.
#' 
#' @param n The number of squares vertically in the heatmap.
#'
#' @param miss Fraction of squares with missing values, default .05.
#'
#' @param main Main title
#' 
#' @return None.
#' @export 
#' @examples
#' pal.heatmap(brewer.paired, n=12)
#' pal.heatmap(coolwarm, n=12)
#' pal.heatmap(tol, n=12)
#' pal.heatmap(glasbey, n=32)
#' pal.heatmap(kelly, n=22, main="kelly", miss=.25)
#' 
#' @author Kevin Wright
#'
#' @references
#' None
#' 
#' @importFrom stats runif
pal.heatmap <- function(pal, n=25, miss=.05, main=""){

  if(miss >  1)
    stop("`miss` should be less than 1.")
  
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
  if(main != "") mtext(main)
  invisible()
}

# ----------------------------------------------------------------------------
# pal.safe

#' Show a palette/colormap for black/white and colorblind safety
#'
#' A single palette/colormap is shown 
#' (1) without any modifications 
#' (2) in black-and-white as if photocopied 
#' (3) as seen by deutan color-blind
#' (4) as seen by protan color-blind
#' (5) as seen by tritan color-blind
#'
#' Rates of colorblindness in women are low, but in men the rates are
#' around 3 to 7 percent, depending on the race.
#' 
#' What to look for:
#' 
#' 1. Are colors still unique when viewed in less-than full color? 
#' 
#' 2. Is a sequential colormap still sequential?
#'
#' @param pal A palette function or a vector of colors.
#' 
#' @param n The number of colors to display for palette functions.
#' 
#' @param main Title to display at the top of the test image
#' 
#' @return
#' None.
#' 
#' @examples 
#' pal.safe(glasbey)
#' pal.safe(rainbow, main="rainbow") # Really, really bad
#' pal.safe(cubicyf(100), main="cubicyf")
#' pal.safe(parula, main="parula")
#' 
#' @author Kevin Wright
#'
#' @references
#'
#' Vischeck. \url{http://www.vischeck.com/vischeck/}
#' 
#' None
#' @export
#' @importFrom colorspace desaturate
#' @importFrom dichromat dichromat
pal.safe <- function(pal, n=100, main=NULL){

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

  if(!is.null(main)) title(main)

  invisible()
}


# ----------------------------------------------------------------------------
# pal.scatter

#' Show a colormap with a scatterplot
#'
#' What to look for:
#' 
#' 1. Can the colors of each point be uniquely identified?
#' 
#' @param pal A palette function or a vector of colors.
#' 
#' @param n The number of colors to display for palette functions.
#'
#' @param main Main title
#' 
#' @return
#' None.
#' @examples
#' pal.scatter(glasbey, n=31, main="glasbey") # FIXME add legend
#' pal.scatter(parula, n=10) # not a good choice
#' 
#' @author Kevin Wright
#' 
#' @references
#' None.
#' @export 
pal.scatter <- function(pal, n=50, main=""){
  if(is.function(pal)) pal <- pal(n)
  
  plot(runif(100), runif(100), col=pal, pch=16,
       xlab="", ylab="",
       xlim=c(0,1), ylim=c(0,1))
  # Need to add a key

  if(main!="") mtext(main)
  
  invisible()
}

# ----------------------------------------------------------------------------
# pal.sineramp

#' Show a colormap with a sineramp
#'
#' The test image shows a sine wave superimposed on a ramp of the palette.  The
#' amplitude of the sine wave is dampened/modulated from full at the top
#' of the image to 0 at the bottom.
#' 
#' The ramp function that the sine wave is superimposed upon is adjusted slightly
#' for each row so that each row of the image spans the full data range of 0 to 255.
#' The wavelength is chosen to create a stimulus that is aligned with the
#' capabilities of human vision.  For the default amplitude of 12.5, the trough
#' to peak distance is 25, which is about 10 percent of the 256 levels of the ramp.
#' Some color palettes (like 'jet') have perceptual flat areas that can hide
#' fluctuations/features of this magnitude.
#' 
#' What to look for:
#' 
#' 1. Is the sine wave equally visible horizontally across the entire image?
#' 
#' 2. At the bottom, is the ramp smooth, or are there features like vertical bands?
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
#' @param main Main title
#' 
#' @return None
#' 
#' @examples 
#' pal.sineramp(parula)
#' pal.sineramp(jet) # Bad: Indistinct wave in green at top. Mach bands at bottom.
#' pal.sineramp(brewer.greys(100))
#' 
#' @author Concept by Peter Kovesi. R code by Kevin Wright.
#' 
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
#' 
#' @export 
pal.sineramp <- function(pal, n=150, nx=512, ny=256,
                         amp=12.5, wavelen=8, pow=2, main="") {

  if(is.function(pal)) pal <- pal(n)
  
  
  # Adjust width of image so there is an integer number of cycles of
  # the sinewave.  Helps for cyclic color palette.
  # May still be a slight discontinuity along the edge.
  cycles <- round(nx/wavelen)
  nx <- cycles*wavelen
  
  # Sine wave
  xval <- 0:(nx-1)
  fx <- amp*sin( 1.0/wavelen * 2*pi*xval)

  # Vertical dampening of the wave
  img <- outer(fx, seq(0,1,length=ny), function(x,y) x*y^pow)

  # Add ramp across entire image
  img <- img + outer(seq(0,1,length=nx), seq(1,1,length=ny), '*') * (255-2*amp)

  # Normalise each row (offset and rescale into [0,1]). Important for cyclic
  # color maps
  img <- apply(img, 2, function(x){
    x <- x - min(x) # set smallest value to 0
    x <- x/max(x) # set largest value to 1
    x
  })

  image(img, col=pal, axes=FALSE)

  if(main!="") mtext(main)
  
  invisible()
}

# ----------------------------------------------------------------------------
# pal.test

#' Show a colormap with multiple images
#'
#' 1. Z-curve
#' 
#' 2. Contrast Sensitivity Function.
#'
#' 3. Frequency ramp. See: http://inversed.ru/Blog_2.htm
#' Are the vertical bands visible across the full vertical axis?
#'
#' 4. 5. Two images of the 'volcano' elevation data in R using forward/reverse
#' colors. Try to find the highest point on the volcano peak.  Many palettes
#' with dark colors at one end of the palette hide the peak (e.g. viridis).
#' Also try to decide if the upperleft and upperright corners are the same color.
#'
#' 6. Luminosity in red, green, blue, and grey.
#' 
#' @param pal A palette function or a vector of colors.
#'
#' @param main Title to display at the top of the test image
#' 
#' @return None.
#' 
#' @export 
#' @examples
#' pal.test(parula)
#' pal.test(viridis) # dark colors are poor
#' pal.test(coolwarm)
#' 
#' @author Kevin Wright
#' 
#' @references
#' # See links above.
#' 
pal.test <- function(pal, main=substitute(pal)){

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

  # Space-filling z-curve
  pal.zcurve(pal=cols64, n=64)  
  
  # Campbell-Robson Contrast Sensitivity Chart
  pal.csf(pal=cols)

  # Frequency ramp
  pal.sineramp(pal=cols, nx=400, wavelen=10)

  # Volcano
  pal.volcano(cols)
  pal.volcano(rev(cols))
  
  # RGB curves
  pal.channels(cols)
    
  # Title. What to do if it is a vector instead???
  # browser()
  if(!is.null(main)) {
    title(main, outer=TRUE)
  }
  
  on.exit(op)
  par(op)

  invisible()
}

# ----------------------------------------------------------------------------
# pal.volcano

#' Show a colormap with a surface of volcano elevation
#'
#' Some palettes with dark colors at one end of the palette hide the 
#' shape of the volcano in the dark colors. Viridis is bad.
#' 
#' What to look for:
#' 
#' 1. Can you locate the highest point on the volcano?
#' 
#' 2. Are the upper-right and lower-right corners the same elevation?
#' 
#' 3. Do any Mach bands circle the peak?
#' 
#' @param pal A palette function or a vector of colors.
#' 
#' @param n The number of colors to display for palette functions.
#'
#' @param main Main title
#'
#' @return None.
#' 
#' @export
#'
#' @examples
#' pal.volcano(parula)
#' pal.volcano(brewer.rdbu) # Mach banding is bad
#' pal.volcano(warmcool, main="warmcool") # No Mach band 
#' pal.volcano(rev(viridis(100))) # Bad: peak position is hidden
#' 
pal.volcano <- function(pal, n=100, main=""){
  
  # need to fix...
  # wonky things can happen with filled.contour because it uses 'pretty'
  # for the 'approximate' number of levels
  
  if(is.function(pal)) {
    pal <- pal(n)
  } else {
    n <- length(pal)
  }
  
  #filled.contour(volcano, col=pal, color.palette = pal, n=n+1, asp = 1, axes=0)
  image(datasets::volcano, col=pal, axes=FALSE, asp=1)

  if(main!="") mtext(main)
  
  invisible()
}


# ----------------------------------------------------------------------------
# pal.zcurve

#' Show a colormap with a space-filling z-curve
#'
#' Construct a Z-order curve, coloring cells with a colormap.
#' The difference in color between squares side-by-side is 1/48 of the full range.
#' The difference in color between one square atop another is 1/96 the full range.
#' 
#' What to look for:
#'  
#' 1. A good color palette of 64 colors should be able to resolve 4 sub-squares 
#' within each of the 16 squares. 
#' 
#' @param pal A continuous color palette function
#' 
#' @param n Number of squares for the z-curve
#'
#' @param main Main title
#'
#' @return None
#' 
#' @examples
#' pal.zcurve(parula,n=4,main="parula")
#' pal.zcurve(parula,n=16)
#' pal.zcurve(parula,n=64)
#' pal.zcurve(parula,n=256)
#' 
#' @author Kevin Wright.
#' 
#' @references 
#' 
#' Peter Karpov. 2016.
#' In Search Of A Perfect Colormap. https://twitter.com/inversed_ru
#' 
#' Z-order curve. https://en.wikipedia.org/wiki/Z-order_curve
#' @export
pal.zcurve <- function(pal, n=64, main=""){
  
  if(!(n %in% c(4,16,64,256))) stop("Value of n can only be one of 4,16,64,256.")
  if(is.function(pal)) pal <- pal(n)
  nr <- sqrt(n)
  
  # Probably a fancier way with recursion...but this is simpler
  if(n > 0){
    zval <- matrix(c(0,1,2,3), byrow=TRUE, ncol=2)
  }
  if(n > 4){
    zval <- cbind(zval, 4+zval)
    zval <- rbind(zval, 8+zval)
  }
  if(n > 16){
    zval <- cbind(zval, 16+zval)
    zval <- rbind(zval, 32+zval)
  }
  if(n > 64){
    zval <- cbind(zval, 64+zval)
    zval <- rbind(zval, 128+zval)
  }
  zval <- zval+1

  # Use t() and nr:1 to match Karpov's arrangement.
  image(t(zval[nr:1,]), col=pal, axes=FALSE)

  if(main!="") mtext(main)
  
  invisible()
}

