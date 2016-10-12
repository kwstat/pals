# pals.R
# Time-stamp: <12 Oct 2016 16:41:59 c:/x/rpack/pals/R/pals.R>
# Copyright: Kevin Wright, 2016. License: GPL-2.


#' Comprehensive color palettes and evaluation tools
#' 
#' Design goals:
#' 
#' 1. All palettes are _functions_ that return a vector of 'n' colors.
#' 2. The palette function names use only lowercase letters.
#' 3. The 'data' directory is not used.
#' 4. The source package is text (no rda files).
#' 5. No dependencies/imports of palettes.
#' 6. Extensive collection of palettes.
#' 7. Multiple tools to evaluate palettes.
#' 
#' @name pals-package
#' @aliases pals-package
#' @docType package
#' @author Kevin Wright
NULL

# ----------------------------------------------------------------------------

#' Show palettes as colored bands
#'
#' Show palettes as colored bands (like paint chips).
#'
#' @param ... Palettes, either functions or vectors.
#' @param n The number of colors to display for palette functions, default 51.
#' @param labels Labels for palettes
#' 
#' @examples
#' pal.chips(cubehelix, gnuplot, jet, tol.rainbow, tol, inferno, magma, plasma, viridis, parula, n=51)
#' pal.chips(c('red','white','blue'), rainbow)
#'
#' @export
pal.chips <- function(..., n=51, labels=NULL){

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
    labels <- labels[-1] # first item is 'pal.chips'
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

}
if(FALSE){
pal.chips(c('red','white','blue'),c('blue','yellow'), c('black','red','gold'), labels=c('USA','Sweden','Germany'))
pal.chips(cm.colors, rainbow, topo.colors, heat.colors, c('red','blue'), n=31)
pal.chips(alphabet)
pal.chips(alphabet,n=25)
pal.chips(alphabet,n=26)
pal.chips(alphabet,n=27)
pal.chips(cubehelix)
pal.chips(alphabet,cubehelix)
pal.chips(cubehelix,alphabet)
pal.chips(warmcool, c('red','orange','yellow','green','blue','purple'), coolwarm, n=11)
pal.chips(alphabet,cols25,glasbey,kelly,stepped,tol)
}

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
#' pal.chips(alphabet,n=26)
#' pal.map(alphabet)
#' pal.heatmap(alphabet)
#'
#' # ----- cols25 -----
#' pal.chips(cols25,n=25)
#' pal.heatmap(cols25)
#'
#' # ------ glasbey ------
#' pal.chips(glasbey,n=32)
#' pal.cube(glasbey, n=32) # Blues are close together
#' pal.heatmap(glasbey(32))
#'
#' # ----- kelly -----
#' pal.chips(kelly,n=22)
#' pal.heatmap(kelly(22))
#'
#' # ----- stepped -----
#' pal.chips(stepped,n=20)
#' pal.heatmap(stepped, n=20)
#' 
#' # ----- tol -----
#' pal.chips(tol,n=12)
#' pal.heatmap(tol, 12)
#' 
#' @references
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
#' Color Schemes Appropriate for Scientific Data Graphics
#' http://geog.uoregon.edu/datagraphics/color_scales.htm
#' 
#' @name discrete
NULL


#' @export
#' @rdname discrete
alphabet <- function(n=26) {
  
  if(n > 26){
    warning("Only 26 colors are available with 'alphabet'")
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
cols25 <- function(n=25) {
  if(n > 25) {
    warning("Only 25 colors are available with 'cols25'.")
    n <- 25
  }
  pal <- c(rgb(31,120,200,max=255), # "#1f78b4",
           "#ff0000", # red
           "#33a02c",
           rgb(106,51,194,max=255), # "#6a3d9a", # mauve
           "#ff7f00",
           "#000000", # "black",
           rgb(255,215,0,max=255), # "gold1",
           "#a6cee3",
           rgb(251,100,150,max=255), # "#FB9A99", # lt pink
           "#b2df8a",
           "#CAB2D6", # lt purple
           "#FDBF6F", # lt orange
           rgb(153, 153, 153, max=255), # "gray60",
           rgb(238, 230, 133, max=255), # "khaki2",
           rgb(200,48,140,max=255), #"maroon",
           rgb(255,131, 250, max=255), # "orchid1",
           rgb(200,20,250,max=255), # "#ff0ac8", # deeppink1
           rgb(0, 0, 255, max=255), # "blue1",
           rgb(54, 100, 139, max=255), # "steelblue4",
           rgb(0,226,229,max=255), # "darkturquoise",
           rgb(0, 255, 0, max=255), # "green1",
           rgb(119,139,0,max=255), # "yellow4",
           rgb(190,190,0,max=255), #"yellow3",
           rgb(139,59,0,max=255), # "darkorange4",
           rgb(165,42,60,max=255) # "brown"
           )
  
  return(pal[1:n])
}


#' @export
#' @rdname discrete
glasbey <- function(n=32) {
  if(n > 32) {
    warning("Only 32 colors are available with 'glasbey'.")
    n <- 32
  }

  pal <- subset(colrs, palette=="glasbey")[,c('red','green','blue')]
  rgb(pal, max=255)[1:n]
}

#' @export
#' @rdname discrete
kelly <- function(n=22) {

  if(n > 22) {
    warning("Only 22 colors are available with 'kelly'")
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

#' @export
#' @rdname discrete
stepped <- function(n=20) {
  
  if(n > 20) {
    warning("Only 20 colors are available with 'stepped'")
    n <- 20
  }

  # Similar to the HSV codes here, but using 4 steps instead of 5
  # http://geog.uoregon.edu/datagraphics/color/StepSeq_25.txt

  pal <- subset(colrs, palette=="stepped")[,c('red','green','blue')]
  # Even though the columns are called red,green,blue, the numbers
  # are really HSV

  # If pal is a tibble, reference by column did not work:
  # hsv(pal[,'red']/360, pal[,'green'], pal[,'blue'])
  pal <- hsv(pal$red/360, pal$green, pal$blue)
  return(pal[1:n])
}



#' @export
#' @rdname discrete
tol <- function(n) {
  if(n > 12) warning("Only 12 colors are available with 'tol'")
  pal <- subset(colrs, palette=="tol" & ncolors==n)
  pal <- rgb(pal[,c('red','green','blue')], max=255)
  pal[1:n]
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

#' Continuous palettes
#'
#' Color palettes designed for continuous data.
#' 
#' The \code{coolwarm} and 'warmcool' palette by Moreland (2009) is colorblind safe.
#' The transition to and from gray is smooth, to reduce Mach banding.
#'
#' The \code{cubehelix} palette is sometimes used in astronomy.
#' Images using this palette will look monotonically increasing to both the
#' human eye and when printed in black and white.
#' This palette is named 'cubehelix' because the r,g,b values produced can be
#' visualised as a squashed helix around the diagonal from black (0,0,0) to
#' white (1,1,1) in the r,g,b color cube.
#'
#' The \code{gnuplot} palette uses black-blue-pink-yellow colors.
#' This palette looks good when printed in black and white.
#' Identical to the sp::bpy.colors palette.
#' 
#' The \code{jet} palette should not be used and is only provided for historical interest.
#' The code for this palette comes from the example section of \code{colorRampPalette}.
#' The 'jet' palette gained popularity as the default colormap in older versions of Matlab.
#' Because of the unevenness of the gradient, jet will exaggerate some features
#' of the data and minimize other features.
#'
#' The \code{parula} palette here is similar to the default Matlab palette.
#' Specific colors were adapted from Gnuplot.
#'
#' The \code{tol.rainbow} palette by Tol (2012) is a dark rainbow palette from
#' purple to red which works much better than standard rainbow palettes
#' for colorblind people. 
#' If 1 <= n <= 13, manually-chosen equidistant rainbow colors are
#' used, where distances are defined by the CIEDE2000 color difference.
#' If 14 <= n <= 21, manually-chosen triplets of colours are used.
#' If n > 21 or if manual=FALSE, the palette computes the colors
#' according to Equation 3 of Tol (2012).
#' 
#' @param n Number of colors to return.
#' @return A vector of colors.
#' 
#' @examples
#'
#' # ----- coolwarm -----
#' filled.contour(volcano, color.palette = coolwarm, nlevels=40, asp = 1, axes=0, main="coolwarm")
#' pal.test(coolwarm)
#' # Note the mach banding gray line in the following:
#' filled.contour(volcano,
#'   color.palette = colorRampPalette(c("#3B4CC0", "lightgray", "#B40426"),space="Lab"),
#'   nlevels=40, asp = 1, axes=0)
#'
#' # ----- cubehelix -----
#' # Full range of colors. Pink is overwhelming. Not the best choice.
#' filled.contour(volcano, color.palette = cubehelix, nlevels=40, asp = 1, axes=0, main="cubehelix")
#' pal.test(cubehelix)
#' 
#' # Limited to mostly blues/greens
#' filled.contour(volcano, color.palette = function(n)
#'   cubehelix(n, start=.5, r=-.75), nlevels=40, asp = 1, axes=0, main="cubehelix")
#'
#' # Similar, but more saturated. See: http://inversed.ru/Blog_2.htm
#' filled.contour(volcano, color.palette = function(n)
#'   cubehelix(n, start=.25, r=-.67, hue=1.5), nlevels=40, asp = 1, axes=0, main="cubehelix")
#'
#' # Dark colors totally lose structure of the volcano peak.
#' op <- par(mfrow=c(2,2), mar=c(2,2,2,2))
#' image(volcano, col = cubehelix(51), asp = 1, axes=0, main="cubehelix")
#' image(volcano, col = cubehelix(51, start=.25, r=-.67, hue=1.5), asp = 1, axes=0, main="cubehelix")
#' image(volcano, col = rev(cubehelix(51)), asp = 1, axes=0, main="cubehelix")
#' image(volcano, col = rev(cubehelix(51, start=.25, r=-.67, hue=1.5)), asp = 1, axes=0, main="cubehelix")
#' par(op)
#'
#' # ----- gnuplot -----
#' filled.contour(volcano, color.palette = gnuplot, nlevels=40, asp = 1, axes=0, main="gnuplot")
#' pal.test(gnuplot)
#'
#' # ----- jet -----
#' filled.contour(volcano, color.palette = jet, nlevels=40, asp = 1, axes=0,  main="jet")
#' pal.test(jet)
#'
#' # ----- parula -----
#' filled.contour(volcano, color.palette = parula, nlevels=40, asp = 1, axes=0, main="parula")
#' pal.test(parula)
#'
#' # ----- tol.rainbow -----
#' filled.contour(volcano, color.palette = tol.rainbow, nlevels=40, asp = 1, axes=0, main="tol.rainbow")
#' pal.test(tol.rainbow)
#' @author Palette colors by various authors. R code by Kevin Wright.
#' 
#' @references
#' 
#' Dave A. Green. (2011).
#' A colour scheme for the display of astronomical intensity images.
#' Bull. Astr. Soc. India, 39, 289–295.
#' http://arxiv.org/abs/1108.5083
#' http://www.mrao.cam.ac.uk/~dag/CUBEHELIX/
#'
#' Kenneth Moreland. (2009).
#' Diverging Color Maps for Scientific Visualization.
#' Proceedings of the 5th International Symposium on Visual Computing.
#' http://www.kennethmoreland.com/color-maps/
#' http://dx.doi.org/10.1007/978-3-642-10520-3_9
#'
#' Paul Tol (2012). Color Schemes. SRON technical note, SRON/EPS/TN/09-002.
#' https://personal.sron.nl/~pault/
#' 
#' My Favorite Colormap.
#' https://web.archive.org/web/20040119000943/http://www.ihe.uni-karlsruhe.de/mitarbeiter/vonhagen/palette.en.html
#'
#' MathWorks documentation.
#' http://www.mathworks.com/help/matlab/ref/colormap.html
#'
#' Gnuplot palettes.
#' https://github.com/Gnuplotting/gnuplot-palettes
#' @name continuous
NULL


#' @param start Start angle (radians) of the helix
#' @param r  Number of rotations of the helix
#' @param hue Saturation of the colors, 0 = grayscale, 1 = fully saturated
#' @param gamma gamma < 1 emphasizes low intensity values, gamma > 1 emphasizes high intensity values
#' @export
#' @rdname continuous
cubehelix <- function(n=25, start = 0.5, r = -1.5, hue = 1, gamma = 1) {
  
  lambda = seq(0, 1, length=n) # value along the black-white diagonal
  
  lgam = rep(lambda^gamma, each = 3)
  phi = 2 * pi * (start/3 + r * lambda) # angle for deviation
  amp = hue * lgam * (1 - lgam)/2 # amplitude
  M = matrix(c(-0.14861, -0.29227, 1.97294, 1.78277, -0.90649, 0),
             byrow=FALSE, ncol = 2)
  out = lgam + amp * (M %*% rbind(cos(phi), sin(phi))) # Eqn 2
  
  # truncate values into [0,1]
  out = pmin(pmax(out, 0), 1)

  # Convert to rgb value
  rgb(t(out))

}

#' @param trim Proportion of tail colors to trim, default 0.1
#' @export 
#' @rdname continuous
gnuplot <- function (n = 25, trim = 0.1) {
  if (trim >= 1 || trim < 0) 
    stop("trim should be in [0, 1]")
  i = seq(0.5 * trim, 1 - 0.5 * trim, length = n)

  R = ifelse(i < 0.25, 0, ifelse(i < 0.57, i/0.32 - 0.78125, 1))
  G = ifelse(i < 0.42, 0, ifelse(i < 0.92, 2 * i - 0.84, 1))
  B = ifelse(i < 0.25, 4 * i, ifelse(i < 0.42, 1, ifelse(i < 0.92, -2 * i + 1.84, i/0.08 - 11.5)))
  rgb(R, G, B)
}


#' @export
#' @rdname continuous
jet <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                          "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

#' @export
#' @rdname continuous
parula <- colorRampPalette(c('#352a87', '#0363e1', '#1485d4', '#06a7c6', '#38b99e',
            '#92bf73', '#d9ba56', '#fcce2e', '#f9fb0e'))

# ----------------------------------------------------------------------------

#' @export
#' @rdname continuous
coolwarm <- function(n){
  # Values from Moreland Table 2
  #colorRampPalette(rgb(pal, max=255))(n)
  colorRampPalette(rgb(subset(colrs, palette=="coolwarm")[,c('red','green','blue')],max=255))(n)
}


#' @export
#' @rdname continuous
warmcool <- function(n){
  rev(coolwarm(n))
}

# ----------------------------------------------------------------------------

#' @param manual If TRUE, return manually-calibrated colors.
#' @export
#' @rdname continuous
tol.rainbow <- function(n=25, manual=TRUE){
  # Original non-R code by P. Tol from
  # https://personal.sron.nl/~pault/rainbow_colors.pro
  # Ported to R by Kevin Wright
  
  if(manual & n >= 14 & n <= 21) {
    banded <- matrix(c(17, 68, 119, 17, 68, 119, 17, 68, 136, 119, 170, 221, 119, 170, 221, 119, 170, 221, 119, 170, 204,
                       68, 119, 170, 119, 170, 204, 119, 170, 204, 119, 170, 221, 68, 119, 170, 17, 68, 119, 17, 68, 153,
                       119, 170, 221, 119, 170, 204, 68, 119, 170, 17, 68, 119, 17, 68, 119, 34, 85, 136, 85, 136, 187), byrow=FALSE, ncol=3)
    banded <- rgb(banded, max=255)
    ix <- list(
      c(0, 1, 2, 3, 4, 5, 9, 10, 11, 12, 13, 14, 15, 16),
      c(0, 1, 2, 3, 4, 5, 9, 10, 11, 12, 13, 14, 15, 16, 17),
      c(20, 0, 1, 2, 3, 4, 5, 9, 10, 11, 12, 13, 14, 15, 16, 17),
      c(19, 20, 0, 1, 2, 3, 4, 5, 9, 10, 11, 12, 13, 14, 15, 16, 17),
      c(18, 19, 20, 0, 1, 2, 3, 4, 5, 9, 10, 11, 12, 13, 14, 15, 16, 17),
      c(20, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17),
      c(19, 20, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17),
      c(18, 19, 20, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))[[n-13]]
    return(banded[ix+1]) # add 1 because original code started indexing at 0
  }

  if (manual & n <=13) {
    x <- list(
      c(0.137),
      c(0.137, 1),
      c(0.137, 0.511, 1),
      c(0.137, 0.36, 0.762, 1),
      c(0.137, 0.335, 0.483, 0.794, 1),
      c(0.137, 0.287, 0.402, 0.651, 0.833, 1),
      c(0., 0.195, 0.34, 0.436, 0.685, 0.843, 1),
      c(0., 0.177, 0.301, 0.39, 0.536, 0.735, 0.861, 1),
      c(0., 0.162, 0.266, 0.36, 0.437, 0.617, 0.768, 0.874, 1),
      c(0., 0.149, 0.239, 0.337, 0.399, 0.508, 0.676, 0.794, 0.885, 1),
      c(0., 0.137, 0.218, 0.312, 0.374, 0.44, 0.577, 0.715, 0.813, 0.894, 1),
      c(0., 0.128, 0.203, 0.284, 0.351, 0.402, 0.487, 0.628, 0.742, 0.826, 0.9, 1),
      c(0., 0.118, 0.188, 0.259, 0.332, 0.38, 0.437, 0.544, 0.67, 0.765, 0.839, 0.907, 1))[[n]]
  } else {
    x <- seq(from=0, to=1, length=n)
  }
  # Eqn 3 of Tol's tech report  
  R <- (0.472-0.567*x+4.05*x^2)/(1+8.72*x-19.17*x^2+14.1*x^3)
  G <- 0.108932 - 1.22635*x + 27.284*x^2 - 98.577*x^3 + 163.3*x^4 - 131.395*x^5 + 40.634*x^6
  B <- 1/(1.97 + 3.54*x - 68.5*x^2 + 243*x^3-297*x^4 + 125*x^5)
  return(rgb(R,G,B))
  
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

#' Continuous palettes by Matteo Niccoli
#'
#' These palettes are intended by be more perceptually balanced than traditional
#' rainbow-like palettes.
#' 
#' \code{isol()}: Lab-based isoluminant rainbow with constant luminance L*=60.
#' Best choice for displaying interval data with external lighting.
#' best for displaying interval data with external lighting.
#' This is so as to allow the lighting to provide the shading to highlight
#' the details of interest. If lighting is combined with a colormap that
#' has its own luminance function associated - even as simple as a
#' linear increase this will confuse the viewer.
#' 
#' \code{linearl()}: Lab-based linear lightness rainbow.
#' A linear lightness modification of Matlab's 'hot' palette.
#' For interval data displayed without external lighting.
#' 100% perceptual
#' 
#' \code{linlhot()}: Linear lightness modification of Matlab's hot color palette.
#' For interval data displayed without external lighting
#' 100% perceptual
#' 
#' \code{cubicyf()}: Lab-based rainbow scheme with cubic-law luminance(default)
#' For interval data displayed without external lighting
#' 100% perceptual
#' 
#' \code{cubicl()}: Lab-based rainbow scheme with cubic-law luminance
#' For interval data displayed without external lighting
#' Similar to cubicyf(), but has red at high end
#' (a modest deviation from 100% perceptual)
#' 
#' @param n Number of colors to return
#' @return A vector of colors
#' 
#' @author Palettes by Matteo Niccoli. R code by Kevin Wright.
#' 
#' @examples
#' pal.test(cubicyf) # purple blue green
#' pal.test(cubicl) # purple blue green orange
#' pal.test(isol) # magenta blue green red
#' pal.test(linearl) # black blue green tan
#' pal.test(linearlhot) # black red yellow
#' pal.chips(cubicyf,cubicl,isol,linearl,linearlhot)
#' 
#' @references
#' Matteo Niccoli (2010).
#' Perceptually improved colormaps.
#' http://www.mathworks.com/matlabcentral/fileexchange/28982-perceptually-improved-colormaps
#' Color definitions from here:
#' http://www.mathworks.com/matlabcentral/fileexchange/28982-perceptually-improved-colormaps/content/pmkmp/pmkmp.m
#' https://mycarta.wordpress.com/2012/05/29/the-rainbow-is-dead-long-live-the-rainbow-series-outline/
#' 
#' @name niccoli
NULL


#' @export
#' @rdname niccoli
cubicyf <- function(n) {
  pal <- subset(colrs, palette=="cubicyf")[,c('red','green','blue')]
  colorRampPalette(rgb(pal))(n)
}


#' @rdname niccoli
#' @export 
isol <- function(n) {
  pal <- subset(colrs, palette=="isol")[,c('red','green','blue')]
  colorRampPalette(rgb(pal))(n)
}


#' @rdname niccoli
#' @export
cubicl <- function(n) {
  pal <- subset(colrs, palette=="cubicl")[,c('red','green','blue')]
  colorRampPalette(rgb(pal))(n)
}


#' @rdname niccoli
#' @export
linearl <- function(n) {
  pal <- subset(colrs, palette=="linearl")[,c('red','green','blue')]
  colorRampPalette(rgb(pal))(n)
}

#' @rdname niccoli
#' @export 
linearlhot <- function(n) {
  pal <- subset(colrs, palette=="linearlhot")[,c('red','green','blue')]
  colorRampPalette(rgb(pal))(n)
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------

#' Show a palette of colors in three dimensional RGB or LUV space
#'
#' The supplied color palette is converted to red,green,blue values
#' and plotted in a three-dimensional scatterplot.
#' 
#' @param pal A palette function or a vector of colors
#' @param n Number of colors
#' @param label If TRUE, show color name/value on plot
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
pal.cube <- function(pal, n=100, label=FALSE, type="RGB"){

  if(is.function(pal)) pal <- pal(n)

  if(type=="RGB") {
    x <- t(col2rgb(pal))
    xl="red"; yl="green"; zl="blue"
  } else if (type=="LUV") {
    luvmat <- as(hex2RGB(pal), "LUV")
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

#' Test palette effectiveness with a heatmap
#'
#' A random heatmap is generated (with 5% missing values) and a key is added
#' to the heatmap by appending a blank column and then a column with the
#' palette colors.
#'
#' In an effective palette, the value of each cell can be correctly
#' interpreted using the key on the right side, and missing values
#' are identified.
#' 
#' @param pal A palette function/vector.
#' @param n Number of colors in the palette.
#' @return None.
#' @author Kevin Wright
#' @export 
#' @examples
#' pal.heatmap(paired(12), n=12)
#' pal.heatmap(coolwarm(12), n=12)
#' pal.heatmap(tol)
#' pal.heatmap(glasbey)
#' pal.heatmap(kelly)
#' @references
#' None
pal.heatmap <- function(pal, n=25){

  if(is.function(pal)) {
    pal <- pal(n)
  } else {
    n <- length(pal)
  }
  
  xdim <- 15
  ydim <- n
  cellvals <- sample(1:n, size=xdim*ydim, replace=TRUE)
  # 5% missing values
  cellvals[runif(xdim*ydim)<.05] <- NA
  mat <- matrix(cellvals, ncol=xdim)
  
  # Add a column of NA and a column for the palette
  mat <- cbind(mat, NA)
  mat <- cbind(mat, 1:n)
  image(t(mat), col=pal,axes=FALSE)
  axis(side=4)
}

# ----------------------------------------------------------------------------

#' Test palette effectiveness with a scatterplot
#'
#' Details.
#' @param pal 
#' @param n 
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

#' Test safety of palette for black/white, colorblind
#'
#' .desc
#'
#' .details
#'
#' @param pal 
#' @param n 
#' @return
#' None.
#' @author Kevin Wright
#' @examples 
#' pal.safe(glasbey(31))
#' pal.safe(rainbow(100))
#' pal.safe(cubicyf(21))
#' @references 
#' None
#' @export 
pal.safe <- function(pal, n=25){
  require(colorspace)
  require(dichromat)

  ncolor <- length(pal)

  # pal is a single vector of colors, now make it a list
  pal <- list(pal,
              desaturate(pal),
              dichromat(pal, type="deutan"),
              dichromat(pal, type="protan"),
              dichromat(pal, type="tritan"))
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

}

# ----------------------------------------------------------------------------

#' Create a test image of a color palette.
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
#' @param pal A palette function.
#' @return None.
#' @export 
#' @examples
#' pal.test(parula)
#' @references
#' # See links inline.
#' 
pal.test <- function(pal){

  op <- par(mfrow=c(2,3),mar=c(2,2,1,1), bg="gray80")

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

  on.exit(op)
  par(op)
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
#' Test palette with a sineramp
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
#' @param pal Palette function/vector
#' @param n Number of colors
#' @param nx Number of 'pixels' horizontally (approximate).
#' @param ny Number of 'pixels' vertically
#' @param amp Amplitude of sine wave, default 12.5
#' @param wavelen Wavelength of sine wave, in pixels, default 8.
#' @param pow Power for dampening the sine wave. Default 2. For no dampening, use 0.
#' For linear dampening, use 1.
#' @return None
#' @author Concept by Peter Kovesi. R code by Kevin Wright.
#' @examples 
#' pal.sineramp(jet) # Very poor in green areas
#' pal.sineramp(parula)
#' pal.sineramp(brewer_greys(100))
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
  image(volcano, col=pal, axes=FALSE, asp=1)
  
}

# ----------------------------------------------------------------------------

#' @title .title
#'
#' .desc
#'
#' The distance between two palettes (of equal length) is calculated pointwise using
#' the Lab color space.  A 'just noticeable difference' between colors is roughly 2.3.
#'
#' @param pal1 A color palette (function or vector)
#' @param pal2 A color palette (function or vector)
#' @param n Number of colors to use
#' @return 
#' @author Kevin Wright
#' @examples 
#' \dontrun{}
#' @references
#' https://en.wikipedia.org/wiki/Color_difference
#' @export 
pal.dist <- function(pal1, pal2, n){
  
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

## interleave <- function(v1,v2) {
##   ord1 <- 2*(1:length(v1))-1
##   ord2 <- 2*(1:length(v2))
##   c(v1,v2)[order(c(ord1,ord2))]
## }
# https://stat.ethz.ch/pipermail/r-help/2006-March/101023.html
interleave(rep(1,5),rep(3,8))
pa0 <- c("#ff0000","#00ff00","#0000ff")
pa1 <- c("#fa0000","#00fa00","#0000fa") # 2.4
pa2 <- c("#f40000","#00f400","#0000f4") # 5.2
pa12 <- interleave(pa1,pa2)
pie(rep(1, 2*length(pa2)), col=pa12)
pal.chips(pa1,pa0,pa2)
max(pal.dist(pa0,pa1)) # 2.4
max(pal.dist(pa0,pa2)) # 5.2

# palettes that are defined by functions are sometimes difficult to compress
# into a colorRampPalette with a small number of colors
pal.chips(terrain.colors(11), terrain.colors(21), terrain.colors(31), terrain.colors(41), terrain.colors(51))
pal.chips(heat.colors(11), heat.colors(21), heat.colors(31), heat.colors(41), heat.colors(51))
pal.chips(topo.colors(11), topo.colors(21), topo.colors(31), topo.colors(41), topo.colors(51))
pal.chips(rainbow(11), rainbow(21), rainbow(31), rainbow(41), rainbow(51))
pal.chips(cm.colors(11), cm.colors(21), cm.colors(31), cm.colors(41), cm.colors(51))

# topo.colors has a sharp boundary, not captured very well by colorRampPalette
# heat.colors needs 80+ colors to reproduce. Why?
palfun <- coolwarm
nsamp <- 15
p1 <- palfun(255)
p2 <- colorRampPalette( palfun(nsamp) )(255)
pal.chips(p1,p2,palfun(nsamp))
max(pal.dist(p1,p2))

# ----------------------------------------------------------------------------

#' @title Compress a color palette function to fewer colors
#'
#' Compress a color palette function to fewer colors
#'
#' Some color palette functions are defined with more colors than 
#' needed.  This function compresses a color palette function down to a sample
#' of colors that can be passed into 'colorRampPalette' and re-create the
#' original palette with a just-noticeable-difference.
#'
#' Color palettes that are defined as a smoothly varying ramp between a set of
#' colors often compress quite well.
#' Color palettes that are defined by functions may not compress well.
#' See the examples.
#'
#' @param pal A palette function
#' @param n Initial number of colors to use
#' @return A vector of colors
#' @author Kevin Wright
#' @examples
#' # The 'cm.colors' palette compresses to only 3 colors
#' cm2 <- pal.compress(cm.colors, n=3)
#' pal.chips(cm.colors(255), colorRampPalette(cm2)(255), cm2)
#' title("cm.colors")
#' # The 'heat.colors' palette needs 84 colors
#' heat2 <- pal.compress(heat.colors, n=3)
#' pal.chips(heat.colors(255), colorRampPalette(heat2)(255), heat2)
#' title("heat.colors")
#' # The 'topo.colors' palette needs 249 colors because of the discontinuity
#' # topo2 <- pal.compress(topo.colors, n=3)
#' # pal.chips(list(topo.colors(255), colorRampPalette(topo2)(255), topo2))
#' # title("topo.colors")
#' 
#' @references 
#' None
#' @export 
pal.compress <- function(pal, n=5) {
  # pal is a function
  pal255 <- pal(255)
  
  done <- FALSE
  while(!done) {
    palc <- colorRampPalette(pal(n))(255) # compressed palette ramp
    p1 <- convertColor(t(col2rgb(pal255)), from="sRGB",to="Lab",scale.in=255)
    p2 <- convertColor(t(col2rgb(palc)), from="sRGB",to="Lab",scale.in=255)
    delta <- max(apply((p1-p2), 1, function(x) sqrt(sum(x^2))))
    if(delta >  2.5) n=n+1 else done=TRUE
  }
  return(pal(n))
}

inferno <- function(n) {
  colorRampPalette(rgb(subset(matplot, palette=="inferno")[,c('red','green','blue')]))(n)
}

magma <- function(n) {
  colorRampPalette(rgb(subset(matplot, palette=="magma")[,c('red','green','blue')]))(n)
}

plasma <- function(n) {
  colorRampPalette(rgb(subset(matplot, palette=="plasma")[,c('red','green','blue')]))(n)
}

viridis <- function(n) {
  colorRampPalette(rgb(subset(matplot, palette=="viridis")[,c('red','green','blue')]))(n)
}
pal.chips(list(inferno,magma, plasma,viridis))
pal.chips(list(viridis(21)))

