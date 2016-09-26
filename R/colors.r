# colors.r
# Time-stamp: <26 Sep 2016 08:38:03 c:/x/rpack/pals/R/colors.r>
# Copyright: Kevin Wright, 2016. License: GPL-2.

#' Comprehensive color palettes and evaluation tools
#' 
#' Design goals:
#' 
#' 1. All palettes are functions that return a vector of 'n' colors.
#' 
#' 2. The function names use only lowercase letters.
#' 
#' 3. The 'data' directory is not used.
#' 
#' @name pals-package
#' @aliases pals-package
#' @docType package
#' @author Kevin Wright
NULL

# ----------------------------------------------------------------------------

# Get kw.fillcols

# Interesting palettes at bottom
# https://github.com/d3/d3-scale/blob/master/README.md#interpolateViridis

# Good test image?
# http://yt-project.org/doc/visualizing/colormaps/index.html

# Need an option to reverse any palette

# http://flowingdata.com/2009/11/12/how-to-make-a-us-county-thematic-map-using-free-tools/

# See scales, ggthemes, viridis::scale_fill_viridis

# In pal.test, add discrete=TRUE to prevent interpolation

# Note, we could define functions like:
# cubicyf <- colorRampPalette(rgb(colormatrix))
# But then the actual colors get hidden deep inside an environment


# TODO: gist_earth, cmocean, interface to colorbrewer


# Best comparison pages
# http://inversed.ru/Blog_2.htm
# http://peterkovesi.com/projects/colourmaps/

# http://graphicdesign.stackexchange.com/questions/3682/where-can-i-find-a-large-palette-set-of-contrasting-colors-for-coloring-many-d

##  Is there a better color scale than the “rainbow” colormap?
## http://stackoverflow.com/q/7251872/590388
# http://stackoverflow.com/questions/13968520/color-selection-for-matplotlib-that-prints-well

# http://geog.uoregon.edu/datagraphics/color_scales.htm

# http://stackoverflow.com/questions/13968520/color-selection-for-matplotlib-that-prints-well
  
## http://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r

# Matteo Niccoli.  Free to re-use mode/media.
# Credit to Matteo Niccoli as the author and mycarta.wordpress.com
## https://mycarta.wordpress.com/2012/05/29/the-rainbow-is-dead-long-live-the-rainbow-series-outline/
## http://mathematica.stackexchange.com/questions/64513/is-there-an-easy-way-to-use-matteo-niccolis-perceptual-color-maps-for-2d-plots
# Following has short cubicyf palette
# Trimmed, original code here
# https://www.mathworks.com/matlabcentral/fileexchange/28982-perceptually-improved-colormaps/content/pmkmp/pmkmp.m
# Non-trimmed version here
# https://github.com/rikrd/matlab/blob/master/output/pmkmp/pmkmp.m


# https://github.com/matplotlib/matplotlib/blob/master/lib/matplotlib/_cm.py

# cmocean, MIT license
# http://matplotlib.org/cmocean/
# https://github.com/kthyng/cmocean
# Actual color definitions are here:
# https://github.com/kthyng/cmocean/tree/master/cmocean/rgb
# R version of cmocean:
# https://github.com/dankelley/oce/

# ----------------------------------------------------------------------------

# Done

# https://personal.sron.nl/~pault/

# https://github.com/btupper/catecolors

# Polychrome
# install.packages("Polychrome", repos="http://R-Forge.R-project.org")
# https://rdrr.io/rforge/Polychrome/man/viewers.html

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
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
#' between colors in a set if the colors are chosen in order from the top.
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
#' pal.bands("alphabet",n=26)
#' pal.map(alphabet)
#' pal.heatmap(alphabet)
#'
#' # ----- cols25 -----
#' pal.bands("cols25",n=25)
#' pal.heatmap(cols25)
#'
#' # ------ glasbey ------
#' pal.bands("glasbey",n=32)
#' pal.cube(glasbey, n=32) # Blues are close together
#' pal.heatmap(glasbey(32))
#'
#' # ----- kelly -----
#' pal.bands("kelly",n=22)
#' pal.heatmap(kelly(22))
#'
#' # ----- tol -----
#' pal.bands("tol",12)
#'
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
#' @export
#' @rdname discrete
alphabet <- function(n=26) {
  
  if(n > 26){
    warning("alphabet() has max 26 colors")
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
  
  pal <- matrix(
    c(2,1,0,0,255,
      3,1,255,0,0,
      4,1,0,255,0,
      5,2,0,0,51,
      6,3,255,0,182,
      7,5,0,83,0,
      8,3,255,211,0,
      9,5,0,159,255,
      10,3,154,77,66,
      11,1,0,255,190,
      12,9,120,63,193,
      13,11,31,150,152,
      14,12,255,172,253,
      15,8,177,204,113,
      16,3,241,8,92,
      17,10,254,143,66,
      18,12,221,0,255,
      19,5,32,26,1,
      20,10,114,0,85,
      21,9,118,108,149,
      22,4,2,173,36,
      23,8,200,255,0,
      24,15,136,108,0,
      25,1,255,183,159,
      26,15,133,133,103,
      27,3,161,3,0,
      28,11,20,249,255,
      29,21,0,71,158,
      30,14,220,94,147,
      31,1,147,212,255,
      32,2,0,76,255,
      1,1,242,243,244 # white #F2F3F4
      ), ncol=5, byrow=TRUE)
  
  rgb(pal[,3], pal[,4], pal[,5], max=255)
}

#' @export
#' @rdname discrete
kelly <- function(n=22) {

  if(n > 22) {
    warning("Only 22 colors are available with kelly()")
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
tol <- function(n=12) {

  if(n > 12) {
    warning("Only 12 colors are available with tol()")
    n <- 12
  }
  
  list(
    c("#4477AA"),
    c("#4477AA", "#CC6677"),
    c("#4477AA", "#DDCC77", "#CC6677"),
    c("#4477AA", "#117733", "#DDCC77", "#CC6677"),
    c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677"),
    c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499"),
    c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499"),
    c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499"),
    c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499"),
    c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
    c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
    c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499")
  )[[n]]

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
#' The 'jet' palette was the default colormap in older versions of Matlab.
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
#'  filled.contour(volcano, color.palette = coolwarm, nlevels=40, asp = 1, main="coolwarm")
#' pal.test(coolwarm)
#' # Note the mach banding gray line in the following:
#' filled.contour(volcano,
#'   color.palette = colorRampPalette(c("#3B4CC0", "lightgray", "#B40426"),space="Lab"),
#'   nlevels=40, asp = 1, main="coolwarm")
#'
#' # ----- cubehelix -----
#' # Full range of colors. Pink is overwhelming. Not the best choice.
#' filled.contour(volcano, color.palette = cubehelix, nlevels=40, asp = 1, main="cubehelix")
#' pal.test(cubehelix)
#' 
#' # Limited to mostly blues/greens
#' filled.contour(volcano, color.palette = function(n)
#'   cubehelix(n, start=.5, r=-.75), nlevels=40, asp = 1, main="cubehelix")
#'
#' # Similar, but more saturated. See: http://inversed.ru/Blog_2.htm
#' filled.contour(volcano, color.palette = function(n)
#'   cubehelix(n, start=.25, r=-.67, hue=1.5), nlevels=40, asp = 1, main="cubehelix")
#'
#' # Dark colors totally lose structure of the volcano peak.
#' op <- par(mfrow=c(2,2), mar=c(2,2,2,2))
#' image(volcano, col = cubehelix(51), asp = 1, main="cubehelix")
#' image(volcano, col = cubehelix(51, start=.25, r=-.67, hue=1.5), asp = 1, main="cubehelix")
#' image(volcano, col = rev(cubehelix(51)), asp = 1, main="cubehelix")
#' image(volcano, col = rev(cubehelix(51, start=.25, r=-.67, hue=1.5)), asp = 1, main="cubehelix")
#' par(op)
#'
#' # ----- Gnuplot -----
#' filled.contour(volcano, color.palette = gnuplot, nlevels=40, asp = 1, main="gnuplot")
#' pal.test(gnuplot)
#'
#' # ----- jet -----
#' filled.contour(volcano, color.palette = jet, nlevels=40, asp = 1, main="jet")
#' pal.test(jet)
#'
#' # ----- parula -----
#' filled.contour(volcano, color.palette = parula, nlevels=40, asp = 1, main="parula")
#' pal.test(parula)
#'
#' # ----- tol.rainbow -----
#' filled.contour(volcano, color.palette = tol.rainbow, nlevels=40, asp = 1, main="tol.rainbow")
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
  pal <- matrix(c(
    59,76,192,
    68,90,204,
    77,104,215,
    87,117,225,
    98,130,234,
    108,142,241,
    119,154,247,
    130,165,251,
    141,176,254,
    152,185,255,
    163,194,255,
    174,201,253,
    184,208,249,
    194,213,244,
    204,217,238,
    213,219,230,
    221,221,221,
    229,216,209,
    236,211,197,
    241,204,185,
    245,196,173,
    247,187,160,
    247,177,148,
    247,166,135,
    244,154,123,
    241,141,111,
    236,127,99,
    229,112,88,
    222,96,77,
    213,80,66,
    203,62,56,
    192,40,47,
    180,4,38), ncol=3, byrow=TRUE)
  colorRampPalette(rgb(pal, max=255))(n)
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
#' 
#' @references
#' Matteo Niccoli (2010).
#' Perceptually improved colormaps.
#' http://www.mathworks.com/matlabcentral/fileexchange/28982-perceptually-improved-colormaps
#' https://mycarta.wordpress.com/2012/05/29/the-rainbow-is-dead-long-live-the-rainbow-series-outline/
#' @export
#' @rdname niccoli
cubicyf <- function(n) {

  pal <- matrix(c(0.5151, 0.0482, 0.6697,
                  0.5199, 0.1762, 0.8083,
                  0.4884, 0.2912, 0.9234,
                  0.4297, 0.3855, 0.9921,
                  0.3893, 0.4792, 0.9775,
                  0.3337, 0.5650, 0.9056,
                  0.2795, 0.6419, 0.8287,
                  0.2210, 0.7123, 0.7258,
                  0.2468, 0.7612, 0.6248,
                  0.2833, 0.8125, 0.5069,
                  0.3198, 0.8492, 0.3956,
                  0.3602, 0.8896, 0.2919,
                  0.4568, 0.9136, 0.3018,
                  0.6033, 0.9255, 0.3295,
                  0.7066, 0.9255, 0.3414,
                  0.8000, 0.9255, 0.3529), byrow=TRUE, ncol=3)
  
  colorRampPalette(rgb(pal))(n)
}


#' @rdname niccoli
#' @export 
isol <- function(n) {
  
  pal <- matrix(c(0.9102, 0.2236, 0.8997,
                  0.4027, 0.3711, 1.0000,
                  0.0422, 0.5904, 0.5899,
                  0.0386, 0.6206, 0.0201,
                  0.5441, 0.5428, 0.0110,
                  1.0000, 0.2288, 0.1631), byrow=TRUE, ncol=3)
  
  colorRampPalette(rgb(pal))(n)
  
}


#' @rdname niccoli
#' @export
#' 
cubicl <- function(n) {
  pal <- matrix(c(
    0.4706,      0, 0.5216,
    0.5137, 0.0527, 0.7096,
    0.4942, 0.2507, 0.8781,
    0.4296, 0.3858, 0.9922,
    0.3691, 0.5172, 0.9495,
    0.2963, 0.6191, 0.8515,
    0.2199, 0.7134, 0.7225,
    0.2643, 0.7836, 0.5756,
    0.3094, 0.8388, 0.4248,
    0.3623, 0.8917, 0.2858,
    0.5200, 0.9210, 0.3137,
    0.6800, 0.9255, 0.3386,
    0.8000, 0.9255, 0.3529,
    0.8706, 0.8549, 0.3608,
    0.9514, 0.7466, 0.3686,
    0.9765, 0.5887, 0.3569), byrow=TRUE, ncol=3)

  colorRampPalette(rgb(pal))(n)
}


#' @rdname niccoli
#' @export
#' 
linearl <- function(n) {
  pal <- matrix(c(
    0.0143,	0.0143,	0.0143,
    0.1413,	0.0555,	0.1256,
    0.1761,	0.0911,	0.2782,
    0.1710,	0.1314,	0.4540,
    0.1074,	0.2234,	0.4984,
    0.0686,	0.3044,	0.5068,
    0.0008,	0.3927,	0.4267,
    0.0000,	0.4763,	0.3464,
    0.0000,	0.5565,	0.2469,
    0.0000,	0.6381,	0.1638,
    0.2167,	0.6966,	0.0000,
    0.3898,	0.7563,	0.0000,
    0.6912,	0.7795,	0.0000,
    0.8548,	0.8041,	0.4555,
    0.9712,	0.8429,	0.7287,
    0.9692,	0.9273,	0.8961), byrow=TRUE, ncol=3)
  
  colorRampPalette(rgb(pal))(n)

}

#' @rdname niccoli
#' @export 
linearlhot <- function(n) {
  pal <- matrix(c(
    0.0225,	0.0121,	0.0121,
    0.1927,	0.0225,	0.0311,
    0.3243,	0.0106,	0.0000,
    0.4463,	0.0000,	0.0091,
    0.5706,	0.0000,	0.0737,
    0.6969,	0.0000,	0.1337,
    0.8213,	0.0000,	0.1792,
    0.8636,	0.0000,	0.0565,
    0.8821,	0.2555,	0.0000,
    0.8720,	0.4182,	0.0000,
    0.8424,	0.5552,	0.0000,
    0.8031,	0.6776,	0.0000,
    0.7659,	0.7870,	0.0000,
    0.8170,	0.8296,	0.0000,
    0.8853,	0.8896,	0.4113,
    0.9481,	0.9486,	0.7165), byrow=TRUE, ncol=3)
  
  colorRampPalette(rgb(pal))(n)
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------


#' View bands of color palettes
#'
#' Creates a plot with various color palettes.
#'
#' @param pals A vector of strings that are the names of
#' functions that return a color palette.
#' @param n The number of levels to display of the palette.
#' @param builtin Should the default R palettes be shown?
#'
#' @examples
#' #pal.bands(coolwarm)
#' pals::pal.bands(c('cubehelix', 'gnuplot', 'jet', 'tol.rainbow','tol','inferno','magma','plasma','viridis', 'parula'), n=31, builtin=TRUE)
#'
#' @export
#' 
pal.bands <- function(pals, n=9, builtin=FALSE){

  # Some code from RColorBrewer::display.brewer.pal

  if(builtin)
    pals <- c(pals,
              "rainbow", "cm.colors", "heat.colors", "topo.colors",
              "terrain.colors")

  # Check to see if palettes exist
  pal.exist <- unlist(lapply(pals, exists))
  if(any(!pal.exist)){
    cat("Can't find palettes:", pals[!pal.exist], "\n")
    pals <- pals[pal.exist]
  }

  npal <- length(pals)
  #nr <- length(pals)
  nc <- max(n)
  ylim <- c(0, npal)
  oldpar <- par(mgp = c(2, 0.25, 0))
  on.exit(par(oldpar))
  plot(1, 1, xlim = c(0, nc), ylim = ylim,
       type = "n", axes = FALSE, 
       bty = "n", xlab = "", ylab = "")
  #browser()
  for (i in 1:npal) {
    ##nj <- n[i]
    nj <- n 
    ## if (colorlist[i] == "") 
    ##   next
    shadi <- eval(parse(text=paste(pals[(npal+1) - i],"(n)",sep="")))
    rect(xleft = 0:(nj - 1), ybottom = i - 1, xright = 1:nj, 
         ytop = i - 0.2, col = shadi, border = NA)
  }
  text(rep(-0.1, npal), (1:npal) - 0.6,
       labels = rev(pals),
       cex=0.6, xpd = TRUE, adj = 1)

}

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
#' # None
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
#' to top. Poor colormaps hide variation at the top.
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

  n <- 150
  cols64 <- pal(64)
  cols <- pal(n)
  
  # pal is a function

  
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
  x <- seq(0,3*pi,length=200)
  y <- seq(0,-2*pi,length=200)
  z <- outer(x,y, function(x,y) cos(x^2)*exp(y))
  image(z, col=cols, axes=FALSE)


  # Frequency ramp suggested by
  # http://inversed.ru/Blog_2.htm
  x <- seq(0,2*pi,length=200)
  y <- seq(0,-pi,length=200)
  z <- outer(x,y, function(x,y) y + x^2 * sin(64*y) / 12)
  image(z, col=cols, axes=FALSE)


  # Volcano
  image(volcano, col=cols, xaxt="n", yaxt="n")
  image(volcano, col=rev(cols), xaxt="n", yaxt="n")

  
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

  on.exit(op)
  par(op)
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

if(FALSE){
#lib(RColorBrewer)
# sequential max 9
blues <- colorRampPalette(brewer.pal(9,"Blues"))
pal.test(blues)
bugn <- colorRampPalette(brewer.pal(9,"BuGn"))
pal.test(bugn)
bupu <- colorRampPalette(brewer.pal(9,"BuPu"))
pal.test(bupu)
gnbu <- colorRampPalette(brewer.pal(9,"GnBu"))
pal.test(gnbu)
greens <- colorRampPalette(brewer.pal(9,"Greens"))
pal.test(greens)
greys <- colorRampPalette(brewer.pal(9,"Greys"))
pal.test(greys)
oranges <- colorRampPalette(brewer.pal(9,"Oranges"))
pal.test(oranges)
orrd <- colorRampPalette(brewer.pal(9,"OrRd"))
pal.test(orrd)
pubu <- colorRampPalette(brewer.pal(9,"PuBu"))
pal.test(pubu)
pubugn <- colorRampPalette(brewer.pal(9,"PuBuGn"))
pal.test(pubugn)
purd <- colorRampPalette(brewer.pal(9,"PuRd"))
pal.test(purd)
purples <- colorRampPalette(brewer.pal(9,"Purples"))
pal.test(purples)
rdpu <- colorRampPalette(brewer.pal(9,"RdPu"))
pal.test(rdpu)
reds <- colorRampPalette(brewer.pal(9,"Reds"))
pal.test(reds)
ylgn <- colorRampPalette(brewer.pal(9,"YlGn"))
pal.test(ylgn)
ylgnbu <- colorRampPalette(brewer.pal(9,"YlGnBu"))
pal.test(ylgnbu)
ylorbr <- colorRampPalette(brewer.pal(9,"YlOrBr"))
pal.test(ylorbr)
ylorrd <- colorRampPalette(brewer.pal(9,"YlOrRd"))
pal.test(ylorrd)

# div max 11
brbg <- colorRampPalette(brewer.pal(11,"BrBG"))
pal.test(brbg)
piyg <- colorRampPalette(brewer.pal(11,"PiYG"))
pal.test(piyg)
prgn <- colorRampPalette(brewer.pal(11,"PRGn"))
pal.test(prgn)
puor <- colorRampPalette(brewer.pal(11,"PuOr"))
pal.test(puor)
rdbu <- colorRampPalette(brewer.pal(11,"RdBu"))
pal.test(rdbu)
rdgy <- colorRampPalette(brewer.pal(11,"RdGy"))
pal.test(rdgy)
rdylbl <- colorRampPalette(brewer.pal(11,"RdYlBu"))
pal.test(rdylbl)
rdylgn <- colorRampPalette(brewer.pal(11,"RdYlGn"))
pal.test(rdylgn)
spectral <- colorRampPalette(brewer.pal(11,"Spectral"))
pal.test(spectral)

# qual
accent <- colorRampPalette(brewer.pal(8,"Accent"))
pal.test(accent, n=8)

dark2 <- colorRampPalette(brewer.pal(8,"Dark2"))
pal.test(dark2)

paired <- colorRampPalette(brewer.pal(12,"Paired"))
pal.test(paired)

pastel1 <- colorRampPalette(brewer.pal(9,"Pastel1"))
pal.test(pastel1)

pastel2 <- colorRampPalette(brewer.pal(8,"Pastel2"))
pal.test(pastel2)

set1 <- colorRampPalette(brewer.pal(9,"Set1"))
pal.test(set1)

set2 <- colorRampPalette(brewer.pal(8,"Set2"))
pal.test(set2)

set3 <- colorRampPalette(brewer.pal(12,"Set3"))
pal.test(set3)

pal.bands(c('accent','dark2','paired','pastel1','pastel2','set1','set2','set3'))
}

 

