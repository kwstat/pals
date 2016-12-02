# discrete.r
# Time-stamp: <29 Nov 2016 13:55:15 c:/x/rpack/pals/R/discrete.R>
# Copyright Kevin Wright, 2016, GPL-3

#' Discrete palettes
#'
#' Color palettes designed for discrete, categorical data with a small number of
#' categories.
#'
#' The \code{alphabet} palette has 26 distinguishable colors that have logical names
#' starting with the English alphabet letters A, B, ... Z.
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
#'
#' The \code{kelly} palette of 22 colors maximize the contrast
#' between colors in a set if the colors are chosen in sequential order.
#' Kelly paid attention to the needs of people with color blindness.  The
#' first nine colors work well for such people and people with normal vision.
#' Kelly did not provide RGB color values, and the paper was in black-and-white.
#' A color image of the Kelly palette can be found in Green-Armytage (2010).
#' The color 'white' has been re-defined as light-gray, #F2F3F4.
#' Commentary: We think the kelly palette has an over-abundance of orange-ish
#' colors, the purples are not very distinct, etc.
#'
#' The \code{stepped} palette has 20 colors (5 hues, 4 levels within each hue)
#' that is useful for showing varying levels within categories.
#' In order to better separate these colors in RGB space, we
#' moved red hue 0 to hue 350, green hue 80 to hue 90 
#' 
#' The \code{tol} palette has 12 colors by Paul Tol.
#' 
#' The \code{watlington} palette has 16 colors.
#' The color 'white' has been re-defined as light-gray, #F2F3F4.
#' 
#' @param n Number of colors to return.
#' 
#' @return A vector of colors as hex strings.
#' 
#' @author Palette colors by various authors. R code by Kevin Wright.
#' 
#' @examples
#'
#' # ----- alphabet -----
#' alphabet()
#' alphabet()["jade"]
#' pal.bands(alphabet,n=26)
#' pal.heatmap(alphabet)
#' pal.cube(alphabet)
#'
#' # ----- cols25 -----
#' pal.bands(cols25,n=25)
#' pal.heatmap(cols25)
#'
#' # ------ glasbey ------
#' pal.bands(glasbey,n=32)
#' pal.heatmap(glasbey(32))
#' pal.cube(glasbey, n=32) # Blues are close together
#'
#' # ----- kelly -----
#' pal.bands(kelly,n=22)
#' pal.heatmap(kelly(22)) # too many orange/pink colors
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
    message("Only 16 colors are available with 'watlington'.")
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
    message("Only 22 colors are available with 'kelly'.")
    n <- 22
  }
  return(syspals$kelly[1:n])
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

