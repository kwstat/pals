# discrete.r
# Time-stamp: <09 Dec 2016 15:31:29 c:/x/rpack/pals/R/discrete.R>
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
#' The \code{alphabet2} palette uses a similar idea with slightly different colors
#' and slightly different names.  This palette comes from the Polychrome package,
#' geenrated with the \code{createPalette} function and then manually
#' arranged and named.
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
#' colors, the purples are not very distinct, color 22 (olive green) is almost
#' identical to color 2 (black), etc.
#'
#' The \code{pal36} palette is also from the Polychrome package.
#' Colors were given a name from the ISCC-NBS standard.
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
#' # ----- alphabet2 -----
#' #  require(Polychrome)
#' #  ab2 <- createPalette(N=26,seedcolors=c("#E2E2E2","#474747","#F70000"))
#' #  pal.bands(ab2, alphabet2(), sort="hue", labels=c("createPalette","alphabet2"))
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
#' # ----- pal36 -----
#' #  require(Polychrome)
#' #  p36 <- createPalette(N=36,seedcolors=c("#474747", "#E2E2E2","#F70000"))
#' #  pal.bands(p36, pal36(), sort="hue", labels=c("createPalette","pal36"))
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
#'
#' pal.bands(alphabet, alphabet2, cols25, glasbey, kelly, pal36, stepped, tol, watlington)
#' 
#' @references
#' 
#' Robert M. Boynton. (1989)
#' Eleven Colors That Are Almost Never Confused. 
#' Proc. SPIE 1077, \emph{Human Vision, Visual Processing, and Digital Display}, 322-332.
#' http://doi.org/10.1117/12.952730
#'
#' Kevin R. Coombes (2016). Polychrome.
#' https://rdrr.io/rforge/Polychrome/man/alphabet.html
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
    rgb(240, 160, 255, maxColorValue=255, names="amethyst"),
    rgb(  0, 117, 220, maxColorValue=255, names="blue"),
    rgb(153,  63,   0, maxColorValue=255, names="caramel"),
    rgb( 76,   0,  92, maxColorValue=255, names="damson"),
    rgb( 25,  25,  25, maxColorValue=255, names="ebony"),
    rgb(  0,  92,  49, maxColorValue=255, names="forest"),
    rgb( 43, 206,  72, maxColorValue=255, names="green"),
    rgb(255, 204, 153, maxColorValue=255, names="honeydew"),
    rgb(128, 128, 128, maxColorValue=255, names="iron"),
    rgb(148, 255, 181, maxColorValue=255, names="jade"),
    rgb(143, 124,   0, maxColorValue=255, names="khaki"),
    rgb(157, 204,   0, maxColorValue=255, names="lime"),
    rgb(194,   0, 136, maxColorValue=255, names="magenta"),
    rgb(  0,  51, 128, maxColorValue=255, names="navy"),
    rgb(255, 164,   5, maxColorValue=255, names="orange"),
    rgb(255, 168, 187, maxColorValue=255, names="pink"),
    rgb( 66, 102,   0, maxColorValue=255, names="quagmire"),
    rgb(255,   0,  16, maxColorValue=255, names="red"),
    rgb( 94, 241, 242, maxColorValue=255, names="sky"),
    rgb(  0, 153, 143, maxColorValue=255, names="turquoise"),
    rgb(224, 255, 102, maxColorValue=255, names="uranium"),
    rgb(116,  10, 255, maxColorValue=255, names="violet"),
    rgb(153,   0,   0, maxColorValue=255, names="wine"),
    rgb(255, 255, 128, maxColorValue=255, names="xanthin"),
    rgb(255, 225,   0, maxColorValue=255, names="yellow"),
    rgb(255,  80,   5, maxColorValue=255, names="zinnia"))
  
  return(pal[1:n])
}

#' @export
#' @rdname discrete
alphabet2 <- function(n=26) {

  if(n > 26){
    message("Only 26 colors are available with 'alphabet2'")
    n <- 26
  }
  
  pal <- structure(c("#AA0DFE", "#3283FE", "#85660D", "#782AB6", "#565656", 
                     "#1C8356", "#16FF32", "#F7E1A0", "#E2E2E2", "#1CBE4F", "#C4451C", 
                     "#DEA0FD", "#FE00FA", "#325A9B", "#FEAF16", "#F8A19F", "#90AD1C", 
                     "#F6222E", "#1CFFCE", "#2ED9FF", "#B10DA1", "#C075A6", "#FC1CBF", 
                     "#B00068", "#FBE426", "#FA0087"),
                   .Names = c("amethyst", "blue", 
                              "caramel", "damson", "ebony", "forest", "green", "honey", "iron", 
                              "jade", "kingcrab", "lavender", "magenta", "navy", "orange", 
                              "pink", "quagmire", "red", "sea", "turquoise", "ultraviolet", 
                              "violet", "wine", "xanthin", "yellow", "zinnia"))
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
           "#565656",
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

# A character vector containing hexadecimal color representations of 36 distinctive colors that are well separated in the CIE L*u*v* color space. Each color is assigned a name from the ISCC-NBS standard.
# The color palette was generated using the createPalette function with three seed colors: ebony ("#474747"), iron ("#E2E2E2"), and red ("#F70000").


#' @rdname discrete
#' @export
pal36 <- function(n=36){
  
  if(n > 36) {
    message("Only 36 colors are available with 'pal36'.")
    n <- 36
  }

  pal <- structure(c("#5A5156", "#E4E1E3", "#F6222E", "#FE00FA", "#16FF32", 
                     "#3283FE", "#FEAF16", "#B00068", "#1CFFCE", "#90AD1C", "#2ED9FF", 
                     "#DEA0FD", "#AA0DFE", "#F8A19F", "#325A9B", "#C4451C", "#1C8356", 
                     "#85660D", "#B10DA1", "#FBE426", "#1CBE4F", "#FA0087", "#FC1CBF", 
                     "#F7E1A0", "#C075A6", "#782AB6", "#AAF400", "#BDCDFF", "#822E1C", 
                     "#B5EFB5", "#7ED7D1", "#1C7F93", "#D85FF7", "#683B79", "#66B0FF", 
                     "#3B00FB"),
                   .Names = c("Dark_Purplish_Gray", "Purplish_White", 
                              "Vivid_Red", "Vivid_Purple", "Vivid_Yellowish_Green", "Strong_Purplish_Blue", 
                              "Vivid_Orange_Yellow", "Vivid_Purplish_Red", "Brilliant_Green", 
                              "Vivid_Yellow_Green", "Vivid_Blue", "Brilliant_Purple", "Vivid_Violet", 
                              "Strong_Pink", "Strong_Blue", "Strong_Reddish_Orange", "Vivid_Green", 
                              "Light_Olive_Brown", "Vivid_Reddish_Purple", "Vivid_Greenish_Yellow", 
                              "Vivid_Yellowish_Green", "Vivid_Red", "Vivid_Purplish_Red", "Pale_Yellow", 
                              "Strong_Reddish_Purple", "Vivid_Violet", "Vivid_Yellow_Green", 
                              "Very_Light_Blue", "Strong_Reddish_Brown", "Very_Light_Yellowish_Green", 
                              "Very_Light_Bluish_Green", "Deep_Greenish_Blue", "Vivid_Purple", 
                              "Deep_Purple", "Brilliant_Blue", "Vivid_Violet"))
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



