# discrete.R
# Time-stamp: <26 Feb 2018 18:15:24 c:/x/rpack/pals/R/discrete.R>
# Copyright Kevin Wright, 2017, GPL-3

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
#' generated with the \code{createPalette} function and then manually
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
#' The \code{okabe} palette was design to be (1) clear for both colorblind and
#' non-colorblind people, (2) vividly colored, and (3) good for screen and printed.
#' The color-blind simulation tools in R suggest this palette is not as useful
#' as hoped.
#' 
#' The \code{polychrome} palette is also from the Polychrome package.
#' Colors were given a name from the ISCC-NBS standard.
#' 
#' The \code{stepped} palette has 24 colors (5 hues, 4 levels within each hue, plus
#' 4 shades of gray) that is useful for showing varying levels within categories.
#' Inspired by (http://geog.uoregon.edu/datagraphics/color_scales.htm), but in
#' order to better separate these colors in RGB space, red hue 0 was moved to hue 350,
#' green hue 80 moved to hue 90. The number of colors within each hue was reduced
#' from 5 to 4, and gray shades were added.
#'
#' The \code{tableau20} palette has 10 pairs of dark/light colors that are used by
#' the Tableau software.
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
#' pal.bands(alphabet, alphabet2, cols25, glasbey, kelly, okabe, polychrome,
#'   stepped, tableau20, tol, watlington)
#' 
#' \dontrun{
#' alphabet()
#' alphabet()["jade"]
#' pal.bands(alphabet,n=26)
#' pal.heatmap(alphabet)
#' # pal.cube(alphabet)
#' 
#' pal.heatmap(alphabet2)
#' 
#' pal.heatmap(cols25)
#'
#' pal.heatmap(glasbey(32))
#' # pal.cube(glasbey, n=32) # Blues are close together
#'
#' pal.heatmap(kelly(22)) # too many orange/pink colors
#'
#' pal.safe(okabe(8)) # not great
#' 
#' pal.heatmap(polychrome)
#' 
#' pal.heatmap(stepped, n=24)
#' 
#' pal.heatmap(tol, 12)
#' 
#' pal.heatmap(watlington(16))
#' }
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
#' Masataka Okabe and Kei Ito (2002).
#' Color Universal Design (CUD) - How to make figures and presentations
#' that are friendly to Colorblind people.
#' http://jfly.iam.u-tokyo.ac.jp/color/
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

# ----------------------------------------------------------------------------

#' @export
#' @rdname discrete
alphabet <- function(n=26) {
  
  if(n > 26){
    message("Only 26 colors are available with 'alphabet'")
    n <- 26
  }

  pal <- c("#F0A0FF","#0075DC","#993F00","#4C005C","#191919","#005C31",
           "#2BCE48","#FFCC99","#808080","#94FFB5","#8F7C00","#9DCC00",
           "#C20088","#003380","#FFA405","#FFA8BB","#426600","#FF0010",
           "#5EF1F2","#00998F","#E0FF66","#740AFF","#990000","#FFFF80",
           "#FFE100","#FF5005")
  names(pal) <- c("amethyst","blue","caramel","damson","ebony","forest",
                   "green","honeydew","iron","jade","khaki","lime","magenta",
                   "navy","orange","pink","quagmire","red","sky","turquoise",
                   "uranium","violet","wine","xanthin","yellow","zinnia") 
  
  return(pal[1:n])
}

# ----------------------------------------------------------------------------

#' @export
#' @rdname discrete
alphabet2 <- function(n=26) {

  if(n > 26){
    message("Only 26 colors are available with 'alphabet2'")
    n <- 26
  }
  
  pal <- c("#AA0DFE", "#3283FE", "#85660D", "#782AB6", "#565656", 
           "#1C8356", "#16FF32", "#F7E1A0", "#E2E2E2", "#1CBE4F",
           "#C4451C", "#DEA0FD", "#FE00FA", "#325A9B", "#FEAF16",
           "#F8A19F", "#90AD1C", "#F6222E", "#1CFFCE", "#2ED9FF",
           "#B10DA1", "#C075A6", "#FC1CBF", "#B00068", "#FBE426",
           "#FA0087")
  names(pal) <- c("amethyst", "blue", "caramel", "damson", "ebony",
                  "forest", "green", "honey", "iron", "jade",
                  "kingcrab", "lavender", "magenta", "navy",
                  "orange", "pink", "quagmire", "red", "sea",
                  "turquoise", "ultraviolet", "violet", "wine",
                  "xanthin", "yellow", "zinnia")
  return(pal[1:n])
}

# ----------------------------------------------------------------------------

#' @export
#' @rdname discrete
cols25 <- function(n=25) {
  if(n > 25) {
    message("Only 25 colors are available with 'cols25'.")
    n <- 25
  }
  ## rgb(31,120,200,maxColorValue=255), # "#1f78b4",
  ## "#ff0000", # red
  ## "#33a02c",
  ## rgb(106,51,194,maxColorValue=255), # "#6a3d9a", # mauve
  ## "#ff7f00",
  ## "#565656",
  ## rgb(255,215,0,maxColorValue=255), # "gold1",
  ## "#a6cee3",
  ## rgb(251,100,150,maxColorValue=255), # "#FB9A99", # lt pink
  ## "#b2df8a",
  ## "#CAB2D6", # lt purple
  ## "#FDBF6F", # lt orange
  ## rgb(153, 153, 153, maxColorValue=255), # "gray60",
  ## rgb(238, 230, 133, maxColorValue=255), # "khaki2",
  ## rgb(200,48,140,maxColorValue=255), #"maroon",
  ## rgb(255,131, 250, maxColorValue=255), # "orchid1",
  ## rgb(200,20,250,maxColorValue=255), # "#ff0ac8", # deeppink1
  ## rgb(0, 0, 255, maxColorValue=255), # "blue1",
  ## rgb(54, 100, 139, maxColorValue=255), # "steelblue4",
  ## rgb(0,226,229,maxColorValue=255), # "darkturquoise",
  ## rgb(0, 255, 0, maxColorValue=255), # "green1",
  ## rgb(119,139,0,maxColorValue=255), # "yellow4",
  ## rgb(190,190,0,maxColorValue=255), #"yellow3",
  ## rgb(139,59,0,maxColorValue=255), # "darkorange4",
  ## rgb(165,42,60,maxColorValue=255) # "brown"

  pal <- c("#1F78C8","#ff0000","#33a02c","#6A33C2","#ff7f00","#565656",
           "#FFD700","#a6cee3","#FB6496","#b2df8a","#CAB2D6","#FDBF6F",
           "#999999","#EEE685","#C8308C","#FF83FA","#C814FA","#0000FF",
           "#36648B","#00E2E5","#00FF00","#778B00","#BEBE00","#8B3B00",
           "#A52A3C")
  
  return(pal[1:n])
}

# ----------------------------------------------------------------------------

#' @export
#' @rdname discrete
glasbey <- function(n=32) {
  if(n > 32) {
    message("Only 32 colors are available with 'glasbey'.")
    n <- 32
  }

  return(syspals$glasbey[1:n])
}

# ----------------------------------------------------------------------------

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

#' @rdname discrete
#' @export
polychrome <- function(n=36){
  
  if(n > 36) {
    message("Only 36 colors are available with 'polychrome'.")
    n <- 36
  }

  pal <- c("#5A5156", "#E4E1E3", "#F6222E", "#FE00FA", "#16FF32", 
           "#3283FE", "#FEAF16", "#B00068", "#1CFFCE", "#90AD1C", "#2ED9FF", 
           "#DEA0FD", "#AA0DFE", "#F8A19F", "#325A9B", "#C4451C", "#1C8356", 
           "#85660D", "#B10DA1", "#FBE426", "#1CBE4F", "#FA0087", "#FC1CBF", 
           "#F7E1A0", "#C075A6", "#782AB6", "#AAF400", "#BDCDFF", "#822E1C", 
           "#B5EFB5", "#7ED7D1", "#1C7F93", "#D85FF7", "#683B79", "#66B0FF", 
           "#3B00FB")
  names(pal) <- c("Dark_Purplish_Gray", "Purplish_White", 
                  "Vivid_Red", "Vivid_Purple", "Vivid_Yellowish_Green", "Strong_Purplish_Blue", 
                  "Vivid_Orange_Yellow", "Vivid_Purplish_Red", "Brilliant_Green", 
                  "Vivid_Yellow_Green", "Vivid_Blue", "Brilliant_Purple", "Vivid_Violet", 
                  "Strong_Pink", "Strong_Blue", "Strong_Reddish_Orange", "Vivid_Green", 
                  "Light_Olive_Brown", "Vivid_Reddish_Purple", "Vivid_Greenish_Yellow", 
                  "Vivid_Yellowish_Green", "Vivid_Red", "Vivid_Purplish_Red", "Pale_Yellow", 
                  "Strong_Reddish_Purple", "Vivid_Violet", "Vivid_Yellow_Green", 
                  "Very_Light_Blue", "Strong_Reddish_Brown", "Very_Light_Yellowish_Green", 
                  "Very_Light_Bluish_Green", "Deep_Greenish_Blue", "Vivid_Purple", 
                  "Deep_Purple", "Brilliant_Blue", "Vivid_Violet")
  
  return(pal[1:n])
}

# ----------------------------------------------------------------------------

#' @export
#' @rdname discrete
stepped <- function(n=24) {
  
  if(n > 24) {
    message("Only 24 colors are available with 'stepped'")
    n <- 24
  }

  return(syspals$stepped[1:n])
}

# ----------------------------------------------------------------------------

#' @export
#' rdname discrete
okabe = function(n=8) {

  # (0,0,0), (230,159,0), (86,180,233) (0,158,115)
  # (240,228,66) (0,114,178) (213,94,0) (204,121,167)
  pal <- c("#000000","#E69F00","#56B4E9","#009E73",
           "#F0E442","#0072B2","#D55E00","#009E73")
  
  return(pal[1:n])
}

# ----------------------------------------------------------------------------

#' @export
#' @rdname discrete
tableau20 = function(n=20) {
  if(n > 20) {
    message("Only 20 colors are available with 'tableau20'.")
    n <- 20
  }

  # (31, 119, 180), (174, 199, 232), (255, 127, 14), (255, 187, 120),  
  # (44, 160, 44), (152, 223, 138), (214, 39, 40), (255, 152, 150),  
  # (148, 103, 189), (197, 176, 213), (140, 86, 75), (196, 156, 148),  
  # (227, 119, 194), (247, 182, 210), (127, 127, 127), (199, 199, 199),  
  # (188, 189, 34), (219, 219, 141), (23, 190, 207), (158, 218, 229)]
  
  pal <- c("#1F77B4","#AEC7E8","#FF7F0E","#FFBB78","#2CA02C","#98DF8A",
           "#D62728","#FF9896","#9467BD","#C5B0D5","#8C564B","#C49C94",
           "#E377C2","#F7B6D2","#7F7F7F","#C7C7C7","#BCBD22","#DBDB8D",
           "#17BECF","#9EDAE5")

  return(pal[1:n])
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

#' @export
#' @rdname discrete
watlington <- function(n=16) {
  if(n > 16) {
    message("Only 16 colors are available with 'watlington'.")
    n <- 16
  }
  return(syspals$watlington[1:n])
}

