# colormaps.R
# Time-stamp: <06 Sep 2018 14:15:08 c:/x/rpack/pals/R/colormaps.R>
# Copyright: Kevin Wright, 2017. License: GPL-3.

# R auto-loads R/sysdata.rda, which contains the 'syspals' object.
# You would think that this code should work:
#   jet1 = colorRampPalette(syspals$jet)
# However, after building the package and loading, R cannot find 'syspals'.
# Instead, wrap that into a function:
#   jet2 = function(n) colorRampPalette(syspals$jet)(n)
# Also, jet1 is 48KB, while jet2 is only 2.3 KB

# ----------------------------------------------------------------------------


#' Miscellaneous colormaps
#'
#' Colormaps designed for continuous data.
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
#' Specific colors were adapted from the BIDS/colormap package.
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
#' The \code{cividis} palette by  Jamie R. Nuñez, Christopher R. Anderton, Ryan S. Renslow,
#' is a variation of viridis that is less colorful.
#' 
#' @param n Number of colors to return.
#' 
#' @return A vector of colors.
#' 
#' @examples
#'
#' pal.bands(coolwarm, cubehelix, gnuplot, jet, parula, tol.rainbow, cividis)
#' 
#' if(FALSE){
#' 
#' # ----- coolwarm -----
#' pal.test(coolwarm) # Minimal mach banding
#' # Note the mach banding gray line in the following:
#' # pal.volcano(colorRampPalette(c("#3B4CC0", "lightgray", "#B40426")))
#'
#' # ----- cubehelix -----
#' # Full range of colors. Pink is overwhelming. Not the best choice.
#' pal.test(cubehelix)
#' 
#' # Mostly blues/greens. Dark areas severely too black. 
#' # Similar, but more saturated. See: http://inversed.ru/Blog_2.htm
#' pal.volcano(function(n) cubehelix(n, start=.25, r=-.67, hue=1.5))
#'
#' # Dark colors totally lose structure of the volcano peak.
#' op <- par(mfrow=c(2,2), mar=c(2,2,2,2))
#' image(volcano, col = cubehelix(51), asp = 1, axes=0, main="cubehelix")
#' image(volcano, col = cubehelix(51, start=.25, r=-.67, hue=1.5), asp = 1, axes=0, main="cubehelix")
#' image(volcano, col = rev(cubehelix(51)), asp = 1, axes=0, main="cubehelix")
#' image(volcano, col = rev(cubehelix(51, start=.25, r=-.67, hue=1.5)), 
#'                                    asp = 1, axes=0, main="cubehelix")
#' par(op)
#'
#' # ----- gnuplot -----
#' pal.test(gnuplot)
#'
#' # ----- jet -----
#' # pal.volcano(jet)
#' pal.test(jet)
#'
#' # ----- parula -----
#' # pal.volcano(parula)
#' pal.test(parula)
#'
#' # ----- tol.rainbow -----
#' # pal.volcano(tol.rainbow)
#' pal.test(tol.rainbow)
#' 
#' } 
#' 
#' # ----- cividis -----
#' # pal.volcano(cividis)
#' pal.test(cividis)
#' 
#' @author Palette colors by various authors. R code by Kevin Wright.
#' 
#' @references
#' 
#' Dave A. Green. (2011).
#' A colour scheme for the display of astronomical intensity images.
#' Bull. Astr. Soc. India, 39, 289-295.
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
#' My Favorite Colormap. (gnuplot)
#' https://web.archive.org/web/20040119000943/http://www.ihe.uni-karlsruhe.de/mitarbeiter/vonhagen/palette.en.html
#'
#' MathWorks documentation.
#' http://www.mathworks.com/help/matlab/ref/colormap.html
#'
#' BIDS/colormap.
#' https://github.com/BIDS/colormap/blob/master/parula.py
#' 
#' Jamie R. Nuñez, Christopher R. Anderton, Ryan S. Renslow (2017).
#' An optimized colormap for the scientific community.
#' https://arxiv.org/abs/1712.01662
#' 
#' @name continuous
NULL

#' @param start Start angle (radians) of the helix
#' @param r  Number of rotations of the helix
#' @param hue Saturation of the colors, 0 = grayscale, 1 = fully saturated
#' @param gamma gamma < 1 emphasizes low intensity values, gamma > 1 emphasizes high intensity values
#' @export
#' @rdname continuous
cubehelix <- function(n=25, start = 0.5, r = -1.5, hue = 1, gamma = 1) {
  
   lambda <- seq(0, 1, length=n) # value along the black-white diagonal
  
  lgam <- rep(lambda^gamma, each = 3)
  phi <- 2 * pi * (start/3 + r * lambda) # angle for deviation
  amp <- hue * lgam * (1 - lgam)/2 # amplitude
  M <- matrix(c(-0.14861, -0.29227, 1.97294, 1.78277, -0.90649, 0),
             byrow=FALSE, ncol = 2)
  out <- lgam + amp * (M %*% rbind(cos(phi), sin(phi))) # Eqn 2
  
  # truncate values into [0,1]
  out <- pmin(pmax(out, 0), 1)

  # Convert to rgb value
  rgb(t(out))

}


#' @param trim Proportion of tail colors to trim, default 0.1
#' 
#' @export 
#' @rdname continuous
gnuplot <- function (n = 25, trim = 0.1) {
  if (trim >= 1 || trim < 0) 
    stop("trim should be in [0, 1]")
  i <- seq(0.5 * trim, 1 - 0.5 * trim, length = n)

  R <- ifelse(i < 0.25, 0, ifelse(i < 0.57, i/0.32 - 0.78125, 1))
  G <- ifelse(i < 0.42, 0, ifelse(i < 0.92, 2 * i - 0.84, 1))
  B <- ifelse(i < 0.25, 4 * i, ifelse(i < 0.42, 1, ifelse(i < 0.92, -2 * i + 1.84, i/0.08 - 11.5)))
  rgb(R, G, B)
}


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
                       119, 170, 221, 119, 170, 204, 68, 119, 170, 17, 68, 119, 17, 68, 119, 34, 85, 136, 85, 136, 187),
                     byrow=FALSE, ncol=3)
    banded <- rgb(banded, maxColorValue=255)
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
  } else { # n >= 22
    x <- seq(from=0, to=1, length=n)
  }
  # Eqn 3 of Tol's tech report  
  R <- (0.472-0.567*x+4.05*x^2)/(1+8.72*x-19.17*x^2+14.1*x^3)
  G <- 0.108932 - 1.22635*x + 27.284*x^2 - 98.577*x^3 + 163.3*x^4 - 131.395*x^5 + 40.634*x^6
  B <- 1/(1.97 + 3.54*x - 68.5*x^2 + 243*x^3-297*x^4 + 125*x^5)
  return(rgb(R,G,B))
  
}

#' @export
#' @rdname continuous
jet <- function(n=25) colorRampPalette(syspals$jet)(n)
  
#' @export
#' @rdname continuous
parula <- function(n=25) colorRampPalette(syspals$parula)(n)
 
#' @export
#' @rdname continuous
coolwarm <- function(n=25) colorRampPalette(syspals$coolwarm)(n)

#' @export
#' @rdname continuous
warmcool <- function(n=25){
  rev(coolwarm(n))
}

#' @export
#' @rdname continuous
cividis <- function(n=25) colorRampPalette(syspals$cividis)(n)

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------


#' ColorBrewer palettes
#'
#' These functions provide a unified access to the ColorBrewer palettes.
#'
#' The palette names begin with 'brewer' to make it easier to use auto-completion.
#' 
#' @examples
#'
#' # Sequential
#' pal.bands(brewer.blues, brewer.bugn, brewer.bupu, brewer.gnbu, brewer.greens,
#'           brewer.greys, brewer.oranges, brewer.orrd, brewer.pubu, brewer.pubugn,
#'           brewer.purd, brewer.purples, brewer.rdpu, brewer.reds, brewer.ylgn,
#'           brewer.ylgnbu, brewer.ylorbr, brewer.ylorrd)
#' 
#' # Diverging
#' pal.bands(brewer.brbg, brewer.piyg, brewer.prgn, brewer.puor, brewer.rdbu,
#'           brewer.rdgy, brewer.rdylbu, brewer.rdylgn, brewer.spectral)
#' 
#' # Qualtitative
#' pal.bands(brewer.accent(8), brewer.dark2(8), brewer.paired(12), brewer.pastel1(9),
#'           brewer.pastel2(8), brewer.set1(9), brewer.set2(8), brewer.set3(10),
#'           labels=c("brewer.accent", "brewer.dark2", "brewer.paired", "brewer.pastel1",
#'           "brewer.pastel2", "brewer.set1", "brewer.set2", "brewer.set3"))
#'
#' \dontrun{
#' 
#' # Sequential
#' pal.test(brewer.blues)
#' pal.test(brewer.bugn)
#' pal.test(brewer.bupu)
#' pal.test(brewer.gnbu)
#' pal.test(brewer.greens)
#' pal.test(brewer.greys)
#' pal.test(brewer.oranges)
#' pal.test(brewer.orrd) 
#' pal.test(brewer.pubu) # good
#' pal.test(brewer.pubugn) # good
#' pal.test(brewer.purd)
#' pal.test(brewer.purples)
#' pal.test(brewer.rdpu)
#' pal.test(brewer.reds)
#' pal.test(brewer.ylgn)
#' pal.test(brewer.ylgnbu)
#' pal.test(brewer.ylorbr)
#' pal.test(brewer.ylorrd)
#' 
#' # Diverging, max n=11 colors
#' pal.test(brewer.brbg)
#' pal.test(brewer.piyg)
#' pal.test(brewer.prgn)
#' pal.test(brewer.puor)
#' pal.test(brewer.rdbu)
#' pal.test(brewer.rdgy)
#' pal.test(brewer.rdylbu)
#' pal.test(brewer.rdylgn)
#' pal.test(brewer.spectral)
#'
#' # Qualtitative. These are weird...don't do this
#' pal.test(brewer.accent)
#' pal.test(brewer.dark2)
#' pal.test(brewer.paired)
#' pal.test(brewer.pastel1)
#' pal.test(brewer.pastel2)
#' pal.test(brewer.set1)
#' pal.test(brewer.set2)
#' pal.test(brewer.set3)
#'
#' # Need to move these to 'tests'         
#' pal.bands(brewer.accent(3), brewer.accent(4), brewer.accent(5), brewer.accent(6),
#'           brewer.accent(7), brewer.accent(8), brewer.accent(9), brewer.accent(10),
#'           brewer.accent(11), brewer.accent(12))
#' #brewer.purd(1) # Should err
#' #brewer.purd(2) # Should err
#' brewer.purd(3)
#' brewer.purd(9)
#' brewer.purd(25)
#' pal.bands(brewer.purd(3), brewer.purd(4), brewer.purd(5), brewer.purd(6),
#'           brewer.purd(7), brewer.purd(8), brewer.purd(9), brewer.purd(10),
#'           brewer.purd(11), brewer.purd(12), brewer.purd(13), brewer.purd(14),
#'           brewer.purd(15), brewer.purd(100))
#' }
#'
#' @name brewer
NULL

get.brewer.pal <- function(bpal, n){
  # bpal is a brewer palette list, list item k-2 is a vector of k color
  rng <- as.numeric(names(bpal))
  maxn <- max(rng)
  if(n < 3) warning("Only accepts n>2")
  if(n <= maxn) {
    return(bpal[[n-2]])
  } else {
    return(colorRampPalette(bpal[[maxn-2]])(n))
  }
}

# ----------------------------------------------------------------------------
# quantitative

#' @param n The number of colors to display for palette functions.
#' @return A vector of colors.
#' @export
#' @rdname brewer
brewer.blues <- function(n) get.brewer.pal(syspals$brewer.blues, n)

#' @export
#' @rdname brewer
brewer.bugn <- function(n) get.brewer.pal(syspals$brewer.bugn, n)

#' @export
#' @rdname brewer
brewer.bupu <- function(n) get.brewer.pal(syspals$brewer.bupu, n)

#' @export
#' @rdname brewer
brewer.gnbu <- function(n) get.brewer.pal(syspals$brewer.gnbu, n)

#' @export
#' @rdname brewer
brewer.greens <- function(n) get.brewer.pal(syspals$brewer.greens, n)

#' @export
#' @rdname brewer
brewer.greys <- function(n) get.brewer.pal(syspals$brewer.greys, n)

#' @export
#' @rdname brewer
brewer.oranges <- function(n) get.brewer.pal(syspals$brewer.oranges, n)

#' @export
#' @rdname brewer
brewer.orrd <- function(n) get.brewer.pal(syspals$brewer.orrd, n)

#' @export
#' @rdname brewer
brewer.pubu <- function(n) get.brewer.pal(syspals$brewer.pubu, n)

#' @export
#' @rdname brewer
brewer.pubugn <- function(n) get.brewer.pal(syspals$brewer.pubugn, n)

#' @export
#' @rdname brewer
brewer.purd <- function(n) get.brewer.pal(syspals$brewer.purd, n)

#' @export
#' @rdname brewer
brewer.purples <- function(n) get.brewer.pal(syspals$brewer.purples, n)

#' @export
#' @rdname brewer
brewer.rdpu <- function(n) get.brewer.pal(syspals$brewer.rdpu, n)

#' @export
#' @rdname brewer
brewer.reds <- function(n) get.brewer.pal(syspals$brewer.reds, n)

#' @export
#' @rdname brewer
brewer.ylgn <- function(n) get.brewer.pal(syspals$brewer.ylgn, n)

#' @export
#' @rdname brewer
brewer.ylgnbu <- function(n) get.brewer.pal(syspals$brewer.ylgnbu, n)

#' @export
#' @rdname brewer
brewer.ylorbr <- function(n) get.brewer.pal(syspals$brewer.ylorbr, n)

#' @export
#' @rdname brewer
brewer.ylorrd <- function(n) get.brewer.pal(syspals$brewer.ylorrd, n)

# ----------------------------------------------------------------------------
# diverging
#' @export
#' @rdname brewer
brewer.brbg <- function(n) get.brewer.pal(syspals$brewer.brbg, n)

#' @export
#' @rdname brewer
brewer.piyg <- function(n) get.brewer.pal(syspals$brewer.piyg, n)

#' @export
#' @rdname brewer
brewer.prgn <- function(n) get.brewer.pal(syspals$brewer.prgn, n)

#' @export
#' @rdname brewer
brewer.puor <- function(n) get.brewer.pal(syspals$brewer.puor, n)

#' @export
#' @rdname brewer
brewer.rdbu <- function(n) get.brewer.pal(syspals$brewer.rdbu, n)

#' @export
#' @rdname brewer
brewer.rdgy <- function(n) get.brewer.pal(syspals$brewer.rdgy, n)

#' @export
#' @rdname brewer
brewer.rdylbu <- function(n) get.brewer.pal(syspals$brewer.rdylbu, n)

#' @export
#' @rdname brewer
brewer.rdylgn <- function(n) get.brewer.pal(syspals$brewer.rdylgn, n)

#' @export
#' @rdname brewer
brewer.spectral <- function(n) get.brewer.pal(syspals$brewer.spectral, n)

# ----------------------------------------------------------------------------
# qualitative

#' @export
#' @rdname brewer
brewer.accent <- function(n) get.brewer.pal(syspals$brewer.accent, n)

#' @export
#' @rdname brewer
brewer.dark2 <- function(n) get.brewer.pal(syspals$brewer.dark2, n)

#' @export
#' @rdname brewer
brewer.paired <- function(n) get.brewer.pal(syspals$brewer.paired, n)

#' @export
#' @rdname brewer
brewer.pastel1 <- function(n) get.brewer.pal(syspals$brewer.pastel1, n)

#' @export
#' @rdname brewer
brewer.pastel2 <- function(n) get.brewer.pal(syspals$brewer.pastel2, n)

#' @export
#' @rdname brewer
brewer.set1 <- function(n) get.brewer.pal(syspals$brewer.set1, n)

#' @export
#' @rdname brewer
brewer.set2 <- function(n) get.brewer.pal(syspals$brewer.set2, n)

#' @export
#' @rdname brewer
brewer.set3 <- function(n) get.brewer.pal(syspals$brewer.set3, n)


# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------


#' Oceanography perceptually uniform colormaps
#'
#' These palettes have been designed to be a collection of perceptually
#' uniform colormaps designed for oceanographic data display.
#'
#' The 'oxy' palette does not include gray as shown in Thyng (2016).
#' 
#' The 'balance', 'delta', and 'curl' palettes were originally given
#' as 2*256 colors (256 each for the left and right half of the palette)
#' and have been downsampled to 256 colors.
#'
#' The palettes from matplotlib have been converted from RGB codes to
#' hexadecimal strings for use in this package.
#'
#' @param n Number of colors
#' 
#' @return None
#' 
#' @author Palette colors by Kristen Thyng. R code by Kevin Wright
#' 
#' @examples
#'
#' pal.bands(ocean.thermal, ocean.haline, ocean.solar, ocean.ice, ocean.gray,
#'           ocean.oxy, ocean.deep, ocean.dense, ocean.algae, ocean.matter,
#'           ocean.turbid, ocean.speed, ocean.amp, ocean.tempo, ocean.phase,
#'           ocean.balance, ocean.delta, ocean.curl, main="Ocean palettes")
#' 
#' \dontrun{
#' pal.test(ocean.thermal)
#' pal.test(ocean.haline) # better than parula!
#' pal.test(ocean.solar)
#' pal.test(ocean.ice)
#' pal.test(ocean.gray)
#' pal.test(ocean.oxy)
#' pal.test(ocean.deep)
#' pal.test(ocean.dense)
#' pal.test(ocean.algae)
#' pal.test(ocean.matter)
#' pal.test(ocean.turbid)
#' pal.test(ocean.speed)
#' pal.test(ocean.amp)
#' pal.test(ocean.tempo)
#' pal.test(ocean.phase)
#' pal.test(ocean.balance)
#' pal.test(ocean.delta)
#' pal.test(ocean.curl)
#' }
#' 
#' @references
#' Thyng, K.M., C.A. Greene, R.D. Hetland, H.M. Zimmerle, and S.F. DiMarco (2016).
#' True colors of oceanography: Guidelines for effective and accurate colormap selection.
#' \emph{Oceanography}, 29(3):9-13, http://dx.doi.org/10.5670/oceanog.2016.66.
#' 
#' @name ocean
NULL

#' @export
#' @rdname ocean
ocean.algae <- function(n) colorRampPalette(syspals$ocean.algae)(n)

#' @export
#' @rdname ocean
ocean.deep <- function(n) colorRampPalette(syspals$ocean.deep)(n)

#' @export
#' @rdname ocean
ocean.dense <- function(n) colorRampPalette(syspals$ocean.dense)(n)

#' @export
#' @rdname ocean
ocean.gray <- function(n) colorRampPalette(syspals$ocean.gray)(n)

#' @export
#' @rdname ocean
ocean.haline <- function(n) colorRampPalette(syspals$ocean.haline)(n)

#' @export
#' @rdname ocean
ocean.ice <- function(n) colorRampPalette(syspals$ocean.ice)(n)

#' @export
#' @rdname ocean
ocean.matter <- function(n) colorRampPalette(syspals$ocean.matter)(n)

#' @export
#' @rdname ocean
ocean.oxy <- function(n) colorRampPalette(syspals$ocean.oxy)(n)

#' @export
#' @rdname ocean
ocean.phase <- function(n) colorRampPalette(syspals$ocean.phase)(n)

#' @export
#' @rdname ocean
ocean.solar <- function(n) colorRampPalette(syspals$ocean.solar)(n)

#' @export
#' @rdname ocean
ocean.thermal <- function(n) colorRampPalette(syspals$ocean.thermal)(n)

#' @export
#' @rdname ocean
ocean.turbid <- function(n) colorRampPalette(syspals$ocean.turbid)(n)

#' @export
#' @rdname ocean
ocean.balance <- function(n) colorRampPalette(syspals$ocean.balance)(n)

#' @export
#' @rdname ocean
ocean.curl <- function(n) colorRampPalette(syspals$ocean.curl)(n)

#' @export
#' @rdname ocean
ocean.delta <- function(n) colorRampPalette(syspals$ocean.delta)(n)

#' @export
#' @rdname ocean
ocean.amp <- function(n) colorRampPalette(syspals$ocean.amp)(n)

#' @export
#' @rdname ocean
ocean.speed <- function(n) colorRampPalette(syspals$ocean.speed)(n)

#' @export
#' @rdname ocean
ocean.tempo <- function(n) colorRampPalette(syspals$ocean.tempo)(n)


# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

#' Matteo Niccoli's perceptually uniform colormaps
#'
#' These colormaps are intended by be more perceptually balanced than traditional
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
#' 
#' @return A vector of colors
#' 
#' @author Palettes by Matteo Niccoli. R code by Kevin Wright.
#' 
#' @examples
#' pal.bands(cubicyf,cubicl,isol,linearl,linearlhot)
#' pal.test(cubicyf) # purple blue green
#' pal.test(cubicl) # purple blue green orange
#' # pal.test(isol) # magenta blue green red. Poor in green area.
#' # pal.test(linearl) # black blue green tan. Poor in black area.
#' # pal.test(linearlhot) # black red yellow
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
cubicyf <- function(n) colorRampPalette(syspals$cubicyf)(n)

#' @rdname niccoli
#' @export 
isol <- function(n) colorRampPalette(syspals$isol)(n)

#' @rdname niccoli
#' @export
cubicl <- function(n) colorRampPalette(syspals$cubicl)(n)

#' @rdname niccoli
#' @export
linearl <- function(n) colorRampPalette(syspals$linearl)(n)

#' @rdname niccoli
#' @export 
linearlhot <- function(n) colorRampPalette(syspals$linearlhot)(n)

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

#' Matplotlib colormaps
#' 
#' Viridis family of colormaps as found in Matplotlib. Designed to be perceptually 
#' uniform, but generally too dark to be useful.
#' 
#' @param n Number of colors to return
#' 
#' @return A vector of colors
#' 
#' @author Palettes by Matteo Niccoli. R code by Kevin Wright.
#' 
#' @examples
#' pal.bands(magma, inferno, plasma, viridis)
#' 
#' @name matplotlib
NULL

#' @export
#' @rdname matplotlib
magma <- function(n) colorRampPalette(syspals$magma)(n)

#' @export
#' @rdname matplotlib
inferno <- function(n) colorRampPalette(syspals$inferno)(n)

#' @export
#' @rdname matplotlib
plasma <- function(n) colorRampPalette(syspals$plasma)(n)

#' @export
#' @rdname matplotlib
viridis <- function(n) colorRampPalette(syspals$viridis)(n)

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

#' Peter Kovesi's perceptually uniform colormaps
#'
#' Peter Kovesi's perceptually uniform colormaps
#'
#' All colormaps are named using Peter Kovesi's naming scheme:
#' <category>_<huesequence>_<lightnessrange>_c<meanchroma>_s<colorshift>
#'
#' Note: \code{kovesi.rainbow} is another name for \code{rainbow_bgyr_35_85_c72}.
#' 
#' @examples
#' 
#' if(FALSE){
#' pal.bands(kovesi.cyclic_grey_15_85_c0, kovesi.cyclic_grey_15_85_c0_s25,
#' kovesi.cyclic_mrybm_35_75_c68, kovesi.cyclic_mrybm_35_75_c68_s25,
#' kovesi.cyclic_mygbm_30_95_c78, kovesi.cyclic_mygbm_30_95_c78_s25,
#' kovesi.cyclic_wrwbw_40_90_c42, kovesi.cyclic_wrwbw_40_90_c42_s25,
#' kovesi.diverging_isoluminant_cjm_75_c23, kovesi.diverging_isoluminant_cjm_75_c24,
#' kovesi.diverging_isoluminant_cjo_70_c25, kovesi.diverging_linear_bjr_30_55_c53,
#' kovesi.diverging_linear_bjy_30_90_c45, kovesi.diverging_rainbow_bgymr_45_85_c67,
#' kovesi.diverging_bkr_55_10_c35, kovesi.diverging_bky_60_10_c30,
#' kovesi.diverging_bwr_40_95_c42, kovesi.diverging_bwr_55_98_c37,
#' kovesi.diverging_cwm_80_100_c22, kovesi.diverging_gkr_60_10_c40,
#' kovesi.diverging_gwr_55_95_c38, kovesi.diverging_gwv_55_95_c39,
#' kovesi.isoluminant_cgo_70_c39, kovesi.isoluminant_cgo_80_c38,
#' kovesi.isoluminant_cm_70_c39, kovesi.linear_bgy_10_95_c74,
#' kovesi.linear_bgyw_15_100_c67, kovesi.linear_bgyw_15_100_c68,
#' kovesi.linear_blue_5_95_c73, kovesi.linear_blue_95_50_c20,
#' kovesi.linear_bmw_5_95_c86, kovesi.linear_bmw_5_95_c89,
#' kovesi.linear_bmy_10_95_c71, kovesi.linear_bmy_10_95_c78,
#' kovesi.linear_gow_60_85_c27, kovesi.linear_gow_65_90_c35,
#' kovesi.linear_green_5_95_c69, kovesi.linear_grey_0_100_c0,
#' kovesi.linear_grey_10_95_c0, kovesi.linear_kry_5_95_c72,
#' kovesi.linear_kry_5_98_c75, kovesi.linear_kryw_5_100_c64,
#' kovesi.linear_kryw_5_100_c67, kovesi.linear_ternary_blue_0_44_c57,
#' kovesi.linear_ternary_green_0_46_c42, kovesi.linear_ternary_red_0_50_c52,
#' kovesi.rainbow_bgyr_35_85_c72, kovesi.rainbow_bgyr_35_85_c73,
#' kovesi.rainbow_bgyrm_35_85_c69, kovesi.rainbow_bgyrm_35_85_c71)
#' }
#' 
#' @author Colormaps by Peter Kovesi. R code by Kevin Wright.
#'
#'
#' @references
#'
#' Peter Kovesi (2016). CET Perceptually Uniform Colour Maps.
#' http://peterkovesi.com/projects/colourmaps/
#' 
#' Peter Kovesi (2015). Good Colour Maps: How to Design Them.
#' Arxiv. https://arxiv.org/abs/1509.03700
#'
#' 
#' https://bokeh.github.io/colorcet/
#' 
#' @name kovesi
NULL

#' @param n The number of colors to display for palette functions.
#' @return A vector of colors.
#' @export
#' @rdname kovesi
kovesi.cyclic_grey_15_85_c0 <- function(n) colorRampPalette(syspals$kovesi.cyclic_grey_15_85_c0)(n)

#' @export
#' @rdname kovesi
kovesi.cyclic_grey_15_85_c0_s25 <- function(n) colorRampPalette(syspals$kovesi.cyclic_grey_15_85_c0_s25)(n)

#' @export
#' @rdname kovesi
kovesi.cyclic_mrybm_35_75_c68 <- function(n) colorRampPalette(syspals$kovesi.cyclic_mrybm_35_75_c68)(n)

#' @export
#' @rdname kovesi
kovesi.cyclic_mrybm_35_75_c68_s25 <- function(n) colorRampPalette(syspals$kovesi.cyclic_mrybm_35_75_c68_s25)(n)

#' @export
#' @rdname kovesi
kovesi.cyclic_mygbm_30_95_c78 <- function(n) colorRampPalette(syspals$kovesi.cyclic_mygbm_30_95_c78)(n)

#' @export
#' @rdname kovesi
kovesi.cyclic_mygbm_30_95_c78_s25 <- function(n) colorRampPalette(syspals$kovesi.cyclic_mygbm_30_95_c78_s25)(n)

#' @export
#' @rdname kovesi
kovesi.cyclic_wrwbw_40_90_c42 <- function(n) colorRampPalette(syspals$kovesi.cyclic_wrwbw_40_90_c42)(n)

#' @export
#' @rdname kovesi
kovesi.cyclic_wrwbw_40_90_c42_s25 <- function(n) colorRampPalette(syspals$kovesi.cyclic_wrwbw_40_90_c42_s25)(n)

#' @export
#' @rdname kovesi
kovesi.diverging_isoluminant_cjm_75_c23 <- function(n) colorRampPalette(syspals$kovesi.diverging_isoluminant_cjm_75_c23)(n)

#' @export
#' @rdname kovesi
kovesi.diverging_isoluminant_cjm_75_c24 <- function(n) colorRampPalette(syspals$kovesi.diverging_isoluminant_cjm_75_c24)(n)

#' @export
#' @rdname kovesi
kovesi.diverging_isoluminant_cjo_70_c25 <- function(n) colorRampPalette(syspals$kovesi.diverging_isoluminant_cjo_70_c25)(n)

#' @export
#' @rdname kovesi
kovesi.diverging_linear_bjr_30_55_c53 <- function(n) colorRampPalette(syspals$kovesi.diverging_linear_bjr_30_55_c53)(n)

#' @export
#' @rdname kovesi
kovesi.diverging_linear_bjy_30_90_c45 <- function(n) colorRampPalette(syspals$kovesi.diverging_linear_bjy_30_90_c45)(n)

#' @export
#' @rdname kovesi
kovesi.diverging_rainbow_bgymr_45_85_c67 <- function(n) colorRampPalette(syspals$kovesi.diverging_rainbow_bgymr_45_85_c67)(n)

#' @export
#' @rdname kovesi
kovesi.diverging_bkr_55_10_c35 <- function(n) colorRampPalette(syspals$kovesi.diverging_bkr_55_10_c35)(n)

#' @export
#' @rdname kovesi
kovesi.diverging_bky_60_10_c30 <- function(n) colorRampPalette(syspals$kovesi.diverging_bky_60_10_c30)(n)

#' @export
#' @rdname kovesi
kovesi.diverging_bwr_40_95_c42 <- function(n) colorRampPalette(syspals$kovesi.diverging_bwr_40_95_c42)(n)

#' @export
#' @rdname kovesi
kovesi.diverging_bwr_55_98_c37 <- function(n) colorRampPalette(syspals$kovesi.diverging_bwr_55_98_c37)(n)

#' @export
#' @rdname kovesi
kovesi.diverging_cwm_80_100_c22 <- function(n) colorRampPalette(syspals$kovesi.diverging_cwm_80_100_c22)(n)

#' @export
#' @rdname kovesi
kovesi.diverging_gkr_60_10_c40 <- function(n) colorRampPalette(syspals$kovesi.diverging_gkr_60_10_c40)(n)

#' @export
#' @rdname kovesi
kovesi.diverging_gwr_55_95_c38 <- function(n) colorRampPalette(syspals$kovesi.diverging_gwr_55_95_c38)(n)

#' @export
#' @rdname kovesi
kovesi.diverging_gwv_55_95_c39 <- function(n) colorRampPalette(syspals$kovesi.diverging_gwv_55_95_c39)(n)

#' @export
#' @rdname kovesi
kovesi.isoluminant_cgo_70_c39 <- function(n) colorRampPalette(syspals$kovesi.isoluminant_cgo_70_c39)(n)

#' @export
#' @rdname kovesi
kovesi.isoluminant_cgo_80_c38 <- function(n) colorRampPalette(syspals$kovesi.isoluminant_cgo_80_c38)(n)

#' @export
#' @rdname kovesi
kovesi.isoluminant_cm_70_c39 <- function(n) colorRampPalette(syspals$kovesi.isoluminant_cm_70_c39)(n)

#' @export
#' @rdname kovesi
kovesi.linear_bgy_10_95_c74 <- function(n) colorRampPalette(syspals$kovesi.linear_bgy_10_95_c74)(n)

#' @export
#' @rdname kovesi
kovesi.linear_bgyw_15_100_c67 <- function(n) colorRampPalette(syspals$kovesi.linear_bgyw_15_100_c67)(n)

#' @export
#' @rdname kovesi
kovesi.linear_bgyw_15_100_c68 <- function(n) colorRampPalette(syspals$kovesi.linear_bgyw_15_100_c68)(n)

#' @export
#' @rdname kovesi
kovesi.linear_blue_5_95_c73 <- function(n) colorRampPalette(syspals$kovesi.linear_blue_5_95_c73)(n)

#' @export
#' @rdname kovesi
kovesi.linear_blue_95_50_c20 <- function(n) colorRampPalette(syspals$kovesi.linear_blue_95_50_c20)(n)

#' @export
#' @rdname kovesi
kovesi.linear_bmw_5_95_c86 <- function(n) colorRampPalette(syspals$kovesi.linear_bmw_5_95_c86)(n)

#' @export
#' @rdname kovesi
kovesi.linear_bmw_5_95_c89 <- function(n) colorRampPalette(syspals$kovesi.linear_bmw_5_95_c89)(n)

#' @export
#' @rdname kovesi
kovesi.linear_bmy_10_95_c71 <- function(n) colorRampPalette(syspals$kovesi.linear_bmy_10_95_c71)(n)

#' @export
#' @rdname kovesi
kovesi.linear_bmy_10_95_c78 <- function(n) colorRampPalette(syspals$kovesi.linear_bmy_10_95_c78)(n)

#' @export
#' @rdname kovesi
kovesi.linear_gow_60_85_c27 <- function(n) colorRampPalette(syspals$kovesi.linear_gow_60_85_c27)(n)

#' @export
#' @rdname kovesi
kovesi.linear_gow_65_90_c35 <- function(n) colorRampPalette(syspals$kovesi.linear_gow_65_90_c35)(n)

#' @export
#' @rdname kovesi
kovesi.linear_green_5_95_c69 <- function(n) colorRampPalette(syspals$kovesi.linear_green_5_95_c69)(n)

#' @export
#' @rdname kovesi
kovesi.linear_grey_0_100_c0 <- function(n) colorRampPalette(syspals$kovesi.linear_grey_0_100_c0)(n)

#' @export
#' @rdname kovesi
kovesi.linear_grey_10_95_c0 <- function(n) colorRampPalette(syspals$kovesi.linear_grey_10_95_c0)(n)

#' @export
#' @rdname kovesi
kovesi.linear_kry_5_95_c72 <- function(n) colorRampPalette(syspals$kovesi.linear_kry_5_95_c72)(n)

#' @export
#' @rdname kovesi
kovesi.linear_kry_5_98_c75 <- function(n) colorRampPalette(syspals$kovesi.linear_kry_5_98_c75)(n)

#' @export
#' @rdname kovesi
kovesi.linear_kryw_5_100_c64 <- function(n) colorRampPalette(syspals$kovesi.linear_kryw_5_100_c64)(n)

#' @export
#' @rdname kovesi
kovesi.linear_kryw_5_100_c67 <- function(n) colorRampPalette(syspals$kovesi.linear_kryw_5_100_c67)(n)

#' @export
#' @rdname kovesi
kovesi.linear_ternary_blue_0_44_c57 <- function(n) colorRampPalette(syspals$kovesi.linear_ternary_blue_0_44_c57)(n)

#' @export
#' @rdname kovesi
kovesi.linear_ternary_green_0_46_c42 <- function(n) colorRampPalette(syspals$kovesi.linear_ternary_green_0_46_c42)(n)

#' @export
#' @rdname kovesi
kovesi.linear_ternary_red_0_50_c52 <- function(n) colorRampPalette(syspals$kovesi.linear_ternary_red_0_50_c52)(n)

#' @export
#' @rdname kovesi
kovesi.rainbow_bgyr_35_85_c72 <- function(n) colorRampPalette(syspals$kovesi.rainbow_bgyr_35_85_c72)(n)

#' @export
#' @rdname kovesi
kovesi.rainbow <- function(n) colorRampPalette(syspals$kovesi.rainbow_bgyr_35_85_c72)(n)

#' @export
#' @rdname kovesi
kovesi.rainbow_bgyr_35_85_c73 <- function(n) colorRampPalette(syspals$kovesi.rainbow_bgyr_35_85_c73)(n)

#' @export
#' @rdname kovesi
kovesi.rainbow_bgyrm_35_85_c69 <- function(n) colorRampPalette(syspals$kovesi.rainbow_bgyrm_35_85_c69)(n)

#' @export
#' @rdname kovesi
kovesi.rainbow_bgyrm_35_85_c71 <- function(n) colorRampPalette(syspals$kovesi.rainbow_bgyrm_35_85_c71)(n)


