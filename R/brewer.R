# brewer.R
# Time-stamp: <17 Nov 2016 16:22:04 c:/x/rpack/pals/R/brewer.R>
# Copyright: Kevin Wright, 2016. License: GPL-3.

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
#' # Diverging
#' pal.bands(brewer.brbg, brewer.piyg, brewer.prgn, brewer.puor, brewer.rdbu,
#'           brewer.rdgy, brewer.rdylbu, brewer.rdylgn, brewer.spectral)
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

get.brewer.pal = function(bpal, n){
  # bpal is a brewer palette list, list item k-2 is a vector of k color
  rng = as.numeric(names(bpal))
  maxn = max(rng)
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
