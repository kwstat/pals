# brewer.R
# Time-stamp: <30 Sep 2016 14:19:06 c:/x/rpack/pals/R/brewer.r>

#' ColorBrewer palettes
#'
#' These functions provide a unified interface to the ColorBrewer palettes.
#'
#' The palette names begin with 'brewer' to make it easier to use auto-completion.
#' For consistency with the rest of this package, the palette names use lower-case letters.
#' 
#' @import RColorBrewer
#' @examples
#'
#' # Qualitative
#' 
#' pal.test(brewer_blues)
#' pal.test(brewer_bugn)
#' pal.test(brewer_bupu)
#' pal.test(brewer_gnbu)
#' pal.test(brewer_greens)
#' pal.test(brewer_greys)
#' pal.test(brewer_oranges)
#' pal.test(brewer_orrd)
#' pal.test(brewer_pubu)
#' pal.test(brewer_pubugn)
#' pal.test(brewer_purd)
#' pal.test(brewer_purples)
#' pal.test(brewer_rdpu)
#' pal.test(brewer_reds)
#' pal.test(brewer_ylgn)
#' pal.test(brewer_ylgnbu)
#' pal.test(brewer_ylorbr)
#' pal.test(brewer_ylorrd)
#'
#' # Diverging, max 11
#' 
#' pal.test(brewer_brbg)
#' pal.test(brewer_piyg)
#' pal.test(brewer_prgn)
#' pal.test(brewer_puor)
#' pal.test(brewer_rdbu)
#' pal.test(brewer_rdgy)
#' pal.test(brewer_rdylbl)
#' pal.test(brewer_rdylgn)
#' pal.test(brewer_spectral)
#'
#' # Qualtitative
#' # These are weird...don't do this
#' 
#' pal.test(brewer_accent)
#' pal.test(brewer_dark2)
#' pal.test(brewer_paired)
#' pal.test(brewer_pastel1)
#' pal.test(brewer_pastel2)
#' pal.test(brewer_set1)
#' pal.test(brewer_set2)
#' pal.test(brewer_set3)
#'
#' pal.bands(c('brewer_accent','brewer_dark2','brewer_paired','brewer_pastel1','brewer_pastel2','brewer_set1','brewer_set2','brewer_set3'))
#'
#' @name brewer
NULL

# ----------------------------------------------------------------------------
# quantitative

#' @export
#' @rdname brewer
brewer_blues <- colorRampPalette(brewer.pal(9,"Blues"))

#' @export
#' @rdname brewer
brewer_bugn <- colorRampPalette(brewer.pal(9,"BuGn"))

#' @export
#' @rdname brewer
brewer_bupu <- colorRampPalette(brewer.pal(9,"BuPu"))

#' @export
#' @rdname brewer
brewer_gnbu <- colorRampPalette(brewer.pal(9,"GnBu"))

#' @export
#' @rdname brewer
brewer_greens <- colorRampPalette(brewer.pal(9,"Greens"))

#' @export
#' @rdname brewer
brewer_greys <- colorRampPalette(brewer.pal(9,"Greys"))

#' @export
#' @rdname brewer
brewer_oranges <- colorRampPalette(brewer.pal(9,"Oranges"))

#' @export
#' @rdname brewer
brewer_orrd <- colorRampPalette(brewer.pal(9,"OrRd"))

#' @export
#' @rdname brewer
brewer_pubu <- colorRampPalette(brewer.pal(9,"PuBu"))

#' @export
#' @rdname brewer
brewer_pubugn <- colorRampPalette(brewer.pal(9,"PuBuGn"))

#' @export
#' @rdname brewer
brewer_purd <- colorRampPalette(brewer.pal(9,"PuRd"))

#' @export
#' @rdname brewer
brewer_purples <- colorRampPalette(brewer.pal(9,"Purples"))

#' @export
#' @rdname brewer
brewer_rdpu <- colorRampPalette(brewer.pal(9,"RdPu"))

#' @export
#' @rdname brewer
brewer_reds <- colorRampPalette(brewer.pal(9,"Reds"))

#' @export
#' @rdname brewer
brewer_ylgn <- colorRampPalette(brewer.pal(9,"YlGn"))

#' @export
#' @rdname brewer
brewer_ylgnbu <- colorRampPalette(brewer.pal(9,"YlGnBu"))

#' @export
#' @rdname brewer
brewer_ylorbr <- colorRampPalette(brewer.pal(9,"YlOrBr"))

#' @export
#' @rdname brewer
brewer_ylorrd <- colorRampPalette(brewer.pal(9,"YlOrRd"))

# ----------------------------------------------------------------------------
# diverging

#' @export
#' @rdname brewer
brewer_brbg <- colorRampPalette(brewer.pal(11,"BrBG"))

#' @export
#' @rdname brewer
brewer_piyg <- colorRampPalette(brewer.pal(11,"PiYG"))

#' @export
#' @rdname brewer
brewer_prgn <- colorRampPalette(brewer.pal(11,"PRGn"))

#' @export
#' @rdname brewer
brewer_puor <- colorRampPalette(brewer.pal(11,"PuOr"))

#' @export
#' @rdname brewer
brewer_rdbu <- colorRampPalette(brewer.pal(11,"RdBu"))

#' @export
#' @rdname brewer
brewer_rdgy <- colorRampPalette(brewer.pal(11,"RdGy"))

#' @export
#' @rdname brewer
brewer_rdylbl <- colorRampPalette(brewer.pal(11,"RdYlBu"))

#' @export
#' @rdname brewer
brewer_rdylgn <- colorRampPalette(brewer.pal(11,"RdYlGn"))

#' @export
#' @rdname brewer
brewer_spectral <- colorRampPalette(brewer.pal(11,"Spectral"))

# ----------------------------------------------------------------------------
# qualitative

#' @export
#' @rdname brewer
brewer_accent <- colorRampPalette(brewer.pal(8,"Accent"))

#' @export
#' @rdname brewer
brewer_dark2 <- colorRampPalette(brewer.pal(8,"Dark2"))

#' @export
#' @rdname brewer
brewer_paired <- colorRampPalette(brewer.pal(12,"Paired"))

#' @export
#' @rdname brewer
brewer_pastel1 <- colorRampPalette(brewer.pal(9,"Pastel1"))

#' @export
#' @rdname brewer
brewer_pastel2 <- colorRampPalette(brewer.pal(8,"Pastel2"))

#' @export
#' @rdname brewer
brewer_set1 <- colorRampPalette(brewer.pal(9,"Set1"))

#' @export
#' @rdname brewer
brewer_set2 <- colorRampPalette(brewer.pal(8,"Set2"))

#' @export
#' @rdname brewer
brewer_set3 <- colorRampPalette(brewer.pal(12,"Set3"))
