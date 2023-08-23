# bivariate.R
# Time-stamp: <30 Dec 2021 12:31:09 c:/one/rpack/pals/r/bivariate.R>
# Copyright Kevin Wright, 2017, GPL-3

#' Bivariate palettes
#'
#' Color palettes designed for bivariate choropleth maps.
#'
#' In many of these palette names, the color in the upper left corner is given first, and the color
#' in the lower right corner is given second.
#'
#' The `brewer.*` palettes use `bin` (binary), `div` (diverging), `qual` (qualitative), `seq` (sequential) for the horizontal and vertical directions.
#' 
#' The `arc.bluepink` palette uses white in the lower-left corner, which makes it
#' difficult to see the difference between low values and missing data on maps.
#' 
#' The `census.blueyellow` palette is slightly different, in that one direction
#' uses lightness, and the other direction uses hue (yellow, green, blue).
#'
#' The `vsup.*` palettes are Value-Suppressing Uncertainty Palettes.
#' 
#' We strongly discourage using `vsup.viridis`, because the horizontal axis has
#' changes in brightness, which are confounded with the changes in brightness
#' in the vertical axis.
#' 
#' These palettes are all deliberately chosen to be discrete.
#' 
#' Bivariate color palettes can be difficult to use and interpret. Please be careful.
#' 
#' @param n Number of colors to return.
#' 
#' @return A vector of colors as hex strings.
#' 
#' @author Palette colors by various authors. R code by Kevin Wright.
#' 
#' @examples
#'
#' bivcol <- function(pal, nx=3, ny=3){
#'   tit <- substitute(pal)
#'   if(is.function(pal)) pal <- pal()
#'   ncol <- length(pal)
#'   if(missing(nx)) nx <- sqrt(ncol)
#'   if(missing(ny)) ny <- nx
#'   image(matrix(1:ncol, nrow=ny), axes=FALSE, col=pal)
#'   mtext(tit)
#' }
#' op <- par(mfrow=c(4,4), mar=c(1,1,2,1))
#' bivcol(arc.bluepink)
#' bivcol(brewer.divbin, nx=3)
#' bivcol(brewer.divdiv)
#' bivcol(brewer.divseq)
#' bivcol(brewer.qualbin, nx=3)
#' bivcol(brewer.qualseq)
#' bivcol(brewer.seqseq1)
#' bivcol(brewer.seqseq2)
#' bivcol(census.blueyellow)
#' bivcol(stevens.bluered)
#' bivcol(stevens.greenblue)
#' bivcol(stevens.pinkblue)
#' bivcol(stevens.pinkgreen)
#' bivcol(stevens.purplegold)
#' bivcol(tolochko.redblue)
#' bivcol(vsup.redblue, nx=8)
#' par(op)
#'
#' 
#' @references
#'
#' Joshua Stevens.
#' http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
#'
#' Cindy Brewer.
#' http://www.personal.psu.edu/cab38/ColorSch/Schemes.html
#'
#' Michael Correll AND Dominik Moritz AND Jeffrey Heer. (2018).
#' Value-Suppressing Uncertainty Palettes.
#' https://github.com/uwdata/papers-vsup
#' 
#' Robin Tolochko.
#' http://tolomaps.tumblr.com/post/131671267233/creating-a-bivariate-choropleth-color-scheme
#'
#' Aileen Buckley.
#' https://www.slideshare.net/aileenbuckley/arc-gis-bivariate-mapping-tools-28903069
#'
#' https://www.census.gov/population/www/cen2000/atlas/
#' Total Population, p. 4.
#' 
#' @name bivariate
NULL


#' @export
#' @rdname bivariate
arc.bluepink <- function(n=9) {
  if(n >  16) {
    message("Only 16 colors are available with 'arc.bluepink'")
    n <- 16
  }

  pal <- c("#ffffff","#ffe6fe","#ffbdff","#ff80fe",
                      "#e7ffff","#d7dafd","#d8a6ff","#c065fe",
                      "#c0fcfd","#a7caff","#8d7efd","#7f65fe",
                      "#74feff","#64c0ff","#5873fe","#4b4cff")
  return(pal[1:16])
}

#' @export
#' @rdname bivariate
brewer.qualbin <- function(n=6) {
  if(n >  6) {
    message("Only 6 colors are available with 'brewer.qualbin'")
    n <- 6
  }

  pal <- c("#cce8d7","#c2d8ea","#fbb4d9","#80b696","#84a0c0","#de68a6")
  return(pal[1:n])
}


#' @export
#' @rdname bivariate
brewer.divbin <- function(n=6) {
  if(n >  6) {
    message("Only 6 colors are available with 'brewer.divbin'")
    n <- 6
  }

  pal <- c("#b7c3e0","#e6e6e6","#ffc080","#004494","#a6a6a6","#cc4c00")
  return(pal[1:n])
}


#' @export
#' @rdname bivariate
brewer.divseq <- function(n=9) {
  if(n >  9) {
    message("Only 9 colors are available with 'brewer.divseq'")
    n <- 9
  }

  pal <- c("#c3b3d8","#e6e6e6","#ffcc80","#7b67ab","#bfbfbf","#f35926","#240d5e","#7f7f7f","#b30000")
  return(pal[1:n])
}


#' @export
#' @rdname bivariate
brewer.qualseq <- function(n=9) {
  if(n >  9) {
    message("Only 9 colors are available with 'brewer.qualseq'")
    n <- 9
  }

  pal <- c("#cce8d7","#cedced","#fbb4d9","#80c39b","#85a8d0","#f668b3","#008837","#0a50a1","#d60066")
  return(pal[1:n])
}


#' @export
#' @rdname bivariate
brewer.divdiv <- function(n=9) {
  if(n >  9) {
    message("Only 9 colors are available with 'brewer.divdiv'")
    n <- 9
  }

  pal <- c("#f37300","#cce88b","#008837","#fe9aa6","#e6e6e6","#9ac9d5","#f0047f","#cd9acc","#5a4da4")
  return(pal[1:n])
}


#' @export
#' @rdname bivariate
brewer.seqseq1 <- function(n=9) {
  if(n >  9) {
    message("Only 9 colors are available with 'brewer.seqseq1'")
    n <- 9
  }

  pal <- c("#e8e6f2", "#b5d3e7", "#4fadd0", "#e5b4d9", "#b8b3d8", "#3983bb", "#de4fa6", "#b03598", "#2a1a8a")
  return(pal[1:n])
}

#' @export
#' @rdname bivariate
brewer.seqseq2 <- function(n=9) {
  if(n >  9) {
    message("Only 9 colors are available with 'brewer.seqseq2'")
    n <- 9
  }

  pal <- c("#f3f3f3", "#b4d3e1", "#509dc2", "#f3e6b3", "#b3b3b3", "#376387", "#f3b300", "#b36600", "#000000")
  return(pal[1:n])
}

#' @export
#' @rdname bivariate
census.blueyellow <- function(n=9){
  if(n >  9) {
    message("Only 9 colors are available with 'census.blueyellow'")
    n <- 9
  }

  pal <- c("#fffdef","#fef3a9","#efd100","#e6f1df","#bedebc","#4eb87b","#d2e4f6","#a1c8ea","#007fc4")
  return(pal[1:9])
}

#' @export
#' @rdname bivariate
tolochko.redblue <- function(n=9) {
  if(n >  9) {
    message("Only 9 colors are available with 'tolochko.redblue'")
    n <- 9
  }

  pal <- c("#dddddd", "#7bb3d1", "#016eae", "#dd7c8a", "#8d6c8f", "#4a4779", "#cc0024", "#8a274a", "#4b264d")
  return(pal[1:n])
}

#' @export
#' @rdname bivariate
stevens.pinkgreen <- function(n=9) {
  if(n >  9) {
    message("Only 9 colors are available with 'stevens.pinkgreen'")
    n <- 9
  }

  pal <- c("#f3f3f3", "#c2f1ce", "#8be2af", "#eac5dd", "#9ec6d3", "#7fc6b1", "#e6a3d0", "#bc9fce", "#7b8eaf")
  return(pal[1:n])
}

#' @export
#' @rdname bivariate
stevens.bluered <- function(n=9) {
  if(n >  9) {
    message("Only 9 colors are available with 'stevens.bluered'")
    n <- 9
  }

  pal <- c("#e8e8e8", "#e4acac", "#c85a5a", "#b0d5df", "#ad9ea5", "#985356", "#64acbe", "#627f8c", "#574249")
  return(pal[1:n])
}

#' @export
#' @rdname bivariate
stevens.pinkblue <- function(n=9) {
  if(n >  9) {
    message("Only 9 colors are available with 'stevens.pinkblue'")
    n <- 9
  }

  pal <- c("#e8e8e8", "#ace4e4", "#5ac8c8", "#dfb0d6", "#a5add3", "#5698b9", "#be64ac", "#8c62aa", "#3b4994")
  return(pal[1:n])
}

#' @export
#' @rdname bivariate
stevens.greenblue <- function(n=9) {
  if(n >  9) {
    message("Only 9 colors are available with 'stevens.greenblue'")
    n <- 9
  }

  pal <- c("#e8e8e8", "#b5c0da", "#6c83b5", "#b8d6be", "#90b2b3", "#567994", "#73ae80", "#5a9178", "#2a5a5b")
  return(pal[1:n])
}

#' @export
#' @rdname bivariate
stevens.purplegold <- function(n=9) {
  if(n >  9) {
    message("Only 9 colors are available with 'stevens.purplegold'")
    n <- 9
  }

  pal <- c("#e8e8e8", "#e4d9ac", "#c8b35a", "#cbb8d7", "#c8ada0", "#af8e53", "#9972af", "#976b82", "#804d36")
  return(pal[1:n])
}

#' @export
#' @rdname bivariate
vsup.viridis <- function(n=32) {
  if(n >  32) {
    message("Only 32 colors are available with 'vsup.viridis'")
    n <- 32
  }

  pal <- c("#48186A", "#424085", "#33628D", "#26828D", "#1FA087", "#3EBA72", "#83D44B", "#D8E318",
           "#755D9B", "#755D9B", "#6792A9", "#6792A9", "#6CC49E", "#6CC49E", "#C5E56D", "#C5E56D", 
           "#9DA4C4", "#9DA4C4", "#9DA4C4", "#9DA4C4", "#B4E5B0", "#B4E5B0", "#B4E5B0", "#B4E5B0",
           "#CDE3E1", "#CDE3E1", "#CDE3E1", "#CDE3E1", "#CDE3E1", "#CDE3E1", "#CDE3E1", "#CDE3E1")
  return(pal[1:n])
}

#' @export
#' @rdname bivariate
vsup.redblue <- function(n=32) {
  if(n >  32) {
    message("Only 32 colors are available with 'vsup.redblue'")
    n <- 32
  }

  pal <- c("#8F223C", "#841A4E", "#790C60", "#6B0573", "#602381", "#5A3C8A","#524D92", "#475D9C",
           "#A85B6D", "#A85B6D", "#94548C", "#94548C", "#8360A2", "#8360A2", "#787BB0","#787BB0", 
           "#BE8EA6", "#BE8EA6", "#BE8EA6", "#BE8EA6", "#A79CC5", "#A79CC5", "#A79CC5", "#A79CC5",
           "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC")

  return(pal[1:n])
}

