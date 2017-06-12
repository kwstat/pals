# bivariate.R
# Time-stamp: <12 Jun 2017 08:51:14 c:/x/rpack/pals/R/bivariate.R>
# Copyright Kevin Wright, 2017, GPL-3

#' Bivariate palettes
#'
#' Color palettes designed for bivariate choropleth maps.
#'
#' In each palette name, the color in the upper left corner is given first, and the color
#' in the lower right corner is given second.
#' 
#' The `arc.bluepink` palette uses white in the lower-left corner, which makes it
#' difficult to see the difference between low values and missing data on maps.
#' 
#' The `census.blueyellow` palette is slightly different, in that one direction
#' uses lightness, and the other direction uses hue (yellow, green, blue).
#' 
#' @param n Number of colors to return.
#' 
#' @return A vector of colors as hex strings.
#' 
#' @author Palette colors by various authors. R code by Kevin Wright.
#' 
#' @examples
#'
#' bivcol <- function(pal){
#'   tit <- substitute(pal)
#'   pal <- pal()
#'   ncol <- length(pal)
#'   nx <- sqrt(length(pal))
#'   image(matrix(1:ncol, nrow=sqrt(ncol)), axes=FALSE, col=pal)
#'   mtext(tit)
#' }
#' op <- par(mfrow=c(3,4), mar=c(1,1,2,1))
#' bivcol(stevens.pinkgreen)
#' bivcol(stevens.bluered)
#' bivcol(stevens.pinkblue)
#' bivcol(stevens.greenblue)
#' bivcol(stevens.purplegold)
#' bivcol(brewer.orangeblue)
#' bivcol(brewer.pinkblue)
#' bivcol(tolochko.redblue)
#' bivcol(arc.bluepink)
#' bivcol(census.blueyellow)
#' par(op)
#' 
#' @references
#'
#' Joshua Stevens.
#' http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
#'
#' Cindy Brewer.
#' http://www.personal.psu.edu/cab38/ColorSch/SchHTMLs/CBColorSeqSeq.html
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
brewer.orangeblue <- function(n=9) {
  if(n >  9) {
    message("Only 9 colors are available with 'brewer.orangeblue'")
    n <- 9
  }

  pal <- c("#f3f3f3", "#b4d3e1", "#509dc2", "#f3e6b3", "#b3b3b3", "#376387", "#f3b300", "#b36600", "#000000")
  return(pal[1:n])
}

#' @export
#' @rdname bivariate
brewer.pinkblue <- function(n=9) {
  if(n >  9) {
    message("Only 9 colors are available with 'brewer.pinkblue'")
    n <- 9
  }

  pal <- c("#e8e6f2", "#b5d3e7", "#4fadd0", "#e5b4d9", "#b8b3d8", "#3983bb", "#de4fa6", "#b03598", "#2a1a8a")
  return(pal[1:n])
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
census.blueyellow <- function(n=9){
  if(n >  9) {
    message("Only 9 colors are available with 'census.blueyellow'")
    n <- 9
  }

  pal <- c("#fffdef","#fef3a9","#efd100","#e6f1df","#bedebc","#4eb87b","#d2e4f6","#a1c8ea","#007fc4")
  return(pal[1:9])
}






