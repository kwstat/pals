#' Pals max colors
#' 
#' This function returns a data frame with the maximum number of colors for
#'  each palette currently available within the \pkg{pals} package.
#' @returns A data frame with the maximum number of colors for each palette.
#' @author R code by Brian M Schilder. 
#' @export
#' @importFrom utils getFromNamespace
#' @examples
#' dat <- pals.maxcolors()
pals.maxcolors <- function(){
  ## Copied function from rlang::is_missing
  is_missing <- function (x) {
    missing(x) || identical(x, quote(expr = ))
  }
  ## Get all exported pals functions
  ns <- getNamespaceExports("pals")
  syspals <- utils::getFromNamespace("syspals", "pals")
  ## Filter out the functions that don't actually exist
  syspals <- syspals[names(syspals) %in% ns]
  dat <- lapply(names(syspals), 
                function(p){
                  f <- formals(utils::getFromNamespace(p,"pals"))
                  if(!"n" %in% names(f)) return(NA)
                  maxcolors <- if(is_missing(f$n)) Inf else eval(f$n)
                  return(
                    data.frame(palette=p,
                               maxcolors=maxcolors, 
                               is_finite=is.finite(maxcolors),
                               stringsAsFactors=FALSE
                    )
                  )
                }) |> Reduce(f=rbind) 
  ## Sort by maxcolors
  dat <- dat[order(dat$maxcolors),]
  return(dat)
} 
