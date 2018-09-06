
context("test_tools.R")

test_that("sort", {
  pal.bands(alphabet, alphabet2) # default none
  pal.bands(alphabet, alphabet2, sort="none")
  pal.bands(alphabet, alphabet2, sort="hue")
  pal.bands(alphabet, alphabet2, sort="luminance")
  expect_error( pal.bands(alphabet, alphabet2, sort="rgb") )
})


## # pal.bands

## # n, main, gap
## pal.bands(coolwarm, warmcool, n=11, gap=0.2, main="coolwarm/warmcool")

## # labels

## # show.names
## pal.bands(alphabet, alphabet2, glasbey, kelly, watlington, show.names=FALSE)
## pal.bands(alphabet, alphabet2, glasbey, kelly, watlington, show.names=TRUE)


## labs=c('alphabet','alphabet2', 'glasbey','kelly','polychrome', 'watlington')
## op=par(mar=c(0,5,3,1))
## pal.bands(alphabet(), alphabet2(), glasbey(), kelly(),
##   polychrome(), watlington(), sort="hue",
##   labels=labs, main="sorted by hue")
## par(op)
## pal.bands(alphabet(), alphabet2(), glasbey(), kelly(),
##   polychrome(), watlington(), sort="luminance",
##   labels=labs, main="sorted by luminance")
 
## # ----------------------------------------------------------------------------

test_that("tools", {

  # pal.channels
  pal.channels(parula, main="parula")
  pal.channels(parula(20))

  # pal.cluster
  pal.cluster(alphabet(10))
  pal.cluster(alphabet, n=10, type="LUV", main="alphabet LUV")
  pal.cluster(alphabet, n=10, type="RGB", main="alphabet RGB")

  #pal.compress(parula, n=10, thresh=2)

  # pal.csf
  pal.csf(parula, n=10, main="parula")
  pal.csf(parula(10))

  #pal.cube(parula,n=40, type="RGB")
  ## pal.cube(parula,n=40,label=TRUE, type="LUV")

  # pal.heatmap
  pal.heatmap(alphabet(10))
  pal.heatmap(alphabet, main="alphabet")
  expect_error(pal.heatmap(alphabet(10), miss=10))

  # pal.safe
  pal.safe(parula(10))
  pal.safe(parula, main="parula")

  # pal.scatter
  pal.scatter(alphabet, main="alphabet")
  pal.scatter(alphabet(10))

  # pal.sineramp
  pal.sineramp(parula(10))
  pal.sineramp(parula, main="parula")
  
  # pal.test
  pal.test(parula(10))
  pal.test(parula, main="parula")
  
  # pal.volcano
  pal.volcano(parula(10))
  pal.volcano(parula, main="parula")
  
  pal.zcurve(coolwarm, n=256)
  expect_error( pal.zcurve(coolwarm, n=5) )
  
})

## pa0 <- c("#ff0000","#00ff00","#0000ff")
## pa1 <- c("#fa0000","#00fa00","#0000fa") # 2.4
## pa2 <- c("#f40000","#00f400","#0000f4") # 5.2
## pal.dist(pa0,pa1) # 1.87, 2.36, 2.11
## pal.maxdist(pa0,pa1) # 2.36
