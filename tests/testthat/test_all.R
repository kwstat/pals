# test_all.R

library(pals)
context("All tests")

cm5 <- c("#80FFFFFF", "#BFFFFFFF", "#FFFFFFFF", "#FFBFFFFF", "#FF80FFFF")
heat12 <- c("#FF0000FF", "#FF2000FF", "#FF4000FF", "#FF6000FF", "#FF8000FF", 
"#FF9F00FF", "#FFBF00FF", "#FFDF00FF", "#FFFF00FF", "#FFFF2AFF", 
"#FFFF80FF", "#FFFFD5FF")

test_that("palette compression",{
  expect_equal(pal.compress(cm.colors), cm5)

  expect_equal(pal.dist(cm.colors(5), cm5), c(0,0,0,0,0))
  
  expect_equal(pal.maxdist(cm.colors(5), cm5), 0)

  # heat.colors
  
  pal.bands(colorRampPalette(heat12), heat.colors)
  
  expect_equal(pal.maxdist(colorRampPalette(heat12), heat.colors), 20.21759809 )
})
               
