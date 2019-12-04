# test_all.R

# In R 3.6.x, cm.colors() includes "FF" for the alpha channel.
# Next version of R does NOT include FF.  
# Remove this test during the transition.

#library(pals)
#
#cm5 <- c("#80FFFF", "#BFFFFF", "#FFFFFF", "#FFBFFF", "#FF80FF")
#heat12 <- c("#FF0000", "#FF2000", "#FF4000", "#FF6000", "#FF8000", 
#"#FF9F00", "#FFBF00", "#FFDF00", "#FFFF00", "#FFFF2A", 
#"#FFFF80", "#FFFFD5")
#
#test_that("palette compression",{
#  expect_equal(pal.compress(cm.colors), cm5)
#
#  expect_equal(pal.dist(cm.colors(5), cm5), c(0,0,0,0,0))
#  
#  expect_equal(pal.maxdist(cm.colors(5), cm5), 0)
#
#  # heat.colors
#  
#  pal.bands(colorRampPalette(heat12), heat.colors)
#  
#  expect_equal(pal.maxdist(colorRampPalette(heat12), heat.colors), 20.21759809 )
#})
               
