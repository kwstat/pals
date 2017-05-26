lib(pals)

# ----------------------------------------------------------------------------

pal.cube(alphabet)
lib(rgl)
snapshot3d("figure/cube_alphabet.png")

lib(png)
pp <- readPNG("figure/cube_alphabet.png")

op <- par(mfrow=c(2,2), mar=c(3,3,2,3))
pal.cluster(alphabet)
pal.heatmap(alphabet,n=26)
pal.scatter(alphabet)
#pal.cube
plot(NA, xlim=c(0,1), ylim=c(0,1))
rasterImage(pp, 0,0, 1,1)
mtext("alphabet palette", outer=TRUE, line=-1.5)
par(op)

savePlot("c:/x/rpack/pals/figure/test_palette.png", type="png")

# ----------------------------------------------------------------------------

pal.test(parula, main="parula")
savePlot("c:/x/rpack/pals/figure/test_colormap.png", type="png")
