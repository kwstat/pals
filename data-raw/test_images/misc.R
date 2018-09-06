# misc.R
# Time-stamp: <04 Jun 2018 09:36:57 c:/x/rpack/pals/data-raw/test_images/misc.R>

library(pals)

# bivariate normal
lib(mvtnorm)
x0 = seq(from=-3, to=3, length=100)
y0 = seq(from=-2, to=2, length=100)
xy = expand.grid(x0,y0)
z0 = matrix(dmvnorm(xy, mean=c(1,0.5))^2 - 0.4*dmvnorm(xy, mean=c(-1,-0.5))^2,
            100,100) / 0.03
filled.contour(x=x0, y=y0, z=z0, color.palette=ocean.delta, nlevels=99)
filled.contour(x=x0, y=y0, z=z0, color.palette=cubehelix, nlevels=99)
filled.contour(x=x0, y=y0, z=z0, color.palette=parula, nlevels=99)
filled.contour(x=x0, y=y0, z=z0, color.palette=viridis, nlevels=99)

# oblique stripes
x1 = seq(from = -pi, to =  pi, length=100)
y1 = seq(from = -pi, to =  pi, length=100)
xy1 = expand.grid(x1,y1)
z1 = matrix( sin(xy1$Var1 + xy1$Var2/4), 100, 100)
filled.contour(x=x1, y=y1, z=z1, color.palette=ocean.delta, nlevels=99)
filled.contour(x=x1, y=y1, z=z1, color.palette=cubehelix, nlevels=99)
filled.contour(x=x1, y=y1, z=z1, color.palette=parula, nlevels=99)
filled.contour(x=x1, y=y1, z=z1, color.palette=viridis, nlevels=99)

