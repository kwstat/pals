library(ggplot2)
library(viridis)

ggplot(data.frame(x = rnorm(10000), y = rnorm(10000)), aes(x = x, y = y)) +
  geom_hex() + coord_fixed() +
  scale_fill_gradientn(colors=ocean.haline(11)) + theme_classic()

ggplot(data.frame(x = rnorm(10000), y = rnorm(10000)), aes(x = x, y = y)) +
  geom_hex() + coord_fixed() +
  scale_fill_gradientn(colors=rev(brewer.greys(11))) + theme_classic()

ggplot(data.frame(x = rnorm(10000), y = rnorm(10000)), aes(x = x, y = y)) +
  geom_hex() + coord_fixed() +
  scale_fill_viridis() + theme_classic()

set.seed(42); pal.heatmap(parula, n=15, miss=0)
set.seed(42); pal.heatmap(viridis, n=15, miss=0)
set.seed(42); pal.heatmap(ocean.haline, n=15, miss=0)

op <- par(mfcol=c(2,3), mar=rep(0,4))
pal.volcano(viridis(99)); mtext("viridis", line=-2)
pal.volcano(rev(viridis(99)))
pal.volcano(ocean.haline(99)) ; mtext("ocean.haline", line=-2)
pal.volcano(rev(ocean.haline(99)))
pal.volcano(parula(99)) ; mtext("parula", line=-2)
pal.volcano(rev(parula(99)))
par(op)
