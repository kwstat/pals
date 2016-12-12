lib(pals)

png("c:/x/catalog.png", width=800, height=2500, pointsize=24)

op = par(mar=c(1,10,2,1))

pal.bands(alphabet, alphabet2, cols25, glasbey, kelly, pal36, stepped, tol, watlington,
          
          coolwarm,cubehelix,gnuplot,jet,parula,tol.rainbow,
          
          cubicyf,cubicl,isol,linearl,linearlhot,
          
          brewer.accent(8), brewer.dark2(8), brewer.paired(12), brewer.pastel1(9),
          brewer.pastel2(8), brewer.set1(9), brewer.set2(8), brewer.set3(10),
          brewer.blues, brewer.bugn, brewer.bupu, brewer.gnbu, brewer.greens,
          brewer.greys, brewer.oranges, brewer.orrd, brewer.pubu, brewer.pubugn,
          brewer.purd, brewer.purples, brewer.rdpu, brewer.reds, brewer.ylgn,
          brewer.ylgnbu, brewer.ylorbr, brewer.ylorrd,
          brewer.brbg, brewer.piyg, brewer.prgn, brewer.puor, brewer.rdbu,
          brewer.rdgy, brewer.rdylbu, brewer.rdylgn, brewer.spectral,

          ocean.thermal, ocean.haline, ocean.solar, ocean.ice, ocean.gray,
          ocean.oxy, ocean.deep, ocean.dense, ocean.algae, ocean.matter,
          ocean.turbid, ocean.speed, ocean.amp, ocean.tempo, ocean.phase,
          ocean.balance, ocean.delta, ocean.curl,

          magma, inferno, plasma, viridis,
          
          kovesi.cyclic_grey_15_85_c0, kovesi.cyclic_grey_15_85_c0_s25,
          kovesi.cyclic_mrybm_35_75_c68, kovesi.cyclic_mrybm_35_75_c68_s25,
          kovesi.cyclic_mygbm_30_95_c78, kovesi.cyclic_mygbm_30_95_c78_s25,
          kovesi.cyclic_wrwbw_40_90_c42, kovesi.cyclic_wrwbw_40_90_c42_s25,
          kovesi.diverging_isoluminant_cjm_75_c23, kovesi.diverging_isoluminant_cjm_75_c24,
          kovesi.diverging_isoluminant_cjo_70_c25, kovesi.diverging_linear_bjr_30_55_c53,
          kovesi.diverging_linear_bjy_30_90_c45, kovesi.diverging_rainbow_bgymr_45_85_c67,
          kovesi.diverging_bkr_55_10_c35, kovesi.diverging_bky_60_10_c30,
          kovesi.diverging_bwr_40_95_c42, kovesi.diverging_bwr_55_98_c37,
          kovesi.diverging_cwm_80_100_c22, kovesi.diverging_gkr_60_10_c40,
          kovesi.diverging_gwr_55_95_c38, kovesi.diverging_gwv_55_95_c39,
          kovesi.isoluminant_cgo_70_c39, kovesi.isoluminant_cgo_80_c38,
          kovesi.isoluminant_cm_70_c39, kovesi.rainbow_bgyr_35_85_c72,
          kovesi.rainbow_bgyr_35_85_c73,
          kovesi.rainbow_bgyrm_35_85_c69, kovesi.rainbow_bgyrm_35_85_c71,

          kovesi.linear_bgy_10_95_c74,
          kovesi.linear_bgyw_15_100_c67, kovesi.linear_bgyw_15_100_c68,
          kovesi.linear_blue_5_95_c73, kovesi.linear_blue_95_50_c20,
          kovesi.linear_bmw_5_95_c86, kovesi.linear_bmw_5_95_c89,
          kovesi.linear_bmy_10_95_c71, kovesi.linear_bmy_10_95_c78,
          kovesi.linear_gow_60_85_c27, kovesi.linear_gow_65_90_c35,
          kovesi.linear_green_5_95_c69, kovesi.linear_grey_0_100_c0,
          kovesi.linear_grey_10_95_c0, kovesi.linear_kry_5_95_c72,
          kovesi.linear_kry_5_98_c75, kovesi.linear_kryw_5_100_c64,
          kovesi.linear_kryw_5_100_c67, kovesi.linear_ternary_blue_0_44_c57,
          kovesi.linear_ternary_green_0_46_c42, kovesi.linear_ternary_red_0_50_c52,
          main="Kovesi linear", show.names=FALSE # ,
          #labels=c("brewer.accent", "brewer.dark2", "brewer.paired", "brewer.pastel1",
          #         "brewer.pastel2", "brewer.set1", "brewer.set2", "brewer.set3",
)
par(op)
dev.off()

# click("c:/x/catalog.png")

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
