# pals 1.9 (2024-07-15)

* Better warning if `pal.cube()` is called without `rgl` package installed (#10) By Adam.

* New exported helper function: `pals.maxcolors()` (#5). By @bschilder


# pals 1.8 (2023-08-20)

* Switch to MIT license.

* Add `trubetskoy` discrete color palette.

* Fix docType issue reported by CRAN.


# pals 1.7 (2021-04-17)

* Added `turbo` palette.


# pals 1.6 (2019-12-04)

* Moved `rgl` from Imports to Suggests to reduce startup load (#4, Kent Johnson).

* Moved `maps` from Depends to Imports.

* New function `pal.heatmap2()`.

* Added value-suppressing uncertainty palettes.

* `brewer.pinkblue` is renamed `brewer.seqseq1`.

* `brewer.orangeblue` is renamed `brewer.seqseq2`.

* `covr` coverage exceeds 90 percent.


# pals 1.5 (2018-01-22)

* Added `tableau20` palette.

* Added `penobscot` data.

* Added `cividis` palette.

* Added `brewer.*` bivariate palettes.


# pals 1.4 (2017-06-12)

* Added 10 new discrete palettes for bivariate choropleth maps.


# pals 1.3 (2017-06-03)

* The `stepped()` palette gains 4 shades of gray for 24 total colors.

* Re-named `pal36()` palette to `polychrome()`.


# pals 1.0 (2016-12-14)

* First CRAN release.

* Include `maps` in Depends, otherwise there is an error about countyMapEnv.


# pals 0.1 - (2016-07-01)

* Began package.


# pals 0.0 - (2005-01-01)

* First palettes and evaluation tools created.
