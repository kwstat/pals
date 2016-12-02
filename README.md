# pals

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/pals)](https://cran.r-project.org/package=pals)
[![CRAN_Downloads](https://cranlogs.r-pkg.org/badges/pals)](https://cranlogs.r-pkg.org/badges/pals)

Color Palettes, Colormaps, and Tools to Evaluate Them

Key features:

* Beta version. Mostly complete. Needs a vignette before release.

* Extensive collection of colormaps and palettes.

* Multiple tools to evaluate colormaps.

## Installation

```R
# Install the released version from CRAN:
install.packages("pals")

# Install the cutting edge development version from GitHub:
# install.packages("devtools")
devtools::install_github("kwstat/pals")
```

## Usage

Vignettes:

[Examples for the pals package](https://rawgit.com/kwstat/pals/master/vignettes/pals_examples.html)

[A Color-Caused Optical Illusion](https://rawgit.com/kwstat/pals/master/vignettes/illusion.html)

```R
require(pals)
pal.test(parula)
```
![pals](figure/parula.png)
