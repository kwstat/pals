# pals 1.9

* License clarification.


# pals 1.8

* Switch to MIT license.
* Fix docType issue reported by CRAN.

## Test environments and results

* local R 4.3.1 on Windows 10
* Winbuilder R-devel
* Rhub

OK (allowing for usual quirky reporting from Rhub)

## revdepcheck results

We checked 9 reverse dependencies (6 from CRAN + 3 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
 
 
# pals 1.7

## Test environments

* local R 4.0.5 on Windows 10
* Winbuilder R-devel
* Rhub Windows Server 2008 R2 SP1, R-devel, 32/64 bit

## Rcmd check results

OK

 
 

# pals 1.6

Fix to accomodate change in base R: cm.colors(n) etc no longer append the code for 'alpha = 1', "FF".

## Test environments

* local R 3.6.1 on Windows 10
* R-hub Windows Server 2008
* winbuilder R-devel

## Rcmd check results

No warnings or errors.


# pals 1.5

## Test environments

* local R 3.4.3 on Windows 7
* win-builder R-release 3.4.3
* win-builder R-devel

## Rcmd check results

There is 1 NOTE

Possibly mis-spelled words in DESCRIPTION:
  Colormaps (5:24)
  colormaps (6:60)


# pals 1.4

## Test environments

* local R 3.4.0 on Windows 7
* win-builder r-devel
* win-builder on R 3.4.0

## Rcmd check results

There is 1 NOTE

Possibly mis-spelled words in DESCRIPTION:
  Colormaps (5:24)
  colormaps (6:60)


# pals 1.0

## Test environments

* local: R 3.3.2 on Windows 7
* win-builder: R-release 3.3.2
* win-builder: R-devel

## Rcmd check results

No warnings or errors.

3 Notes:

1.

New release

2.

Possibly mis-spelled words in DESCRIPTION:
  Colormaps (5:24)
  colormaps (6:60)

3. (This happens because many examples are grouped into a single file instead of split across multiple files)

Examples with CPU or elapsed time > 10s
            user system elapsed
            continuous 10.95   0.42   11.64

