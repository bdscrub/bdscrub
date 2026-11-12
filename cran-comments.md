## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.
* On some platforms R CMD check may give a NOTE about marked UTF-8 strings
  in the data. The data sets contain the official Bangla (Bengali-script)
  names of the districts and divisions of Bangladesh, stored as UTF-8; they
  are an intended part of the data.

## Test environments

* local Windows 11, R 4.6.1 (R CMD check --as-cran)
<!-- add the results of win-builder (devel and release), the macOS builder
     and R-hub here before submitting -->

## Data sources

The package includes reference data compiled from public sources (Bangladesh
Post office list, HDX/BBS administrative boundary names, ISO 3166-2:BD codes).
Sources and reuse terms are documented in inst/COPYRIGHTS and in the help
pages of the data sets.
