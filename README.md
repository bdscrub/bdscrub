# bdscrub

<!-- badges: start -->
<!-- After the first upload, replace bdscrub below with the name of your GitHub organisation. -->
[![R-CMD-check](https://github.com/bdscrub/bdscrub/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bdscrub/bdscrub/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE.md)
<!-- badges: end -->

**bdscrub** cleans the fields that make Bangladeshi survey and administrative
data messy: mobile numbers, money amounts written with lakh and crore, Bangla
digits, missing-value markers, and district, division and thana names,
including whole free-text addresses.

It works offline, needs only base R, and never guesses silently: a value it
cannot read becomes `NA` with a warning, and every name it changes is listed
in a message.

## Installation

bdscrub is not on CRAN yet. Install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("bdscrub/bdscrub")
```

No package can be installed on your computer? Download
[`extras/bdscrub_standalone.R`](extras/bdscrub_standalone.R) and run
`source("bdscrub_standalone.R")`: it holds the whole package in one file.

## The eleven functions

| Function | What it does |
|---|---|
| `bd_phone()` | mobile numbers in one format; several numbers per cell; operator |
| `bd_amount()` | money amounts ("Tk 1,25,000", "1.5 lakh", Bangla); ranges with `details = TRUE` |
| `bd_number()` | plain numbers with Bangla digits and lakh grouping |
| `bd_taka()` | write amounts with lakh and crore grouping |
| `bd_digits()` | Bangla digits to English digits and back |
| `bd_place()` | district, division and thana (upazila) names; codes; map spellings |
| `bd_address()` | post code, thana, district and division from an address |
| `bd_missing()` | "N/A", "-", "#N/A" and similar to `NA` |
| `bd_squish()` | extra and invisible spaces |
| `bd_check()` | how many values of each column can be read, and why not |
| `bd_clean()` | clean many columns of a data frame at once |

Data sets: `bd_districts`, `bd_divisions`, `bd_thanas`, `bd_postcodes`,
`bd_phone_prefixes`, `bd_na_words`.

## Examples

``` r
library(bdscrub)

# Mobile numbers
bd_phone(c("01712345678", "+880 1712-345678", "1912345678", "017-1234"))
#> [1] "01712345678" "01712345678" "01912345678" NA
#> Warning: 1 non-missing value could not be read as mobile numbers and was set to NA.

# Money
bd_amount(c("Tk 1,25,000", "1.5 lakh", "1 crore 25 lakh", "25k"))
#> [1]   125000   150000 12500000    25000

# Place names: other spellings and typing errors are fixed and reported
bd_place(c("Chittagong", "jessore district", "Nababganj", "Kishorgnj"))
#> [1] "Chattogram"       "Jashore"          "Chapai Nawabganj" "Kishoreganj"
#> 2 values were not written as the standard district name and were changed:
#>   "Kishorgnj" -> "Kishoreganj" (typing error, corrected by fuzzy matching)
#>   "Nababganj" -> "Chapai Nawabganj" (typing error, corrected by fuzzy matching)
#> Typing-error corrections are guesses: check them by hand.

# Thanas: the same name exists in several districts, so give the district
bd_place(c("Kaliganj", "Sadar", "Savar Upazila"), level = "thana",
         district = c("Gazipur", "Feni", NA))
#> [1] "Kaliganj"   "Feni Sadar" "Savar"
#> 1 value was not written as the standard thana name and was changed:
#>   "Sadar" -> "Feni Sadar" ("Sadar" read with the district given)

# Names for joining to a map (boundary-file spelling)
bd_place(c("Chattogram", "Brahmanbaria"), style = "map")
#> [1] "Chittagong"    "Brahamanbaria"

# Addresses
bd_address(c("House 12, Road 5, Mirpur 10, Dhaka-1216", "Ashulia, Savar, Dhaka"))
#>   postcode   post_office   thana district division   status
#> 1     1216    Mirpur TSO  Mirpur    Dhaka    Dhaka complete
#> 2     <NA>          <NA> Ashulia    Dhaka    Dhaka complete
```

A whole data frame at once:

``` r
survey <- data.frame(
  mobile  = c("01712345678", "+880 1812-345678", "017-1234"),
  zila    = c("Chittagong", "jessore district", "Nababganj"),
  income  = c("Tk 25,000", "1.5 lakh", "BDT 1 crore 20 lakh"),
  address = c("Agrabad, Chattogram 4100", "Kotwali, Jessore", "Shibganj")
)
spec <- c(mobile = "phone", zila = "district", income = "amount",
          address = "address")

bd_check(survey, spec, district = "zila")                  # what can be read
bd_check(survey, spec, district = "zila", details = TRUE)  # what cannot, and why
bd_clean(survey, spec, district = "zila")                  # adds the cleaned columns
```

## Learn more

* `vignette("bdscrub")` and the help pages (`?bd_place`, `?bd_address`, ...).
* [`extras/bdscrub_guide.pdf`](extras/bdscrub_guide.pdf): every function and
  argument with examples (17 pages).
* [`extras/demo_bdscrub.R`](extras/demo_bdscrub.R): runs every function on a
  messy survey file of 3,030 rows (`extras/data/`) and scores the results
  against an answer key.
* [`extras/bdscrub_slides.html`](extras/bdscrub_slides.html): a short
  presentation (download and open in a browser).

## Notes on the reference data

* District and division spellings follow ISO 3166-2, including the April 2018
  changes (Chattogram, Barishal, Cumilla, Jashore, Bogura). `style = "legacy"`
  gives the older spellings, `style = "bangla"` Bengali script, and
  `style = "map"` the spellings of the HDX/BBS boundary files.
* Below the district, bdscrub works with thanas: 503 rural thanas (same name
  and area as the upazila), 61 city thanas and 6 thanas inside a larger rural
  thana (Ashulia, Tongi, ...). Upazila names are read as thana names.
* Post codes come from the Bangladesh Post district pages (1,329 offices).
* The operator of a phone prefix is the operator it was **allocated** to.
  Since mobile number portability began in 2018 a number may be served by
  another operator.

Known gaps: post codes of Mymensingh division, Bangla thana names and newer
metropolitan thanas are not in the data yet. The address reader has been
tested on constructed cases and generated data, not yet on a hand-coded
sample of real survey addresses. Sources of all data are listed in
[`inst/COPYRIGHTS`](inst/COPYRIGHTS).

## Contributing

Found a spelling that is not recognised, or a wrong result? Please open an
issue with the input value and what you expected. Missing spellings are the
most useful contribution: they go into the CSV files in `data-raw/`.

## Authors

Ishtiaq Mahmud Fahim and Humaira Kashmi.

## Licence

MIT. See [LICENSE.md](LICENSE.md).
