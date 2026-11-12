# bdscrub 1.0.0

* Below the district the package works with **thanas only** (8 November
  2026): there is no separate upazila level, column or data set. A rural
  thana has the same name and area as its upazila, so upazila names are read
  as thana names.
* Review fixes (8 November 2026): numbers separated only by spaces, and
  numbers ending in ".0", are read by `bd_phone()`; "per month", "monthly"
  and similar words after an amount are ignored and noted; "City" and "City
  Corporation" after a district name are ignored; `bd_address()` treats
  "N/A" and similar markers as missing and, when an address names a thana and
  the larger thana it lies in, gives the smaller one; Tongi added as a thana;
  two post office names corrected.

The interface was reduced to eleven functions; options that used to be
separate functions are now arguments.

* `bd_phone()` replaces `bd_phone_standardise()`, `bd_phone_valid()` and
  `bd_phone_prefix_operator()`. It finds several numbers in one cell
  (`multiple`), explains invalid values and gives the operator with
  `details = TRUE`, and can read the "+881" typing error (`plus881`).
* `bd_amount()` replaces `bd_parse_amount()`; `details = TRUE` reads ranges
  ("15-20k", "2 lakh theke 3 lakh") and approximate amounts.
* `bd_number()`, `bd_taka()` and `bd_digits()` replace `bd_parse_number()`,
  `bd_format_amount()`, `bn_to_en_digits()` and `en_to_bn_digits()`.
  `bd_has_bangla()` was removed.
* `bd_place()` replaces `bd_district_standardise()`,
  `bd_division_standardise()` and `bd_district_info()`, and adds thanas
  (`level = "thana"`, which also reads upazila names), boundary-file spellings (`style = "map"`) and a
  `district` argument for names used in several districts.
* **`fuzzy = TRUE` is now the default.** Every correction is listed in a
  message (`report`).
* New `bd_address()`: post code, post office, thana, district and
  division from free-text addresses, with a status and a note.
* `bd_missing()` replaces `bd_na_if()`; `bd_na_markers()` became the data set
  `bd_na_words`.
* `bd_check(details = TRUE)` lists every value that cannot be read, with the
  reason. `bd_check()` and `bd_clean()` know the types "thana" and
  "address" and take a `district` column.
* New data sets: `bd_thanas`, `bd_postcodes`, `bd_na_words`;
  `bd_districts` gains `district_map`.
* Tables are returned as tibbles when tibble is installed.

# bdscrub 0.1.0

* First version (never released).
