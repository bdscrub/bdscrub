#' bdscrub: Clean Bangladesh-Specific Data Fields
#'
#' Tools for the fields that make Bangladeshi survey and administrative data
#' messy. Eleven functions:
#'
#' - [bd_phone()]: mobile numbers in one format; several numbers per cell, operator;
#' - [bd_amount()]: money amounts with lakh, crore, Bangla digits; ranges;
#' - [bd_number()]: plain numbers with Bangla digits and lakh grouping;
#' - [bd_taka()]: write amounts with lakh and crore grouping;
#' - [bd_digits()]: Bangla digits to English digits and back;
#' - [bd_place()]: district, division and thana (upazila) names; codes; map spellings;
#' - [bd_address()]: post code, thana, district and division from an address;
#' - [bd_missing()]: missing-value markers ("N/A", "-", "#N/A") to `NA`;
#' - [bd_squish()]: extra and invisible spaces;
#' - [bd_check()]: how many values of each column can be read, and why not;
#' - [bd_clean()]: clean many columns of a data frame at once.
#'
#' Data sets: [bd_districts], [bd_divisions], [bd_thanas],
#' [bd_postcodes], [bd_phone_prefixes], [bd_na_words].
#'
#' All functions work offline and need only base R. Tables are returned as
#' tibbles when the tibble package is installed.
#'
#' @keywords internal
"_PACKAGE"
