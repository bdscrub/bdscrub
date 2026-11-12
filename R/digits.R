#' Convert between Bangla and English digits
#'
#' Replaces the ten Bangla digits (U+09E6 to U+09EF) by 0 to 9, or the other
#' way round. Everything else in the text is left as it is.
#'
#' Every other bdscrub function already reads Bangla digits, so you need
#' `bd_digits()` only to change text you want to keep as text (for example a
#' column of house numbers), or to write numbers in Bangla for a report.
#'
#' @param x A character vector (numbers and factors are converted to text).
#' @param to `"en"` (the default) converts Bangla digits to English digits;
#'   `"bn"` converts English digits to Bangla digits.
#' @return A character vector of the same length as `x`.
#' @seealso [bd_number()] to read numbers, [bd_taka()] to write amounts.
#' @examples
#' # "01712345678" typed with Bangla digits
#' bn <- intToUtf8(0x09E6 + c(0, 1, 7, 1, 2, 3, 4, 5, 6, 7, 8))
#' bn
#' bd_digits(bn)
#' bd_digits("Room 205", to = "bn")
#' @export
bd_digits <- function(x, to = c("en", "bn")) {
  to <- match.arg(to)
  x <- .as_utf8_chr(x)
  if (to == "en") chartr(.bn_digits, .en_digits, x) else chartr(.en_digits, .bn_digits, x)
}

#' Read plain numbers
#'
#' Reads numbers written with Bangla or English digits and with or without
#' thousands separators, such as "1,25,000", " 34 " or 41 written in Bangla digits. The
#' commas must be in valid places (international 1,234,567 or South Asian
#' 12,34,567); "1,2,5" is not read.
#'
#' Anything else becomes `NA`: words, units and currency ("34 years",
#' "5 lakh", "Tk 500"). This is deliberate: use `bd_number()` for counts and
#' measurements such as age or household size, where "5 lakh" would be an
#' error, and [bd_amount()] for money.
#'
#' @param x A character vector.
#' @param warn If `TRUE` (the default), warn how many non-missing values could
#'   not be read.
#' @return A numeric vector of the same length as `x`.
#' @seealso [bd_amount()] for money amounts with units.
#' @examples
#' bd_number(c("1,25,000", intToUtf8(c(0x09EA, 0x09E7)), " 34 ", "abc", "5 lakh"),
#'           warn = FALSE)
#' @export
bd_number <- function(x, warn = TRUE) {
  .check_flag(warn, "warn")
  s <- .ungroup_digits(.squish(.ascii_signs(.bn2en(x))))
  ok <- !is.na(s) & grepl("^[-+]?([0-9]+([.][0-9]*)?|[.][0-9]+)$", s, perl = TRUE)
  out <- rep(NA_real_, length(s))
  out[ok] <- as.numeric(s[ok])
  .warn_invalid(sum(!ok & !is.na(s) & s != ""), "read as numbers", warn)
  out
}
