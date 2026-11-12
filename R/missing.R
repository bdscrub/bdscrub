#' Remove extra spaces
#'
#' Collapses runs of spaces, tabs and line breaks into one space and removes
#' spaces at the start and end. Non-breaking and other Unicode spaces, the
#' zero-width space and the byte-order mark (often at the start of a CSV
#' file) are handled too.
#'
#' @param x A character vector.
#' @param strip_quotes If `TRUE`, also remove quotes around the whole value,
#'   such as the apostrophe Excel puts before numbers stored as text.
#'   Default `FALSE`.
#' @return A character vector of the same length as `x`.
#' @examples
#' bd_squish(c("  Dhaka   District ", paste0("Tk", intToUtf8(0xA0), "500"), "'01712345678"))
#' bd_squish("'01712345678", strip_quotes = TRUE)
#' @export
bd_squish <- function(x, strip_quotes = FALSE) {
  .check_flag(strip_quotes, "strip_quotes")
  .squish(.as_utf8_chr(x), strip_quotes = strip_quotes)
}

#' Turn missing-value markers into NA
#'
#' Spreadsheets and survey exports mark missing values in many ways: "N/A",
#' "-", "#N/A", "null", "not applicable", the Bangla words for "not
#' applicable" and "no information", or an empty cell. `bd_missing()` turns
#' these into `NA` and leaves everything else unchanged. Upper and lower case
#' and extra spaces do not matter.
#'
#' Words such as "none", "unknown" or "don't know" are not treated as missing
#' by default, because in a survey they are often real answers. Add them with
#' `extra` when they mean missing in your data. The built-in list, with the
#' meaning of each marker, is the data set [bd_na_words].
#'
#' @param x A vector (character, factor or numeric).
#' @param extra Optional character vector of more markers, for example
#'   `c("none", "unknown")`.
#' @param markers The markers to use. `NULL` (the default) uses the built-in
#'   list ([bd_na_words]); give your own vector to replace it.
#' @param blank If `TRUE` (the default), empty and space-only values become
#'   `NA` too.
#' @return `x` with the markers set to `NA`, of the same type and length.
#' @examples
#' bd_missing(c("23", "N/A", " - ", "#N/A", "none", "41"))
#' bd_missing(c("23", "none", "unknown"), extra = c("none", "unknown"))
#' @export
bd_missing <- function(x, extra = NULL, markers = NULL, blank = TRUE) {
  .check_flag(blank, "blank")
  if (!is.null(markers) && !is.character(markers) || !is.null(extra) && !is.character(extra)) {
    stop("`markers` and `extra` must be character vectors.", call. = FALSE)
  }
  if (is.null(markers)) markers <- .tab_na_words
  key <- function(v) tolower(.bn_key(.squish(.as_utf8_chr(v))))
  hit <- key(x) %in% key(c(markers, extra))
  if (blank) hit <- hit | !.nonblank(x)
  hit <- hit & !is.na(x)
  if (is.factor(x)) {
    x[hit] <- NA
    return(droplevels(x, exclude = NA))
  }
  x[hit] <- NA
  x
}
