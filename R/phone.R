# Phone numbers --------------------------------------------------------------

# A Bangladeshi mobile number is 11 digits: 0, then 1, an operator digit
# (3 to 9) and 8 more digits. Before the national number there may be the
# country code (+880, 00880 or 880) and/or the trunk 0.
.phone_pattern <- "^(?:(?:[+]|00)?880)?0?(1[3-9][0-9]{8})$"
.phone_has_prefix <- "^(?:(?:[+]|00)?880|0)"
# Characters people put inside one number: every space and dash, brackets,
# dots, slashes, quotes and invisible characters.
.phone_inside_re <- "[\\s\\p{Zs}\\p{Pd}\u200b\u200c\u200d\ufeff()./\"'\u201c\u201d\u2018\u2019]"
# Separators between two numbers in one cell: / , ; & | "or" and line breaks.
.phone_split_re <- "\\s*(?:/|,|;|&|\\||\\n|(?<![a-z])or(?![a-z]))\\s*"
# A run of digits with the characters allowed inside a number.
.phone_run_re <- "(?:[+]|00)?[0-9](?:[0-9]|[\\s\\p{Pd}.()])*[0-9]"
# One complete number with its 0 or country code, possibly with spaces or
# dashes inside, used to find numbers that are separated only by spaces
# ("01712345678 01812345678").
.phone_strict_re <- "(?<![0-9])(?:(?:[+]|00)?880[\\s-]?|0)1[3-9](?:[\\s-]?[0-9]){8}(?![0-9])"
# +881 followed by a 10-digit mobile number (see `plus881`).
.phone_881_re <- "^(?:[+]|00)?881([3-9][0-9]{8})$"

# The 10-digit national number (1XXXXXXXXX) of each value, or NA.
.phone_core <- function(x, allow_missing_zero = TRUE) {
  s <- .ascii_signs(.bn2en(x))
  # A number stored as text by a spreadsheet can end in ".0" (8801712345678.0).
  s <- sub("^\\s*([+]?[0-9]+)[.]0+\\s*$", "\\1", s, perl = TRUE)
  s <- gsub(.phone_inside_re, "", s, perl = TRUE)
  ok <- !is.na(s) & grepl(.phone_pattern, s, perl = TRUE)
  if (!allow_missing_zero) ok <- ok & grepl(.phone_has_prefix, s, perl = TRUE)
  out <- rep(NA_character_, length(s))
  out[ok] <- sub(.phone_pattern, "\\1", s[ok], perl = TRUE)
  out
}

# Write 10-digit national numbers in the chosen format.
.phone_format <- function(core, format) {
  out <- rep(NA_character_, length(core))
  ok <- !is.na(core)
  out[ok] <- switch(format,
    local = paste0("0", core[ok]),
    e164 = paste0("+880", core[ok]),
    international = paste0("+880 ", substr(core[ok], 1L, 4L), "-", substr(core[ok], 5L, 10L))
  )
  out
}

# Why a value is not a mobile number (one short reason per value).
.phone_reason <- function(x) {
  s <- .ascii_signs(.bn2en(x))
  compact <- gsub("[^0-9+]", "", s)
  core <- sub("^(?:(?:[+]|00)?880|0)", "", compact, perl = TRUE)
  nd <- nchar(gsub("[^0-9]", "", s))
  out <- rep("not a mobile number", length(s))
  out[nd == 0L] <- "no digits"
  out[nd > 0L & nchar(core) < 10L] <- "too few digits"
  out[nchar(core) > 10L] <- "too many digits"
  out[nchar(core) == 10L & !grepl("^1[3-9]", core)] <- "not a mobile prefix (013 to 019)"
  out[grepl("^(?:[+]|00)?881", compact, perl = TRUE)] <-
    "country code +881 (satellite phones); +880 with its 0 missing? See `plus881`"
  out[grepl("^\\s*[0-9][.,][0-9]+\\s*[eE][+]?[0-9]+\\s*$", s)] <-
    "Excel scientific notation: the digits are lost"
  out[is.na(s)] <- NA_character_
  out
}

# Every mobile number in one cell: national numbers and a note.
.phone_find <- function(v, allow_missing_zero, plus881) {
  parts <- strsplit(tolower(.bn2en(v)), .phone_split_re, perl = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  orig <- parts
  # Keep the digit run of each part, so that words around a number
  # ("phone:", "alt", "(home)") do not matter.
  has <- grepl(.phone_run_re, parts, perl = TRUE)
  parts[has] <- regmatches(parts, regexpr(.phone_run_re, parts, perl = TRUE))
  core <- .phone_core(parts, allow_missing_zero)
  # A part may hold more numbers separated only by spaces or words
  # ("01712345678 01812345678", "Mob 0171... Home 0181..."); every complete
  # number in it is taken too.
  for (i in seq_along(orig)) {
    found <- regmatches(orig[i], gregexpr(.phone_strict_re, orig[i], perl = TRUE))[[1]]
    if (length(found) > 1L || (length(found) && is.na(core[i]))) {
      core <- c(core, .phone_core(found))
    }
  }
  note <- character(0)
  if (plus881) {
    t <- gsub("[^0-9+]", "", .ascii_signs(parts))
    fix <- is.na(core) & grepl(.phone_881_re, t, perl = TRUE)
    if (any(fix)) {
      core[fix] <- paste0("1", sub(.phone_881_re, "\\1", t[fix], perl = TRUE))
      note <- c(note, paste0("read +881 as +880 with the 0 missing: ",
                             paste(orig[fix], collapse = ", ")))
    }
  }
  nums <- unique(core[!is.na(core)])
  if (!length(nums)) note <- c(note, unique(.phone_reason(orig)))
  list(nums = nums, note = if (length(note)) paste(note, collapse = "; ") else NA_character_)
}

#' Clean Bangladeshi mobile numbers
#'
#' Reads mobile numbers however they were typed and writes them in one
#' format. It removes spaces, dashes, dots, brackets and quotes, reads Bangla
#' digits and accepts the country code written as `+880`, `00880` or `880`.
#' A number is valid when what is left is a 10-digit national number starting
#' with 13 to 19, with or without the leading 0. This checks the format only;
#' it cannot tell whether a number is in service.
#'
#' A cell may hold several numbers ("01712345678 / 01812345678",
#' "01712345678, alt 01912345678"). They are separated at "/", ",", ";",
#' "&", "|", "or" and line breaks. Which number to keep is set by `multiple`;
#' `details = TRUE` lists all of them.
#'
#' Bangladesh's country code is +880. "+881" is the code for satellite
#' phones, but it is a frequent typing error for "+880" with the 0 dropped
#' ("+881712345678"). Such values are invalid unless you set
#' `plus881 = TRUE`, and every value read that way is named in `note`.
#'
#' @param x A character, numeric or factor vector. Numbers stored as numbers
#'   keep all their digits.
#' @param format How to write valid numbers: `"local"` (01712345678, the
#'   default), `"e164"` (+8801712345678, for SMS gateways and databases) or
#'   `"international"` (+880 1712-345678, for printing).
#' @param details If `FALSE` (the default), return the clean numbers. If
#'   `TRUE`, return a table with one row per value (see Value).
#' @param multiple What to return when a cell has more than one number:
#'   `"na"` (the default) returns `NA`, so that no number is chosen silently;
#'   `"first"` returns the first number. With `details = TRUE` all numbers
#'   are in `all_numbers` either way.
#' @param allow_missing_zero Accept 10-digit numbers without the leading 0
#'   (1712345678)? Spreadsheets drop the 0 when a column is stored as
#'   numbers. Default `TRUE`.
#' @param plus881 Read "+881" followed by ten digits as "+880" with its 0
#'   missing? Default `FALSE`; turn it on only when you know your data has
#'   this typing error.
#' @param warn If `TRUE` (the default), warn how many non-missing values gave
#'   no number.
#' @return With `details = FALSE`, a character vector of the same length as
#'   `x`, with `NA` for missing and invalid values. With `details = TRUE`, a
#'   tibble (a data frame if tibble is not installed) with columns:
#'   \describe{
#'     \item{input}{the value as given.}
#'     \item{phone}{the clean number, chosen as set by `multiple`.}
#'     \item{valid}{`TRUE` if the cell has at least one valid number, `NA`
#'       for missing values.}
#'     \item{n_numbers}{how many different numbers the cell has.}
#'     \item{all_numbers}{all of them, separated by "; ".}
#'     \item{operator}{the operator the prefix of `phone` was allocated to
#'       (see [bd_phone_prefixes]). Since mobile number portability (October
#'       2018), this is not necessarily the current operator.}
#'     \item{note}{why there is no number, or what was assumed.}
#'   }
#' @seealso [bd_phone_prefixes] for the operator table, [bd_clean()] to clean
#'   a phone column in a data frame.
#' @examples
#' x <- c("01712345678", "+880 1712-345678", "8801812345678", "1912345678",
#'        "0171234567", "01712345678 / 01812345678", "+881712345678", NA)
#' bd_phone(x, warn = FALSE)
#' bd_phone(x, format = "e164", multiple = "first", warn = FALSE)
#' bd_phone(x, details = TRUE, plus881 = TRUE)
#' @export
bd_phone <- function(x, format = c("local", "e164", "international"), details = FALSE,
                     multiple = c("na", "first"), allow_missing_zero = TRUE,
                     plus881 = FALSE, warn = TRUE) {
  format <- match.arg(format)
  multiple <- match.arg(multiple)
  .check_flag(details, "details")
  .check_flag(allow_missing_zero, "allow_missing_zero")
  .check_flag(plus881, "plus881")
  .check_flag(warn, "warn")
  input <- .as_utf8_chr(x)
  present <- .nonblank(input)

  # Work on the distinct values only; survey columns repeat values.
  u <- unique(input[present])
  core <- .phone_core(u, allow_missing_zero)
  nums <- as.list(core)
  note <- rep(NA_character_, length(u))
  # Values that are not one clean number: look for numbers inside them.
  for (i in which(is.na(core))) {
    f <- .phone_find(u[i], allow_missing_zero, plus881)
    nums[[i]] <- f$nums
    note[i] <- f$note
  }
  n_num <- lengths(nums)
  first <- vapply(nums, function(z) if (length(z)) z[1L] else NA_character_, "")
  chosen <- first
  if (multiple == "na") chosen[n_num > 1L] <- NA_character_
  several <- n_num > 1L
  note[several] <- paste0(n_num[several], " numbers in one cell; ",
                          if (multiple == "na") "phone is NA (see `multiple`)" else "phone is the first",
                          ifelse(is.na(note[several]), "", paste0("; ", note[several])))

  idx <- match(input, u)
  phone <- .phone_format(chosen[idx], format)
  .warn_invalid(sum(present & is.na(first[idx])), "read as mobile numbers", warn)
  if (!details) return(phone)

  all <- vapply(nums, function(z) if (length(z)) paste(.phone_format(z, format), collapse = "; ")
                else NA_character_, "")
  op <- .tab_prefixes$operator[match(paste0("0", substr(chosen, 1L, 2L)), .tab_prefixes$prefix)]
  .as_tbl(data.frame(
    input = input,
    phone = phone,
    valid = ifelse(present, !is.na(first[idx]), NA),
    n_numbers = ifelse(present, n_num[idx], 0L),
    all_numbers = all[idx],
    operator = op[idx],
    note = note[idx],
    stringsAsFactors = FALSE
  ))
}
