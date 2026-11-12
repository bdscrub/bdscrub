# Money amounts ---------------------------------------------------------------

# Units recognised in money amounts. Bangla words are written as escapes:
# koti = \u0995\u09cb\u099f\u09bf, lakh = \u09b2\u09be\u0996,
# lokkho = \u09b2\u0995\u09cd\u09b7, hajar = \u09b9\u09be\u099c\u09be\u09b0.
.amount_units <- data.frame(
  pattern = c(
    "crores?|cr|koti|\u0995\u09cb\u099f\u09bf",
    "billions?|bn",
    "millions?|mn",
    "lakhs?|lacs?|lk|\u09b2\u09be\u0996|\u09b2\u0995\u09cd\u09b7",
    "thousands?|hajar|hazar|k|\u09b9\u09be\u099c\u09be\u09b0"
  ),
  # Units are powers of ten. The value is built as text such as "1.1e5" and
  # parsed once, which gives the nearest double to the exact amount;
  # multiplying 1.1 * 1e5 would give 110000.00000000001 instead.
  exponent = c(7L, 9L, 6L, 5L, 3L),
  stringsAsFactors = FALSE
)

.amount_unit_regex <- paste0(
  "(", paste(.amount_units$pattern, collapse = "|"), ")(?![a-z])"
)
.amount_number_regex <- "([-+]?(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+))"
.amount_token_regex <- paste0(.amount_number_regex, " ?(?:", .amount_unit_regex, ")?")

# Currency markers removed before parsing: the taka sign (U+09F3), Tk, BDT,
# taka, the Bangla word taka (\u099f\u09be\u0995\u09be), the endings "/-" and
# "/=", "only", and the Bangla word for only (\u09ae\u09be\u09a4\u09cd\u09b0).
.amount_currency_regex <- paste0(
  "\u09f3|(?<![a-z])(?:tk[.]?|bdt|taka|only)(?![a-z])|",
  "\u099f\u09be\u0995\u09be|\u09ae\u09be\u09a4\u09cd\u09b0|/[-=]"
)

# Power of ten for each unit ("" means no unit, exponent 0).
.unit_exponent <- function(unit) {
  out <- integer(length(unit))
  for (i in seq_len(nrow(.amount_units))) {
    hit <- unit != "" & grepl(paste0("^(?:", .amount_units$pattern[i], ")$"),
                              unit, perl = TRUE)
    out[hit] <- .amount_units$exponent[i]
  }
  out
}

# Exact value of a number written as text times 10^exponent.
.scale_number <- function(num, exponent) {
  as.numeric(paste0(num, "e", exponent))
}

.parse_one_amount <- function(s) {
  if (is.na(s) || s == "") {
    return(NA_real_)
  }
  if (!grepl(paste0("^(?:", .amount_token_regex, " ?)+$"), s, perl = TRUE)) {
    return(NA_real_)
  }
  tokens <- regmatches(s, gregexpr(.amount_token_regex, s, perl = TRUE))[[1]]
  parts <- regmatches(tokens, regexec(paste0("^", .amount_token_regex, "$"),
                                      tokens, perl = TRUE))
  num <- vapply(parts, function(p) p[2L], character(1))
  unit <- vapply(parts, function(p) p[3L], character(1))
  unit[is.na(unit)] <- ""
  n <- length(tokens)
  if (n > 1L) {
    # "1 crore 25 lakh" is fine; "100 200", "25 lakh 1 crore" are not.
    if (any(unit[-n] == "")) return(NA_real_)
    if (any(grepl("^[-+]", num[-1L]))) return(NA_real_)
    if (any(diff(.unit_exponent(unit)) >= 0)) return(NA_real_)
  }
  value <- abs(.scale_number(num, .unit_exponent(unit)))
  total <- if (n > 1L && any(value[-1L] >= 10^.unit_exponent(unit)[-n])) NA_real_ else sum(value)
  if (grepl("^-", num[1L])) -total else total
}


# Exact amounts (single values only; ranges and approximate values give NA).
.parse_amount <- function(x) {
  s <- tolower(.bn_compose(.bn2en(x)))
  # A hyphen or dash right after the currency is a separator, not a minus
  # sign: "BDT-500", "Tk.-500", "Tk - 500" are 500. A real minus sign
  # (U+2212) and a sign before the currency ("-Tk 500") stay negative.
  s <- gsub("(\u09f3|(?<![a-z])(?:tk[.]?|bdt|taka))\\s*[-\u2010-\u2014\uff0d]\\s*(?=[0-9.])",
            "\\1 ", s, perl = TRUE)
  s <- .ascii_signs(s)
  s <- gsub(.amount_currency_regex, " ", s, perl = TRUE)
  s <- .squish(.ungroup_digits(s))
  # Punctuation that ends a sentence carries no value: "5 lakh.", "500 taka<danda>"
  s <- .squish(sub("[\u0964.,;:!?]+$", "", s, perl = TRUE))
  # A sign written before the currency ("-Tk 500") is now separated from
  # the number by a space; join them again.
  s <- sub("^([-+]) (?=[0-9.])", "\\1", s, perl = TRUE)
  # Parse each distinct value once; survey columns repeat values a lot.
  u <- unique(s)
  val <- rep(NA_real_, length(u))
  # Fast vectorised path: a single number with an optional unit.
  single <- paste0("^", .amount_token_regex, "$")
  one <- !is.na(u) & grepl(single, u, perl = TRUE)
  if (any(one)) {
    num <- sub(single, "\\1", u[one], perl = TRUE)
    unit <- sub(single, "\\2", u[one], perl = TRUE)
    val[one] <- .scale_number(num, .unit_exponent(unit))
  }
  # Slow path only for possible combined amounts such as "1 crore 25 lakh":
  # at least two numbers and no comma left over from wrong digit grouping.
  multi <- which(!one & !is.na(u) & !grepl(",", u, fixed = TRUE) &
                   grepl("[0-9][^0-9.]+[-+.]?[0-9]", u, perl = TRUE))
  if (length(multi)) {
    val[multi] <- vapply(u[multi], .parse_one_amount, numeric(1), USE.NAMES = FALSE)
  }
  val[match(s, u)]
}


# Words that mark an approximate amount: about, approx(imately), around,
# roughly, nearly, almost, "~", and the Bangla words praay and anumanik.
.rough_regex <- paste0(
  "(?i)(?<![a-z])(?:approx(?:imately)?[.]?|about|around|roughly|nearly|almost|apx)(?![a-z])|",
  "[~\u2248]|\u09aa\u09cd\u09b0\u09be(?:\u09af\u09bc|\u09df)|\u0986\u09a8\u09c1\u09ae\u09be\u09a8\u09bf\u0995"
)
# A range: two parts with a number each, joined by a dash, "to" or the Bangla
# word theke ("from").
.range_regex <- paste0("^(.*[0-9].*?)\\s*(?:\\p{Pd}|\u2212|(?<![a-z])to(?![a-z])|",
                       "\u09a5\u09c7\u0995\u09c7)\\s*(.*[0-9].*)$")

# A time period after an amount ("per month", "monthly", the Bangla word
# mase): it does not change the amount, so it is removed and noted.
.period_regex <- paste0(
  "(?i)\\s*(?:(?:per|a|/|each|every)\\s*(?:month|mon|mo|year|yr|annum|week|day)|",
  "monthly|yearly|annually|weekly|daily|\u09aa\u09cd\u09b0\u09a4\u09bf\\s*\u09ae\u09be\u09b8\u09c7|",
  "\u09ae\u09be\u09b8\u09bf\u0995|\u09ae\u09be\u09b8\u09c7)\\s*[.]?\\s*$")

# Remove a time period at the end; returns the text and what was removed.
.drop_period <- function(s) {
  has <- !is.na(s) & grepl(.period_regex, s, perl = TRUE)
  what <- rep(NA_character_, length(s))
  if (any(has)) {
    what[has] <- .squish(regmatches(s[has], regexpr(.period_regex, s[has], perl = TRUE)))
    s[has] <- sub(.period_regex, "", s[has], perl = TRUE)
  }
  list(s = s, what = what)
}

# The unit written in a piece of text ("" if none).
.unit_of <- function(t) {
  t <- tolower(.bn_compose(t))
  m <- regmatches(t, regexpr(.amount_unit_regex, t, perl = TRUE))
  if (length(m)) m else ""
}

# Why an amount could not be read.
.amount_reason <- function(x) {
  s <- tolower(.bn2en(x))
  out <- rep("not an amount", length(s))
  out[!grepl("[0-9]", s)] <- "no number"
  out[grepl("[0-9],[0-9]", s) &
        is.na(bd_number(gsub("[^0-9,]", "", s), warn = FALSE))] <- "commas in unusual places"
  out[grepl(paste0("[0-9] ?(?:", .amount_unit_regex, ") ?[0-9]"), s, perl = TRUE)] <-
    "parts that do not add up (for example 12 hajar 1335)"
  out
}

# Amounts as ranges: low, high, approximate and a note for each value.
.amount_details <- function(input) {
  n <- length(input)
  note <- rep(NA_character_, n)
  per <- .drop_period(input)
  rough <- !is.na(per$s) & grepl(.rough_regex, per$s, perl = TRUE)
  s <- per$s
  s[rough] <- .squish(gsub(.rough_regex, " ", s[rough], perl = TRUE))
  single <- .parse_amount(s)
  low <- high <- single
  s_en <- .bn2en(s)
  try <- which(is.na(single) & !is.na(s_en) &
                 grepl(.range_regex, s_en, perl = TRUE, ignore.case = TRUE))
  for (i in try) {
    lo_t <- sub(.range_regex, "\\1", s_en[i], perl = TRUE, ignore.case = TRUE)
    hi_t <- sub(.range_regex, "\\2", s_en[i], perl = TRUE, ignore.case = TRUE)
    lo <- .parse_amount(lo_t)
    hi <- .parse_amount(hi_t)
    # "15-20k": a unit only at the upper end is meant for both ends.
    uh <- .unit_of(hi_t)
    if (!is.na(lo) && !is.na(hi) && .unit_of(lo_t) == "" && uh != "") {
      lo2 <- .parse_amount(paste(lo_t, uh))
      if (!is.na(lo2) && lo2 < hi) {
        lo <- lo2
        note[i] <- "unit of the upper end used for both ends"
      }
    }
    if (!is.na(lo) && !is.na(hi) && lo < hi) {
      low[i] <- lo
      high[i] <- hi
      note[i] <- if (is.na(note[i])) "a range" else paste0("a range; ", note[i])
    } else if (!is.na(lo) && !is.na(hi)) {
      note[i] <- "range with the lower end not below the upper end"
    }
  }
  approx <- rough & !is.na(low)
  note[approx] <- ifelse(is.na(note[approx]), "approximate", paste0("approximate; ", note[approx]))
  bad <- is.na(low) & .nonblank(input) & is.na(note)
  note[bad] <- .amount_reason(s[bad])
  pn <- !is.na(per$what) & !is.na(low)
  note[pn] <- ifelse(is.na(note[pn]), paste0("\"", per$what[pn], "\" ignored"),
                     paste0(note[pn], "; \"", per$what[pn], "\" ignored"))
  exact <- ifelse(!is.na(low) & low == high & !approx, low, NA_real_)
  data.frame(input = input, amount = exact, low = low, high = high,
             approximate = ifelse(.nonblank(input), approx, NA), note = note,
             stringsAsFactors = FALSE)
}

#' Read money amounts written in Bangladeshi styles
#'
#' Reads amounts such as "Tk 1,25,000", "BDT 3.2 crore", "1.5 lakh",
#' "500/-", "25k" or the same written in Bangla, and returns them in taka.
#'
#' The reader:
#' - reads Bangla digits and ignores letter case;
#' - removes the taka sign, "Tk", "Tk.", "BDT", "taka" (also in Bangla),
#'   the endings "/-" and "/=", the word "only" (also in Bangla) and a full
#'   stop or comma at the end;
#' - removes the commas of correctly grouped digits, in South Asian
#'   (1,25,000) or international (125,000) grouping; wrongly grouped digits
#'   ("1,2,5", "Tk 25,00") are not read, because they could mean two
#'   different amounts;
#' - multiplies by the units thousand ("k", "hajar"), lakh ("lac", "lk"),
#'   million ("mn"), crore ("cr", "koti") and billion ("bn"), in English or
#'   Bangla;
#' - adds up combined amounts such as "1 crore 25 lakh" or "12 hajar 335"
#'   when the units decrease from left to right and each part is smaller
#'   than the unit before it ("12 hajar 1335" is rejected as a likely typing
#'   error).
#'
#' Amounts with units are computed exactly: "1.1 lakh" is exactly 110000.
#'
#' **Ranges and approximate amounts.** "15000-20000", "15k to 20k", "2 lakh
#' theke 3 lakh" (theke means "from ... to" in Bangla) are ranges; "about 5
#' lakh", "approx 30000", "~25k" are approximate. They are not exact
#' amounts, so the plain result is `NA` for them; `details = TRUE` gives
#' their lower and upper ends and marks approximate values. What to do with
#' a range (the midpoint, an interval model) is a decision for the analysis,
#' not for data cleaning. A range with a unit only at the upper end
#' ("15-20k") uses that unit for both ends, which is how such ranges are
#' normally meant; `note` says so.
#'
#' @param x A character vector of amounts.
#' @param details If `FALSE` (the default), return the exact amounts. If
#'   `TRUE`, return a table with one row per value (see Value).
#' @param warn If `TRUE` (the default), warn how many non-missing values are
#'   not exact amounts.
#' @return With `details = FALSE`, a numeric vector of the same length as
#'   `x`. With `details = TRUE`, a tibble (a data frame if tibble is not
#'   installed) with columns:
#'   \describe{
#'     \item{input}{the value as given.}
#'     \item{amount}{the exact amount; `NA` for ranges, approximate and
#'       unreadable values.}
#'     \item{low, high}{the lower and upper end; equal for a single amount.}
#'     \item{approximate}{`TRUE` if written as approximate.}
#'     \item{note}{"a range", "approximate", or why the value could not be
#'       read.}
#'   }
#' @seealso [bd_taka()] to write amounts, [bd_number()] for plain numbers.
#' @examples
#' x <- c("Tk 1,25,000", "BDT 3.2 crore", "1.5 lakh", "1 crore 25 lakh",
#'        "500/-", "25k", "15-20k", "about 5 lakh", "Tk 25,00")
#' bd_amount(x, warn = FALSE)
#' bd_amount(x, details = TRUE)
#'
#' # "5 lakh taka" written in Bangla
#' bn <- intToUtf8(c(0x09EB, 0x20, 0x09B2, 0x09BE, 0x0996, 0x20, 0x099F, 0x09BE, 0x0995, 0x09BE))
#' bn
#' bd_amount(bn)
#' @export
bd_amount <- function(x, details = FALSE, warn = TRUE) {
  .check_flag(details, "details")
  .check_flag(warn, "warn")
  input <- .as_utf8_chr(x)
  if (!details) {
    out <- .parse_amount(.drop_period(input)$s)
    .warn_invalid(sum(is.na(out) & .nonblank(input)), "read as exact amounts", warn)
    return(out)
  }
  d <- .amount_details(input)
  .warn_invalid(sum(is.na(d$low) & .nonblank(input)), "read as amounts", warn)
  .as_tbl(d)
}

#' Write amounts with lakh and crore grouping
#'
#' Formats numbers the way amounts are written in Bangladesh: the last three
#' digits form one group and the digits before them are grouped in pairs, so
#' 12500000 becomes "1,25,00,000".
#'
#' @param x A numeric vector.
#' @param digits Number of decimal places. Default 0.
#' @param prefix,suffix Text put before and after each number, for example
#'   `prefix = "Tk "` or `suffix = "/-"`.
#' @param bangla If `TRUE`, write the digits in Bangla. Default `FALSE`.
#' @return A character vector of the same length as `x`, with `NA` where `x`
#'   is missing or not finite.
#' @seealso [bd_amount()] for the reverse.
#' @examples
#' bd_taka(c(500, 125000, 12500000, -9876543.21), digits = 2)
#' bd_taka(1250000, prefix = "Tk ", suffix = "/-")
#' bd_taka(125000, bangla = TRUE)
#' @export
bd_taka <- function(x, digits = 0, prefix = "", suffix = "", bangla = FALSE) {
  if (!is.numeric(x)) {
    stop("`x` must be numeric.", call. = FALSE)
  }
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
      digits < 0 || digits != round(digits)) {
    stop("`digits` must be a single non-negative whole number.", call. = FALSE)
  }
  if (!is.character(prefix) || length(prefix) != 1L ||
      !is.character(suffix) || length(suffix) != 1L) {
    stop("`prefix` and `suffix` must be single strings.", call. = FALSE)
  }
  .check_flag(bangla, "bangla")
  ok <- is.finite(x)
  out <- rep(NA_character_, length(x))
  if (any(ok)) {
    v <- x[ok]
    txt <- formatC(abs(v), format = "f", digits = digits, big.mark = "")
    int <- sub("[.].*$", "", txt)
    frac <- ifelse(grepl("[.]", txt), sub("^[^.]*", "", txt), "")
    long <- nchar(int) > 3L
    head <- substr(int[long], 1L, nchar(int[long]) - 3L)
    tail <- substr(int[long], nchar(int[long]) - 2L, nchar(int[long]))
    head <- gsub("([0-9])(?=([0-9]{2})+$)", "\\1,", head, perl = TRUE)
    int[long] <- paste0(head, ",", tail)
    neg <- v < 0 & grepl("[1-9]", txt)
    out[ok] <- paste0(ifelse(neg, "-", ""), prefix, int, frac, suffix)
  }
  if (bangla) {
    out <- chartr(.en_digits, .bn_digits, out)
  }
  out
}
