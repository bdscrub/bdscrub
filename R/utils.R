# Internal helpers --------------------------------------------------------

# Bengali digits U+09E6 to U+09EF, written with escapes so the source stays ASCII.
.bn_digits <- "\u09e6\u09e7\u09e8\u09e9\u09ea\u09eb\u09ec\u09ed\u09ee\u09ef"
.en_digits <- "0123456789"

# Coerce input to a UTF-8 character vector without losing digits of
# large whole numbers (for example phone numbers stored as numbers).
.as_utf8_chr <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (is.numeric(x)) {
    out <- rep(NA_character_, length(x))
    ok <- !is.na(x)
    whole <- ok & is.finite(x) & x == round(x)
    out[whole] <- formatC(x[whole], format = "f", digits = 0, big.mark = "")
    out[ok & !whole] <- format(x[ok & !whole], scientific = FALSE, trim = TRUE,
                               digits = 15)
    return(out)
  }
  if (!is.character(x) && !is.logical(x)) {
    stop("`x` must be a character, numeric or factor vector.", call. = FALSE)
  }
  x <- enc2utf8(as.character(x))
  # Text read with the wrong encoding can contain bytes that are not valid
  # UTF-8, which would make every regular expression fail with an error.
  # Such bytes are shown as "<ff>" etc., so the value stays visible but can
  # no longer match anything and is reported as invalid.
  bad <- !is.na(x) & !validUTF8(x)
  if (any(bad)) {
    x[bad] <- iconv(x[bad], "UTF-8", "UTF-8", sub = "byte")
  }
  x
}

# Repair the most common non-canonical Bengali vowel-sign sequences so that
# pattern matching does not depend on how the text was typed.
# U+09C7 U+09BE -> U+09CB (o-kar), U+09C7 U+09D7 -> U+09CC (au-kar).
.bn_compose <- function(x) {
  x <- gsub("\u09c7\u09be", "\u09cb", x, fixed = TRUE)
  gsub("\u09c7\u09d7", "\u09cc", x, fixed = TRUE)
}

.squish <- function(x, strip_quotes = TRUE) {
  # Zero-width space (U+200B) and byte-order mark (U+FEFF, often at the start
  # of a CSV file) are invisible and never meaningful in these fields.
  x <- gsub("[\u200b\ufeff]", "", x, perl = TRUE)
  # Every kind of space (\p{Zs} covers the non-breaking, thin and en spaces
  # that word processors and web pages insert) becomes one ordinary space.
  x <- gsub("[\\s\\p{Zs}]+", " ", x, perl = TRUE)
  x <- gsub("^ | $", "", x, perl = TRUE)
  if (strip_quotes) {
    # Quotes wrapped around a whole value ("'0171..." is Excel's text marker).
    x <- gsub("^[\"'\u201c\u201d\u2018\u2019]+|[\"'\u201c\u201d\u2018\u2019]+$", "", x, perl = TRUE)
  }
  gsub("^ | $", "", x, perl = TRUE)
}

# Unicode signs that look like + and - become ASCII: the minus sign (U+2212)
# and the full-width plus and hyphen-minus (U+FF0B, U+FF0D).
.ascii_signs <- function(x) {
  chartr("\u2212\uff0b\uff0d", "-+-", x)
}

.warn_invalid <- function(n, what, warn) {
  if (isTRUE(warn) && n > 0L) {
    warning(sprintf("%d non-missing value%s could not be %s and %s set to NA.",
                    n, if (n == 1L) "" else "s", what,
                    if (n == 1L) "was" else "were"),
            call. = FALSE)
  }
  invisible(NULL)
}

.check_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be TRUE or FALSE.", name), call. = FALSE)
  }
  invisible(x)
}

.nonblank <- function(x) {
  x <- .as_utf8_chr(x)
  !is.na(x) & .squish(x) != ""
}

# Normalise Bangla text for matching (not for display):
# - letters with nukta written as one code point (U+09DC rra, U+09DD rha,
#   U+09DF yya) are decomposed to letter + nukta (U+09BC), the canonical form;
# - zero-width joiner and non-joiner (U+200C, U+200D) are removed;
# - long vowel signs ii (U+09C0) and uu (U+09C2) become short i (U+09BF) and
#   u (U+09C1), a frequent spelling variation;
# - o-kar and au-kar typed as two code points are composed (.bn_compose()).
.bn_key <- function(x) {
  x <- gsub("\u09dc", "\u09a1\u09bc", x, fixed = TRUE)
  x <- gsub("\u09dd", "\u09a2\u09bc", x, fixed = TRUE)
  x <- gsub("\u09df", "\u09af\u09bc", x, fixed = TRUE)
  x <- gsub("[\u200c\u200d]", "", x, perl = TRUE)
  x <- chartr("\u09c0\u09c2", "\u09bf\u09c1", x)
  .bn_compose(x)
}

# Remove the commas from correctly grouped numbers and leave every other
# comma in place, so that badly grouped text (for example "1,2,3") later fails
# to parse instead of being silently read as 123. Two groupings are accepted:
# international  1,234,567  (groups of three)
# South Asian    12,34,567  (a group of three, then groups of two)
.ungroup_digits <- function(x) {
  # Only touch non-missing strings that contain a digit-comma-digit run;
  # `regmatches<-` would otherwise turn NA into the string "NA".
  has <- !is.na(x) & grepl("[0-9],[0-9]", x, perl = TRUE)
  if (!any(has)) {
    return(x)
  }
  run <- "[0-9]+(?:,[0-9]+)+"
  valid <- "^(?:[0-9]{1,3}(?:,[0-9]{3})+|[0-9]{1,2}(?:,[0-9]{2})+,[0-9]{3})$"
  fix <- function(v) {
    ok <- grepl(valid, v, perl = TRUE)
    v[ok] <- gsub(",", "", v[ok], fixed = TRUE)
    v
  }
  y <- x[has]
  n_runs <- lengths(gregexpr(run, y, perl = TRUE))
  # Common case, one grouped number per string: fully vectorised.
  one <- n_runs == 1L
  if (any(one)) {
    y1 <- y[one]
    m <- regexpr(run, y1, perl = TRUE)
    len <- attr(m, "match.length")
    y[one] <- paste0(substr(y1, 1L, m - 1L),
                     fix(substr(y1, m, m + len - 1L)),
                     substr(y1, m + len, nchar(y1)))
  }
  # Rare case, several grouped numbers in one string ("1,25,000 or 1,50,000").
  if (any(!one)) {
    y2 <- y[!one]
    g <- gregexpr(run, y2, perl = TRUE)
    regmatches(y2, g) <- lapply(regmatches(y2, g), fix)
    y[!one] <- y2
  }
  x[has] <- y
  x
}

# Bangla digits -> English digits (used by every parser).
.bn2en <- function(x) chartr(.bn_digits, .en_digits, .as_utf8_chr(x))

# Return a tibble when the tibble package is installed, else a data frame.
# bdscrub itself needs only base R; tibble is a suggested package.
.as_tbl <- function(df) {
  rownames(df) <- NULL
  if (requireNamespace("tibble", quietly = TRUE)) tibble::as_tibble(df) else df
}

# "Kaliganj (Gazipur); Kaliganj (Jhenaidah)": a short list of places, at most
# `max` shown.
.fmt_places <- function(name, district, max = 6L) {
  p <- unique(paste0(name, " (", district, ")"))
  if (length(p) > max) {
    paste0(paste(p[seq_len(max)], collapse = "; "), "; and ", length(p) - max, " more")
  } else {
    paste(p, collapse = "; ")
  }
}

.check_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be a single string.", name), call. = FALSE)
  }
  invisible(x)
}

.check_distance <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0) {
    stop("`max_distance` must be a single non-negative number.", call. = FALSE)
  }
  invisible(x)
}
