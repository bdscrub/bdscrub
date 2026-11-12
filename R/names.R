# Place-name matching shared by bd_place(), bd_address() and bd_clean() -----
#
# Steps for every level (district, division, thana):
#   1. .name_key()    reduce a name to a matching key;
#   2. .lookup()      all known spellings of the level, as keys;
#   3. .match_names() exact key match, then (optionally) a careful fuzzy match;
#   4. .report_changes() list the values that were changed, as a message.

# Words removed from every name: "District", "Zila", "Division" and the like.
.admin_words_re <- paste0("(?<![a-z])(district|zila|zilla|zela|dist|division|div|bibhag|",
                          "hill district|city corporation|city corp|city)(?![a-z])")
# Bangla zela (district) and bibhag (division).
.admin_words_bn_re <- "\u099c\u09c7\u09b2\u09be|\u09ac\u09bf\u09ad\u09be\u0997"
# Words removed from thana names: "Thana", "P.S." and also "Upazila", because
# most thanas are written as upazilas ("Savar Upazila").
.local_words_re <- paste0("(?<![a-z])(upazila|upazilla|upozila|upozilla|upojela|upojila|",
                          "thana|police station|p[.]? ?s[.]?|metropolitan)(?![a-z])")
# Bangla upojela and thana.
.local_words_bn_re <- "\u0989\u09aa\u099c\u09c7\u09b2\u09be|\u09a5\u09be\u09a8\u09be"
# Post office types removed from post office names ("Mirpur TSO" -> "mirpur").
.office_words_re <- "(?<![a-z])(gpo|ho|upo|so|tso|edso|edbo|bo|tsc|eddo)(?![a-z])"

# Fuzzy matching limits. Targets shorter than this are matched exactly only:
# abbreviations such as "ctg" would otherwise attract short words ("city").
.fuzzy_min_target <- 4L

#' Matching key of a place name
#'
#' English names become lower-case ASCII letters with suffixes such as
#' "District" removed and "-gonj" read as "-ganj". Bangla names stay in
#' Bengali script, with the same suffixes removed and the usual typing
#' variants made equal (long and short i and u, nukta letters, joiners, and
#' the two na and two sa letters).
#' @param x Character vector.
#' @param local Also remove "Thana", "P.S.", "Upazila" (for thanas).
#' @param post Also remove post office types such as "TSO" and "SO".
#' @return Character vector of keys; `NA` for missing or empty names.
#' @noRd
.name_key <- function(x, local = FALSE, post = FALSE) {
  s <- tolower(.squish(.as_utf8_chr(x)))
  if (post) s <- gsub(.office_words_re, " ", s, perl = TRUE)
  if (local) {
    s <- gsub(.local_words_re, " ", s, perl = TRUE)
    s <- gsub(.local_words_bn_re, " ", s, perl = TRUE)
  }
  s <- .bn_key(s)
  s <- chartr("\u09a3\u09b7", "\u09a8\u09b8", s)
  # Apostrophes are dropped ("Cox's" -> "coxs"); other punctuation separates
  # words, so "Dhaka-Zila" and "Dhaka(District)" lose the suffix.
  s <- gsub("['\u2019]", "", s, perl = TRUE)
  s <- gsub("[^a-z \u0980-\u09ff]", " ", s, perl = TRUE)
  s <- gsub(.admin_words_re, " ", s, perl = TRUE)
  s <- gsub(.admin_words_bn_re, " ", s, perl = TRUE)
  s <- gsub("[^a-z\u0980-\u09ff]", "", s, perl = TRUE)
  s <- gsub("gonj", "ganj", s, fixed = TRUE)
  s[s == ""] <- NA_character_
  s
}

# Reference table of each level: one row per place, with `name` and
# `district` (NA for divisions). Ids used below are row numbers of it.
.ref <- function(level) {
  switch(level,
    district = data.frame(name = .tab_districts$district, district = .tab_districts$district,
                          stringsAsFactors = FALSE),
    division = data.frame(name = .tab_divisions$division, district = NA_character_,
                          stringsAsFactors = FALSE),
    thana = data.frame(name = .tab_thanas$thana, district = .tab_thanas$district,
                       stringsAsFactors = FALSE))
}

# Cache of the lookup tables (they are built on first use).
.cache <- new.env(parent = emptyenv())

#' All known spellings of one level
#'
#' @param level "district", "division" or "thana".
#' @return A list: `tab`, a data frame with `key`, `id` (row of `.ref(level)`)
#'   and `kind` (which list of spellings the key comes from, most official
#'   first), and `by_key`, the rows of `tab` for each key.
#' @noRd
.lookup <- function(level) {
  if (!is.null(.cache[[level]])) return(.cache[[level]])
  tab <- switch(level,
    district = {
      d <- .tab_districts
      id <- seq_len(nrow(d))
      al <- .tab_dist_alias
      data.frame(
        key = .name_key(c(d$district, d$district_legacy, d$district_map, d$district_bn, al$alias)),
        id = c(id, id, id, id, match(al$district, d$district)),
        kind = rep(c("official", "old spelling", "boundary-file spelling", "Bangla", "variant"),
                   c(rep(nrow(d), 4L), nrow(al))),
        stringsAsFactors = FALSE)
    },
    division = {
      v <- .tab_divisions
      id <- seq_len(nrow(v))
      al <- .tab_div_alias
      # Every division is named after its headquarters district, so variant
      # spellings of those districts (for example "Rongpur") apply too.
      hq <- .tab_dist_alias[.tab_dist_alias$district %in% v$division, ]
      data.frame(
        key = .name_key(c(v$division, v$division_legacy, v$division_bn, al$alias, hq$alias)),
        id = c(id, id, id, match(al$division, v$division), match(hq$district, v$division)),
        kind = rep(c("official", "old spelling", "Bangla", "variant", "variant"),
                   c(rep(nrow(v), 3L), nrow(al), nrow(hq))),
        stringsAsFactors = FALSE)
    },
    thana = {
      th <- .tab_thanas
      id <- seq_len(nrow(th))
      al <- .tab_thana_alias
      within <- th$type == "within"
      # A "within" thana has no area of its own in the boundary files; its
      # map name is that of the area it lies in, so it is not a spelling.
      has_map <- th$map_name != "" & !within
      sadar <- th$type == "rural" & grepl(" Sadar$", th$thana)
      leg <- .tab_districts$district_legacy[match(th$district, .tab_districts$district)]
      tab <- data.frame(
        key = .name_key(c(th$thana, th$map_name[has_map], al$alias,
                          paste(leg[sadar], "Sadar"), rep("sadar", sum(sadar))), local = TRUE),
        id = c(id, id[has_map], match(paste(al$district, al$thana), paste(th$district, th$thana)),
               id[sadar], id[sadar]),
        kind = c(rep("official", nrow(th)), rep("boundary-file spelling", sum(has_map)), al$kind,
                 rep("old district spelling", sum(sadar)), rep("sadar", sum(sadar))),
        stringsAsFactors = FALSE)
      # A name of a "within" thana means that thana, even where the post
      # office list uses it for the larger area ("Fatulla").
      own <- tab$key[!is.na(tab$id) & within[tab$id]]
      tab[(!is.na(tab$id) & within[tab$id]) | !tab$key %in% own, ]
    })
  tab <- tab[!is.na(tab$key) & !is.na(tab$id), ]
  # Keep the first (most official) kind for each key and place.
  tab <- tab[!duplicated(paste(tab$key, tab$id)), ]
  rownames(tab) <- NULL
  out <- list(tab = tab, by_key = split(seq_len(nrow(tab)), tab$key))
  .cache[[level]] <- out
  out
}

#' Closest known spelling, for typing errors
#'
#' Accepts a match only when the closest place is unique and the number of
#' edits is at most `max_distance` and at most a third of the length of the
#' key (one edit for 3 to 5 letters, two for 6 to 8).
#' @param key One key.
#' @param pool Rows of a lookup table to choose from.
#' @param names Place name of each id, to decide whether the closest
#'   spellings all name the same place.
#' @return The ids of the closest place (several when one name is used in
#'   several districts), or an empty vector.
#' @noRd
.fuzzy_pick <- function(key, pool, names, max_distance) {
  pool <- pool[nchar(pool$key) >= .fuzzy_min_target & pool$kind != "sadar", , drop = FALSE]
  if (!nrow(pool)) return(integer(0))
  d <- utils::adist(key, pool$key)[1L, ]
  m <- min(d)
  ids <- unique(pool$id[d == m])
  if (m <= min(max_distance, nchar(key) %/% 3L) && length(unique(names[ids])) == 1L) ids else integer(0)
}

#' Match names of one level
#'
#' @param x Character vector of names.
#' @param level "district", "division" or "thana".
#' @param district Standard district names (same length as `x`, `NA` where
#'   unknown) for thanas; `NULL` otherwise.
#' @return A data frame, one row per value: `id` (row of `.ref(level)`, or
#'   `NA`), `match` ("exact", "variant", "typing error", "ambiguous", "not
#'   found", "missing"), `how` (the kind of spelling that matched),
#'   `name` (the standard name, also for an ambiguous name used in several
#'   districts) and `candidates`.
#' @noRd
.match_names <- function(x, level, district = NULL, fuzzy = TRUE, max_distance = 2) {
  local <- level == "thana"
  key <- .name_key(x, local = local)
  dist <- if (is.null(district)) rep(NA_character_, length(key)) else district
  lk <- .lookup(level)
  ref <- .ref(level)
  combo <- paste(key, dist, sep = "\r")
  first <- which(!duplicated(combo))
  res <- lapply(first, function(i) {
    k <- key[i]
    d <- dist[i]
    out <- list(id = NA_integer_, match = "not found", how = NA_character_,
                name = NA_character_, candidates = NA_character_)
    if (is.na(k)) {
      if (!.nonblank(x[i])) out$match <- "missing"
      return(out)
    }
    rows <- lk$by_key[[k]]
    hit <- lk$tab[if (is.null(rows)) integer(0) else rows, , drop = FALSE]
    if (!is.na(d)) {
      hit <- hit[ref$district[hit$id] %in% d, , drop = FALSE]
    } else {
      hit <- hit[hit$kind != "sadar", , drop = FALSE]  # "Sadar" alone needs a district
    }
    ids <- unique(hit$id)
    how <- if (nrow(hit)) hit$kind[1L] else NA_character_
    if (!length(ids) && local && k == "sadar" && is.na(d)) {
      out$match <- "ambiguous"
      out$candidates <- "\"Sadar\": give the district"
      return(out)
    }
    if (!length(ids) && fuzzy) {
      pool <- lk$tab
      if (!is.na(d)) pool <- pool[ref$district[pool$id] %in% d, , drop = FALSE]
      ids <- .fuzzy_pick(k, pool, ref$name, max_distance)
      how <- "typing error"
    }
    if (!length(ids)) return(out)
    if (length(unique(ref$name[ids])) == 1L) out$name <- ref$name[ids[1L]]
    if (length(ids) > 1L) {
      out$match <- "ambiguous"
      out$candidates <- .fmt_places(ref$name[ids], ref$district[ids])
      out$how <- how
      return(out)
    }
    out$id <- ids
    out$how <- how
    out$match <- if (how == "official") "exact" else if (how == "typing error") "typing error" else "variant"
    out
  })
  idx <- match(combo, combo[first])
  data.frame(
    id = vapply(res, function(r) as.integer(r$id), 1L)[idx],
    match = vapply(res, `[[`, "", "match")[idx],
    how = vapply(res, `[[`, "", "how")[idx],
    name = vapply(res, `[[`, "", "name")[idx],
    candidates = vapply(res, `[[`, "", "candidates")[idx],
    stringsAsFactors = FALSE
  )
}

# Why each kind of spelling is reported, and which `report` setting lists it.
.change_why <- c(
  "typing error" = "typing error, corrected by fuzzy matching",
  "variant" = "not the standard spelling",
  "postal spelling" = "spelling used by the post office",
  "boundary-file spelling" = "spelling used in boundary (map) files",
  "old district spelling" = "Sadar with the old district spelling",
  "sadar" = "\"Sadar\" read with the district given",
  "old spelling" = "spelling used before 2018",
  "Bangla" = "written in Bangla"
)
.change_fixes <- c("typing error", "variant", "postal spelling", "boundary-file spelling",
                   "old district spelling", "sadar")

#' List changed names in a message
#'
#' @param input Values as given.
#' @param output Values returned.
#' @param how Kind of spelling that matched each value (see `.change_why`).
#' @param level Level name used in the message.
#' @param report "fixes" (variants and typing errors), "all" (also old
#'   spellings and Bangla) or "none".
#' @param label Optional prefix, such as the column name in bd_clean().
#' @noRd
.report_changes <- function(input, output, how, level, report, label = NULL) {
  kinds <- switch(report, none = character(0), fixes = .change_fixes,
                  all = names(.change_why))
  typed <- .squish(.as_utf8_chr(input))
  # Values that differ only in case, spaces or a suffix such as "District"
  # are not reported.
  hit <- !is.na(how) & how %in% kinds & !is.na(output) &
    !(.name_key(typed, local = TRUE) %in% .name_key(output, local = TRUE))
  if (!any(hit)) return(invisible(NULL))
  pair <- paste(typed[hit], output[hit], how[hit], sep = "\r")
  first <- !duplicated(pair)
  ch <- data.frame(typed = typed[hit][first], output = output[hit][first],
                   how = how[hit][first], n = tabulate(match(pair, pair[first])),
                   stringsAsFactors = FALSE)
  ch <- ch[order(match(ch$how, names(.change_why)), -ch$n, ch$typed), ]
  lines <- sprintf("  \"%s\" -> \"%s\" (%s%s)", ch$typed, ch$output, .change_why[ch$how],
                   ifelse(ch$n > 1L, paste0("; ", ch$n, " values"), ""))
  if (length(lines) > 20L) {
    lines <- c(lines[1:20], sprintf("  ... and %d more", length(lines) - 20L))
  }
  n <- sum(hit)
  head <- sprintf("%s%d %s %s not written as the standard %s name and %s changed:",
                  if (is.null(label)) "" else paste0(label, ": "), n,
                  if (n == 1L) "value" else "values", if (n == 1L) "was" else "were",
                  level, if (n == 1L) "was" else "were")
  tail <- if (any(ch$how == "typing error")) "Typing-error corrections are guesses: check them by hand."
  message(paste(c(head, lines, tail), collapse = "\n"))
  invisible(NULL)
}

# Standard district names of a `district` argument: NULL, one value for all
# of `x`, or one value per element of `x`.
.district_arg <- function(district, n) {
  if (is.null(district)) return(NULL)
  m <- .match_names(district, "district")
  d <- .tab_districts$district[m$id]
  bad <- .nonblank(district) & is.na(d)
  if (any(bad)) {
    warning(sprintf("%d value%s of `district` could not be matched and %s ignored: %s",
                    sum(bad), if (sum(bad) == 1L) "" else "s",
                    if (sum(bad) == 1L) "was" else "were",
                    paste(utils::head(unique(.squish(.as_utf8_chr(district[bad]))), 5L),
                          collapse = ", ")), call. = FALSE)
  }
  if (length(d) == 1L) d <- rep(d, n)
  if (length(d) != n) {
    stop("`district` must have length 1 or the same length as `x`.", call. = FALSE)
  }
  d
}
