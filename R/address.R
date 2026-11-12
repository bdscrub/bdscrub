# Location from free-text addresses --------------------------------------------
#
# bd_address() reads each distinct address once, in six steps:
#   1. .find_postcode()    a 4-digit number that is a known post code;
#   2. .find_places()      district, division and thana names among
#                          sequences of 1 to 4 words;
#   3. .find_post_office() a post office named after "P.O." (when there is
#                          no post code);
#   4. .pick_district()    the district, from names, thanas and post code;
#   5. .pick_thana()       the thana inside that district;
#   6. .check_address()    division and post code against the district, and
#                          the status.

# Words that show that a place name is part of a street or institution name
# ("Dhaka Road", "Jessore Road", "Mymensingh Medical College"). The last two
# are Bangla road and sarak.
.street_words <- c("road", "rd", "lane", "ln", "street", "st", "avenue", "ave",
                   "highway", "bypass", "sarak", "sorok", "more", "mor", "medical",
                   "college", "university", "cantonment", "cantt", "bus", "stand",
                   "\u09b0\u09cb\u09a1", "\u09b8\u09dc\u0995")
# Words that mark a division (the last is Bangla bibhag).
.division_words <- c("division", "div", "bibhag", "\u09ac\u09bf\u09ad\u09be\u0997")
# Words before a post office name (the last is Bangla dakghar).
.post_office_words <- c("po", "post", "dakghar", "\u09a1\u09be\u0995\u0998\u09b0")
# Words before a post code.
.postcode_words <- c("code", "postcode", "pc", "zip", "post", "dhaka", "ctg")
# Longest place name looked for, in words ("Cox s Bazar Sadar" has four).
.max_place_words <- 4L

# Scores used to choose between several district names in one address.
# A thana of the district named elsewhere, or a post code in it, is strong
# evidence; being named last is weak evidence (addresses usually end with the
# district); a district name that is also a thana of another district named
# in the address is most likely that thana ("Faridpur, Pabna": Faridpur
# thana in Pabna district).
.score_thana <- 2
.score_postcode <- 2
.score_last <- 0.5
.score_is_thana <- -3

# Words of each address: lower case, English digits, "P.O." as "po" and
# "P.S." as "ps", any punctuation as a space. Bangla letters are kept.
.addr_words <- function(s) {
  s <- tolower(.bn2en(.squish(s)))
  s <- gsub("[\u200c\u200d]", "", s, perl = TRUE)
  s <- gsub("(?<![a-z])p[ ./]*o(?![a-z])", " po ", s, perl = TRUE)
  s <- gsub("(?<![a-z])p[ ./]*s(?![a-z])", " ps ", s, perl = TRUE)
  s <- gsub("['\u2019]", "", s, perl = TRUE)
  s <- gsub("[^a-z0-9\u0980-\u09e5\u09f0-\u09ff]+", " ", s, perl = TRUE)
  lapply(strsplit(trimws(s), " ", fixed = TRUE), function(w) w[w != ""])
}

# All sequences of 1 to 4 words of all addresses, with their keys, built at
# once as plain vectors (one data frame per address would be slow).
# a = address, s and e = first and last word.
.addr_spans <- function(words) {
  n <- lengths(words)
  a <- s <- txt <- vector("list", .max_place_words)
  for (len in seq_len(.max_place_words)) {
    ok <- which(n >= len)
    cnt <- n[ok] - len + 1L
    a[[len]] <- rep(ok, cnt)
    s[[len]] <- sequence(cnt)
    flat <- unlist(words[ok], use.names = FALSE)
    off <- rep(cumsum(c(0L, n[ok]))[seq_along(ok)], cnt)
    parts <- lapply(0:(len - 1L), function(k) flat[off + s[[len]] + k])
    txt[[len]] <- if (length(parts[[1L]])) do.call(paste, parts) else character(0)
  }
  len <- rep(seq_len(.max_place_words), lengths(a))
  sp <- data.frame(a = unlist(a), s = unlist(s), txt = unlist(txt), stringsAsFactors = FALSE)
  sp$e <- sp$s + len - 1L
  sp <- sp[order(sp$a, -(sp$e - sp$s), sp$s), , drop = FALSE]
  sp <- sp[!grepl("^[0-9 ]+$", sp$txt), , drop = FALSE]
  sp$key <- .name_key(sp$txt)
  sp$lkey <- .name_key(sp$txt, local = TRUE)
  sp
}

# Lookup tables for addresses (built once).
.addr_tables <- function() {
  if (!is.null(.cache$addr)) return(.cache$addr)
  first_name <- function(level, names) {
    tab <- .lookup(level)$tab
    tab <- tab[!duplicated(tab$key), ]
    stats::setNames(names[tab$id], tab$key)
  }
  th <- .tab_thanas
  lk <- .lookup("thana")$tab
  lk <- lk[lk$kind != "sadar", ]
  pc <- .tab_postcodes
  pc$po_key <- .name_key(pc$post_office, post = TRUE)
  whole <- which(th$type != "within")
  pc$thana_id <- whole[match(paste(pc$district, pc$thana),
                             paste(th$district, th$thana)[whole])]
  out <- list(district = first_name("district", .tab_districts$district),
              division = first_name("division", .tab_divisions$division),
              thana_ids = lapply(split(lk$id, lk$key), unique),
              pc = pc)
  .cache$addr <- out
  out
}

# 1. Post code: a known 4-digit post code. When there are several, prefer one
# after a word such as "code" or at the end of the address.
.find_postcode <- function(w, t) {
  num <- which(grepl("^[0-9]{4}$", w) & w %in% t$pc$postcode)
  if (!length(num)) return(NULL)
  prev <- c("", w)[num]
  marked <- num[prev %in% .postcode_words | num == length(w)]
  k <- if (length(marked)) marked[length(marked)] else num[length(num)]
  t$pc[t$pc$postcode == w[k], ][1L, ]
}

# 2. Place names: one hit per word sequence that names a district,
# division or thana.
.find_places <- function(w, sp, t) {
  n <- length(w)
  hits <- list()
  add <- function(...) hits[[length(hits) + 1L]] <<- list(...)
  for (r in seq_len(nrow(sp))) {
    s <- sp$s[r]
    e <- sp$e[r]
    key <- sp$key[r]
    lkey <- sp$lkey[r]
    nxt <- if (e < n) w[e + 1L] else ""
    if (nxt %in% .street_words) next
    is_div <- nxt %in% .division_words || any(w[s:e] %in% .division_words)
    if (!is.na(key) && nchar(key) >= 3L) {
      if (!is_div && !is.na(t$district[key])) {
        add(kind = "district", s = s, e = e, value = unname(t$district[key]))
      }
      if (is_div && !is.na(t$division[key])) {
        add(kind = "division", s = s, e = e, value = unname(t$division[key]))
      }
    }
    if (!is.na(lkey) && nchar(lkey) >= .fuzzy_min_target && length(t$thana_ids[[lkey]])) {
      add(kind = "thana", s = s, e = e, ids = t$thana_ids[[lkey]])
    }
    if (s == e && w[s] == "sadar") add(kind = "sadar", s = s, e = e)
  }
  # A shorter hit inside a longer hit of the same kind is dropped
  # ("cox s bazar" is the district, not a "bazar" part).
  inside <- vapply(hits, function(h) any(vapply(hits, function(g) {
    g$kind == h$kind && g$s <= h$s && g$e >= h$e && (g$e - g$s) > (h$e - h$s)
  }, TRUE)), TRUE)
  hits <- hits[!inside]
  # In the order they are written, so that "the last one named" is last.
  hits[order(vapply(hits, `[[`, 0, "s"), vapply(hits, `[[`, 0, "e"))]
}

# 3. Post office after "P.O." when there is no post code.
.find_post_office <- function(w, t) {
  n <- length(w)
  for (p in which(w %in% .post_office_words)) {
    for (len in 3:1) {
      if (p + len > n) next
      name <- paste(w[(p + 1L):(p + len)], collapse = " ")
      k <- .name_key(name)
      r <- t$pc[!is.na(k) & t$pc$po_key %in% k, ]
      if (nrow(r) == 1L) {
        attr(r, "note") <- paste0("post code from the post office name \"", name, "\"")
        return(r)
      }
    }
  }
  NULL
}

.thana_districts <- function(h) unique(.tab_thanas$district[h$ids])
.same_span <- function(a, b) a$s == b$s && a$e == b$e

# 4. District: the best-scoring district name, else the only district of the
# thana names, else the district of the post code.
.pick_district <- function(dh, th_hits, pc_dist) {
  note <- character(0)
  dist <- NA_character_
  if (length(dh)) {
    cand <- unique(vapply(dh, `[[`, "", "value"))
    last_e <- max(vapply(dh, `[[`, 0, "e"))
    score <- vapply(cand, function(d) {
      own <- dh[vapply(dh, function(h) h$value == d, TRUE)]
      th_elsewhere <- any(vapply(th_hits, function(h) d %in% .thana_districts(h) &&
                                   !any(vapply(own, .same_span, TRUE, b = h)), TRUE))
      last <- max(vapply(own, `[[`, 0, "e")) == last_e
      is_th <- any(vapply(th_hits, function(h) any(vapply(own, .same_span, TRUE, b = h)) &&
                            any(.thana_districts(h) %in% setdiff(cand, d)), TRUE))
      .score_thana * th_elsewhere + .score_postcode * isTRUE(pc_dist == d) +
        .score_last * last + .score_is_thana * is_th
    }, 0)
    dist <- cand[which.max(score)]
    if (length(cand) > 1L) {
      note <- paste0("several district names (", paste(cand, collapse = ", "), "); used ", dist)
    }
  }
  own_span <- function(h) any(vapply(dh, function(d) d$value == dist && .same_span(d, h), TRUE))
  th2 <- Filter(function(h) is.na(dist) || !own_span(h), th_hits)
  if (is.na(dist) && length(th2)) {
    ds <- unique(unlist(lapply(th2, .thana_districts)))
    if (length(ds) == 1L) {
      dist <- ds
      note <- c(note, "district from the thana name")
    } else if (!is.na(pc_dist) && pc_dist %in% ds) {
      dist <- pc_dist
      note <- c(note, "district from the post code")
    }
  }
  if (is.na(dist) && !is.na(pc_dist)) {
    dist <- pc_dist
    note <- c(note, "district from the post code only")
  }
  list(district = dist, thana_hits = th2, note = note)
}

# 5. Thana inside the district: the last one named, else "Sadar", else the
# thana of the post code.
.pick_thana <- function(dist, th2, has_sadar, pc_row) {
  th <- .tab_thanas
  note <- character(0)
  id <- NA_integer_
  if (is.na(dist)) {
    if (length(th2)) {
      ids <- unique(unlist(lapply(th2, `[[`, "ids")))
      note <- paste0("thana name used in several districts: ",
                     .fmt_places(th$thana[ids], th$district[ids]))
    }
    return(list(id = id, note = note, ambiguous = length(th2) > 0L))
  }
  ins <- Filter(function(h) dist %in% .thana_districts(h), th2)
  if (length(ins)) {
    ids_in <- vapply(ins, function(h) h$ids[th$district[h$ids] == dist][1L], 1L)
    id <- ids_in[length(ids_in)]
    # A thana inside the area of another thana that is named too
    # ("Ashulia, Savar" or "Savar, Ashulia"): the smaller one is meant.
    inner <- ids_in[th$type[ids_in] == "within" & th$area[ids_in] %in% th$thana[ids_in]]
    if (length(inner)) id <- inner[length(inner)]
  } else if (has_sadar) {
    j <- which(th$district == dist & th$type == "rural" & grepl(" Sadar$", th$thana))
    if (length(j) == 1L) id <- j
  }
  if (is.na(id) && length(th2)) {
    # Village names often equal thana names elsewhere, so this is a note.
    ids <- unique(unlist(lapply(th2, `[[`, "ids")))
    note <- c(note, paste0("names a thana of another district: ",
                           .fmt_places(th$thana[ids], th$district[ids])))
  }
  if (is.na(id) && !is.null(pc_row) && !is.na(pc_row$thana_id) &&
      isTRUE(pc_row$district == dist)) {
    id <- pc_row$thana_id
    note <- c(note, "thana from the post code")
  }
  list(id = id, note = note, ambiguous = FALSE)
}

# One address. `w` its words, `sp` its word sequences that name a place,
# `hint` the district given by the user (or NA).
.read_address <- function(addr, w, sp, hint, t) {
  res <- list(postcode = NA_character_, post_office = NA_character_,
              thana = NA_character_, district = NA_character_, division = NA_character_,
              status = "not found", note = character(0))
  if (is.na(addr) || !length(w)) {
    res$status <- "missing"
    return(res)
  }
  pc_row <- .find_postcode(w, t)
  hits <- .find_places(w, sp, t)
  kinds <- vapply(hits, `[[`, "", "kind")
  if (is.null(pc_row)) {
    pc_row <- .find_post_office(w, t)
    if (!is.null(pc_row)) res$note <- c(res$note, attr(pc_row, "note"))
  }
  if (!is.null(pc_row)) {
    res$postcode <- pc_row$postcode
    res$post_office <- pc_row$post_office
  }
  pc_dist <- if (is.null(pc_row)) NA_character_ else pc_row$district

  dh <- hits[kinds == "district"]
  d <- .pick_district(dh, hits[kinds == "thana"], pc_dist)
  dist <- d$district
  res$note <- c(res$note, d$note)
  th2 <- d$thana_hits
  if (!is.na(hint)) {
    # District names that are also a thana of the given district
    # ("Faridpur" in Pabna) do not contradict it.
    th_in_hint <- Filter(function(h) hint %in% .thana_districts(h), hits[kinds == "thana"])
    dh_other <- Filter(function(g) !any(vapply(th_in_hint, .same_span, TRUE, a = g)), dh)
    named <- unique(vapply(dh_other, `[[`, "", "value"))
    if (length(named) && !hint %in% named) {
      res$note <- c(res$note, paste0("the address names ", paste(named, collapse = ", "),
                                     ", but the district given is ", hint))
      res$status <- "conflict"
    }
    if (!isTRUE(dist == hint)) {
      dist <- hint
      th2 <- Filter(function(h) !any(vapply(dh, function(g) g$value == hint &&
                                               .same_span(g, h), TRUE)), hits[kinds == "thana"])
    }
  }
  p <- .pick_thana(dist, th2, any(kinds == "sadar"), pc_row)
  res$note <- c(res$note, p$note)
  if (p$ambiguous) res$status <- "ambiguous"
  if (!is.na(p$id)) {
    res$thana <- .tab_thanas$thana[p$id]
  }

  # 6. Division and consistency.
  vh <- hits[kinds == "division"]
  if (!is.na(dist)) {
    res$district <- dist
    res$division <- .tab_districts$division[match(dist, .tab_districts$district)]
    if (length(vh) && vh[[length(vh)]]$value != res$division) {
      res$note <- c(res$note, paste0("the address names ", vh[[length(vh)]]$value,
                                     " division, but ", dist, " is in ", res$division))
      res$status <- "conflict"
    }
  } else if (length(vh)) {
    res$division <- vh[[length(vh)]]$value
  }
  if (!is.na(pc_dist) && !is.na(dist) && pc_dist != dist) {
    res$note <- c(res$note, paste0("post code ", res$postcode, " (", res$post_office,
                                   ") is in ", pc_dist, ", not ", dist))
    res$status <- "conflict"
  }
  if (res$status == "not found") {
    res$status <- if (!is.na(p$id)) "complete" else if (!is.na(dist)) "district only" else
      if (!is.na(res$division)) "division only" else "not found"
  }
  res
}

#' Read the location from free-text addresses
#'
#' Finds the post code, post office, thana, district and division in
#' addresses written in many ways: "Vill: Char Kalia, PO: Kalia, Narail -
#' 7520", "House 12, Road 5, Mirpur 10, Dhaka-1216", "Sarail, B. Baria",
#' Bangla district names, old and new spellings. Every value comes from the
#' package's lists of places and post offices; nothing is guessed. When an
#' address is ambiguous or contradicts itself, `status` and `note` say so.
#'
#' How an address is read:
#' 1. **Post code**: a 4-digit number that is a known post code (preferring
#'    one after "code" or at the end).
#' 2. **Place names**: every sequence of one to four words is looked up as a
#'    district, division and thana name (upazila names included), in all
#'    known spellings. A name followed by "Road", "College", "Medical" and
#'    the like is part of a street or institution name and is skipped
#'    ("Dhaka Road, Mymensingh").
#' 3. **Post office**: if there is no post code, a post office name after
#'    "P.O." gives the post code.
#' 4. **District**: when several district names occur, the one with a thana
#'    or the post code in it, or named last, is used; a district name that
#'    is also a thana of another district named in the address is read as
#'    that thana ("Faridpur, Pabna" is Faridpur thana in Pabna). Without a
#'    district name, the district comes from the thana name (when only one
#'    district has it) or from the post code.
#' 5. **Thana** in that district: the last one named, else "Sadar" with the
#'    district, else the thana of the post code.
#' 6. **Checks**: a division or post code that does not fit the district
#'    makes the status "conflict".
#'
#' Fuzzy matching is not used here: in a whole address, many words would
#' match some place by chance.
#'
#' @param x A character vector of addresses.
#' @param district Optional: the district of each address, when the data have
#'   it in a separate column (one value for all addresses, or one per
#'   address, in any spelling [bd_place()] understands). It is used as the
#'   district, settles thana names used in several districts, and an address
#'   that names a different district gets the status "conflict".
#' @return A tibble (a data frame if tibble is not installed) with one row
#'   per address:
#'   \describe{
#'     \item{input}{the address as given.}
#'     \item{postcode, post_office}{the post code and its post office.}
#'     \item{thana}{the thana: a rural thana (same name and area as the
#'       upazila), a city thana (Mirpur, Double Mooring) or a thana inside a
#'       larger rural thana (Ashulia). See [bd_thanas].}
#'     \item{district, division}{the district and its division.}
#'     \item{status}{"complete" (thana and district found), "district only",
#'       "division only", "ambiguous" (a thana name used in several
#'       districts, no district), "conflict" (parts of the address
#'       contradict each other), "not found" or "missing".}
#'     \item{note}{how the result was reached when it was not direct, and
#'       what contradicts what.}
#'   }
#' @seealso [bd_place()] for single place names, [bd_postcodes] and
#'   [bd_thanas] for the lists used.
#' @examples
#' bd_address(c("House 12, Road 5, Mirpur 10, Dhaka-1216",
#'               "Vill: Char Kalia, P.O: Kalia, Narail - 7520",
#'               "Kaliganj", "Kaliganj, Gazipur", "Sarail, B. Baria",
#'               "Ashulia, Dhaka", "Mirpur, Dhaka-7030", "Dhaka Road, Mymensingh"))
#'
#' # A separate district column settles "Kaliganj"
#' bd_address(c("Kaliganj", "Bazar road, Kaliganj"), district = c("Jhenaidah", "Satkhira"))
#' @export
bd_address <- function(x, district = NULL) {
  input <- .as_utf8_chr(x)
  hint <- .district_arg(district, length(input))
  if (is.null(hint)) hint <- rep(NA_character_, length(input))
  combo <- paste(input, hint, sep = "\r")
  first <- which(!duplicated(combo))
  ux <- input[first]
  words <- .addr_words(ux)
  # Missing-value markers ("N/A", "-") are missing addresses.
  words[is.na(ux) | is.na(bd_missing(ux))] <- list(character(0))
  t <- .addr_tables()
  sp <- .addr_spans(words)
  # Only word sequences that can name a place are looked at further.
  keep <- (!is.na(sp$key) & (sp$key %in% names(t$district) | sp$key %in% names(t$division))) |
    (!is.na(sp$lkey) & sp$lkey %in% names(t$thana_ids)) | sp$txt == "sadar"
  by_addr <- split(sp[keep, , drop = FALSE], factor(sp$a[keep], levels = seq_along(ux)))
  r <- lapply(seq_along(ux), function(k) {
    .read_address(ux[k], words[[k]], by_addr[[k]], hint[first[k]], t)
  })
  get <- function(field) vapply(r, `[[`, "", field)
  out <- data.frame(
    input = ux,
    postcode = get("postcode"), post_office = get("post_office"),
    thana = get("thana"),
    district = get("district"), division = get("division"),
    status = get("status"),
    note = vapply(r, function(z) if (length(z$note)) paste(z$note, collapse = "; ") else NA_character_, ""),
    stringsAsFactors = FALSE
  )
  out <- out[match(combo, combo[first]), , drop = FALSE]
  out$input <- input
  .as_tbl(out)
}
