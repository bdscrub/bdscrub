# District, division and thana names --------------------------------------------

# District codes: two-digit BBS codes ("26" or 26), pcodes ("BD3026") and
# ISO 3166-2 codes ("BD-13"). Returns the row of .tab_districts or NA.
.district_code <- function(input) {
  tab <- .tab_districts
  s <- toupper(gsub("[[:space:]]", "", input))
  idx <- rep(NA_integer_, length(s))
  bbs <- !is.na(s) & grepl("^[0-9]{1,2}$", s)
  idx[bbs] <- match(sprintf("%02d", as.integer(s[bbs])), tab$bbs_code)
  pc <- !is.na(s) & grepl("^BD[0-9]{4}$", s)
  idx[pc] <- match(s[pc], tab$pcode)
  iso <- !is.na(s) & grepl("^BD-[0-9]{1,2}$", s)
  idx[iso] <- match(sprintf("BD-%02d", as.integer(sub("BD-", "", s[iso]))), tab$iso_code)
  attr(idx, "is_code") <- bbs | pc | iso
  idx
}

# The name of each matched place in the requested style.
.styled <- function(id, level, style) {
  if (level == "district") {
    col <- switch(style, current = "district", legacy = "district_legacy",
                  bangla = "district_bn", map = "district_map")
    return(.tab_districts[[col]][id])
  }
  if (level == "division") {
    col <- switch(style, current = "division", legacy = , map = "division_legacy",
                  bangla = "division_bn")
    return(.tab_divisions[[col]][id])
  }
  out <- switch(style, current = .tab_thanas$thana[id], bangla = rep(NA_character_, length(id)),
                .tab_thanas$map_name[id])
  out[out %in% ""] <- NA_character_
  out
}

#' Standardise district, division and thana names
#'
#' Looks up place names written in any of their known spellings and returns
#' one standard spelling, so that data from different sources can be joined
#' with each other and with boundary (map) files.
#'
#' **Matching.** Upper and lower case, spaces, punctuation and words such as
#' "District", "Zila", "Division", "Thana", "P.S.", "Upazila" (also in
#' Bangla) do not matter, and "-gonj" is read as "-ganj". Each name is then
#' looked up among:
#' - the official spelling (the 2018 spellings for districts and divisions:
#'   Chattogram, Cumilla, Jashore, Barishal, Bogura ...);
#' - the spelling used before 2018 (Chittagong, Comilla, Jessore ...);
#' - the spelling in the boundary files most maps use (HDX/BBS, for example
#'   "Brahamanbaria", "Nawabganj", "Maulvibazar");
#' - the Bangla name (districts and divisions);
#' - spellings found in survey and administrative data ("Netrokona",
#'   "Jhalokati", "Nababganj") and, for thanas, the spellings of the post
#'   office list.
#'
#' For Bangla text, long and short i and u, letters with a nukta typed as one
#' or two characters, the two na letters and the two sa letters are treated
#' as equal, because these are the usual ways one name is typed
#' differently.
#'
#' **Typing errors (`fuzzy = TRUE`, the default).** A name that is still not
#' found is compared with all known spellings by edit distance
#' ([utils::adist()]). It is accepted only when one place is closest and
#' the number of edits is at most `max_distance` and at most a third of the
#' length of the name (one edit for names of 3 to 5 letters). Spellings of
#' three letters or fewer, such as "Ctg", are matched exactly only. Every
#' correction is listed in the message, so it can be checked.
#'
#' **Thanas.** Below the district, bdscrub works with thanas (police
#' stations). A rural thana has the same name and area as the upazila of the
#' same name, so an upazila column can be cleaned with `level = "thana"` too
#' ("Savar Upazila" gives "Savar"). [bd_thanas] has three types: "rural"
#' (503), "city" (61 thanas of the city corporations, for example Mirpur and
#' Double Mooring) and "within" (6 thanas inside the area of a larger rural
#' thana, for example Ashulia in the Savar area). Many thana names are used
#' in several districts (Kaliganj is in four, there is a Kotwali in most
#' cities). Give `district` when you know it; without it such names are
#' "ambiguous" and become `NA`. "Sadar" alone is understood only with a
#' district ("Sadar" in Feni is Feni Sadar).
#'
#' **What is changed.** Names that cannot be matched become `NA`; nothing is
#' guessed silently. When the function finishes it prints a message listing
#' the values that were not written in the standard spelling and were
#' changed, for example `"Nababganj" -> "Chapai Nawabganj" (not the
#' standard spelling)`. Use `report` to choose what is listed, or
#' [suppressMessages()] to silence it.
#'
#' @param x A character vector of names. For `level = "district"`, district
#'   codes are accepted too: two-digit BBS codes ("26" or 26), pcodes
#'   ("BD3026") and ISO 3166-2 codes ("BD-13"). BBS and ISO numbers differ
#'   (BBS 26 is Dhaka, ISO BD-26 is Kishoreganj), so plain numbers are read as
#'   BBS codes.
#' @param level What `x` holds: `"district"` (the default), `"division"` or
#'   `"thana"` (thana or upazila names).
#' @param district For thanas only: the district of each value, in any
#'   spelling this function understands, as one value for all of `x` or one
#'   value per element of `x` (for example the district column of the same
#'   data). It decides between thanas with the same name and makes "Sadar"
#'   readable. A thana that is not in the given district is not matched.
#'   Ignored for districts and divisions.
#' @param style Spelling to return: `"current"` (the default) is the official
#'   spelling; `"legacy"` the spelling used before 2018 (districts and
#'   divisions); `"bangla"` the name in Bengali script (districts and
#'   divisions only); `"map"` the name in the HDX/BBS boundary files, for
#'   joining to a map. For thanas, `"legacy"` is the same as `"map"`. Thanas
#'   created after the boundary files were made have no map name and give
#'   `NA`; a "within" thana has no area of its own in those files, so it gets
#'   the name of the area it lies in (Ashulia gives Savar), and a message
#'   lists these.
#' @param fuzzy Correct typing errors? Default `TRUE`. See Details.
#' @param max_distance Largest number of edits for a typing error. Default 2.
#' @param details If `FALSE` (the default), return the standard names. If
#'   `TRUE`, return a table with one row per value (see Value).
#' @param report Which changes to list in a message: `"fixes"` (the default)
#'   lists other spellings and typing errors; `"all"` also lists pre-2018
#'   spellings and Bangla names; `"none"` lists nothing.
#' @param warn If `TRUE` (the default), warn how many non-missing values
#'   could not be matched to one place.
#' @return With `details = FALSE`, a character vector of the same length as
#'   `x`. With `details = TRUE`, a tibble (a data frame if tibble is not
#'   installed) with one row per value. The columns depend on `level`:
#'   \describe{
#'     \item{district}{`input`, `district`, `division`, `bbs_code`, `pcode`,
#'       `iso_code`, `match`, `matched_as`, `candidates`.}
#'     \item{division}{`input`, `division`, `bbs_code`, `match`,
#'       `matched_as`, `candidates`.}
#'     \item{thana}{`input`, `thana`, `district`, `division`, `type`
#'       ("rural", "city" or "within"), `map_name`, `pcode`, `match`,
#'       `matched_as`, `candidates`, `note`.}
#'   }
#'   `match` is "exact" (the official spelling), "variant" (another known
#'   spelling), "typing error" (corrected by fuzzy matching), "code" (a
#'   district code), "ambiguous" (several places have this name; see
#'   `candidates`), "not found" or "missing". `matched_as` names the list of
#'   spellings that matched (for example "old spelling", "postal spelling",
#'   "boundary-file spelling"). The name column in the table always uses the
#'   chosen `style`.
#' @seealso [bd_address()] for whole addresses, [bd_districts],
#'   [bd_thanas].
#' @examples
#' bd_place(c("Chittagong", "jessore district", "Jhalokati", "Munshigonj",
#'            "COX'S BAZAR", "Nababganj", "Kishorgnj"))
#' bd_place(c("Chattogram", "Bogura"), style = "legacy")
#' bd_place(c("Jessore", "Bogra"), style = "bangla")
#' bd_place(c("26", "BD-13", "BD4041"), details = TRUE)
#'
#' bd_place(c("Barisal Division", "chittagong"), level = "division")
#'
#' # Thanas: Kaliganj is in four districts; "Sadar" needs the district
#' bd_place(c("Kaliganj", "Kaliganj", "Sadar", "Savar Upazila"), level = "thana",
#'          district = c("Gazipur", NA, "Feni", NA), warn = FALSE)
#' bd_place(c("Ashulia", "Fatulla", "Mirpur", "Double Mooring"), level = "thana",
#'          district = c("Dhaka", "Narayanganj", "Dhaka", "Chittagong"), details = TRUE)
#'
#' # Names for joining to a map
#' bd_place(c("Brahmanbaria Sadar", "Ashulia"), level = "thana", style = "map")
#' @export
bd_place <- function(x, level = c("district", "division", "thana"),
                     district = NULL, style = c("current", "legacy", "bangla", "map"),
                     fuzzy = TRUE, max_distance = 2, details = FALSE,
                     report = c("fixes", "all", "none"), warn = TRUE) {
  level <- match.arg(level)
  style <- match.arg(style)
  report <- match.arg(report)
  .check_flag(fuzzy, "fuzzy")
  .check_distance(max_distance)
  .check_flag(details, "details")
  .check_flag(warn, "warn")
  if (level == "thana" && style == "bangla") {
    warning("Bangla names of thanas are not in the data yet; style = \"bangla\" gives NA.",
            call. = FALSE)
  }
  input <- .as_utf8_chr(x)
  dist <- if (level == "thana") .district_arg(district, length(input))
  m <- .match_names(input, level, dist, fuzzy, max_distance)

  if (level == "district") {
    code <- .district_code(input)
    is_code <- attr(code, "is_code")
    m$id[is_code] <- code[is_code]
    m$match[is_code] <- ifelse(is.na(code[is_code]), "not found", "code")
    m$how[is_code] <- NA_character_
    m$candidates[is_code] <- NA_character_
  }
  out <- .styled(m$id, level, style)
  .warn_invalid(sum(.nonblank(input) & is.na(m$id)), paste("matched to one", level), warn)
  .report_changes(input, out, m$how, level, report)
  if (level == "thana" && style %in% c("map", "legacy") && report != "none") {
    .report_map_areas(m$id, out)
  }
  if (!details) return(out)

  matched_as <- ifelse(m$how %in% "sadar", "Sadar + district", m$how)
  matched_as[m$match == "ambiguous"] <- NA_character_
  tab <- switch(level,
    district = {
      d <- .tab_districts[m$id, ]
      data.frame(input = input, district = out, division = d$division,
                 bbs_code = d$bbs_code, pcode = d$pcode, iso_code = d$iso_code,
                 stringsAsFactors = FALSE)
    },
    division = {
      v <- .tab_divisions[m$id, ]
      data.frame(input = input, division = out, bbs_code = v$bbs_code,
                 stringsAsFactors = FALSE)
    },
    thana = {
      th <- .tab_thanas[m$id, ]
      blank_na <- function(v) ifelse(v %in% "", NA_character_, v)
      data.frame(input = input, thana = out, district = th$district, division = th$division,
                 type = th$type, map_name = blank_na(th$map_name), pcode = blank_na(th$pcode),
                 stringsAsFactors = FALSE)
    })
  tab$match <- m$match
  tab$matched_as <- matched_as
  tab$candidates <- m$candidates
  if (level == "thana") tab$note <- ifelse(.tab_thanas$note[m$id] %in% "", NA_character_,
                                           .tab_thanas$note[m$id])
  .as_tbl(tab)
}

# Message for "within" thanas written with the map name of the area they lie
# in (style = "map"): Ashulia -> Savar.
.report_map_areas <- function(id, out) {
  w <- !is.na(id) & .tab_thanas$type[id] %in% "within"
  if (!any(w)) return(invisible(NULL))
  pairs <- unique(paste0("  \"", .tab_thanas$thana[id[w]], "\" -> \"", out[w], "\""))
  message(sum(w), if (sum(w) == 1L) " thana has" else " thanas have",
          " no area of its own in the boundary files and ",
          if (sum(w) == 1L) "was" else "were", " given the area it lies in:\n",
          paste(pairs, collapse = "\n"))
  invisible(NULL)
}
