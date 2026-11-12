# Checking and cleaning whole data frames -------------------------------------

.spec_types <- c("phone", "amount", "number", "district", "division", "thana", "address")

.validate_spec <- function(data, spec) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (is.list(spec)) spec <- unlist(spec)
  if (!is.character(spec) || is.null(names(spec)) || any(names(spec) == "")) {
    stop("`spec` must be a named character vector, for example ",
         "c(mobile = \"phone\", zila = \"district\").", call. = FALSE)
  }
  if (anyDuplicated(names(spec))) {
    stop("Each column may appear only once in `spec`. Duplicated: ",
         paste(unique(names(spec)[duplicated(names(spec))]), collapse = ", "), call. = FALSE)
  }
  missing_cols <- setdiff(names(spec), names(data))
  if (length(missing_cols)) {
    stop("Column(s) not found in `data`: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  bad <- setdiff(unique(spec), .spec_types)
  if (length(bad)) {
    stop("Unknown type(s) in `spec`: ", paste(bad, collapse = ", "), ". Use one of: ",
         paste(.spec_types, collapse = ", "), ".",
         if ("upazila" %in% bad) " Upazila names are cleaned with type \"thana\".",
         call. = FALSE)
  }
  spec
}

# Standard district names from the column named by `district`, or NULL.
.district_column <- function(data, district) {
  if (is.null(district)) return(NULL)
  if (!is.character(district) || length(district) != 1L || !district %in% names(data)) {
    stop("`district` must be the name of one column in `data`.", call. = FALSE)
  }
  # Read the district names once (one warning for names that cannot be read).
  .district_arg(bd_missing(data[[district]]), nrow(data))
}

# Clean one column. Returns a list: `value` (the main result, NA where the
# value could not be read), `extra` (more columns, named by their suffix) and
# `reason` (why each value could not be read; NA where it could).
.clean_one <- function(x, type, dist, phone_format = "local", style = "current",
                       fuzzy = TRUE, report = "none", label = NULL) {
  if (type == "phone") {
    d <- bd_phone(x, format = phone_format, details = TRUE, warn = FALSE)
    return(list(value = d$phone, extra = NULL, reason = d$note))
  }
  if (type == "amount") {
    d <- bd_amount(x, details = TRUE, warn = FALSE)
    return(list(value = d$amount, extra = NULL, reason = d$note))
  }
  if (type == "number") {
    v <- bd_number(x, warn = FALSE)
    return(list(value = v, extra = NULL, reason = ifelse(is.na(v), "not a plain number", NA)))
  }
  if (type == "address") {
    d <- bd_address(x, district = dist)
    ok <- !is.na(d$district) & d$status != "conflict"
    extra <- list(postcode = d$postcode, thana = d$thana,
                  district = d$district, division = d$division, status = d$status)
    reason <- ifelse(ok, NA_character_,
                     ifelse(is.na(d$note), d$status, paste0(d$status, ": ", d$note)))
    return(list(value = NULL, extra = extra, reason = reason, ok = ok))
  }
  # district, division, thana
  if (type != "thana") dist <- NULL
  if (type == "thana" && style == "bangla") style <- "current"
  d <- bd_place(x, level = type, district = dist, style = style, fuzzy = fuzzy,
                details = TRUE, report = "none", warn = FALSE)
  value <- d[[type]]
  how <- ifelse(d$match %in% c("exact", "code", "ambiguous"), NA_character_,
                ifelse(d$matched_as %in% "Sadar + district", "sadar", d$matched_as))
  .report_changes(x, value, how, type, report, label)
  if (type == "thana" && style %in% c("map", "legacy") && report != "none") {
    .report_map_areas(.match_names(x, "thana", dist, fuzzy)$id, value)
  }
  why <- if (type == "thana") d$note else d$candidates
  fail <- is.na(value) & .nonblank(x)
  reason <- rep(NA_character_, length(value))
  reason[fail] <- paste("not a", type, "name")
  reason[fail & d$match == "ambiguous"] <- paste("ambiguous:", why[fail & d$match == "ambiguous"])
  reason[fail & d$match %in% c("exact", "variant", "typing error", "code")] <-
    paste0("no ", style, " spelling of this ", type)
  if (!is.null(dist)) {
    reason[fail & d$match == "not found" & .nonblank(dist)] <-
      paste("not a", type, "of the district given")
  }
  extra <- NULL
  list(value = value, extra = extra, reason = reason)
}

#' Check the columns of a data frame
#'
#' Reports, for each column named in `spec`, how many values are missing, how
#' many can be read as the stated type and how many cannot. The data are not
#' changed; [bd_clean()] gives the cleaned values. With `details = TRUE` the
#' result lists every value that cannot be read, with the reason, so it can
#' be fixed at the source or recoded by hand.
#'
#' `NA`, blank values and missing-value markers such as "N/A", "-" or "#N/A"
#' (see [bd_missing()]) count as missing, not as invalid.
#'
#' The types are:
#' - `"phone"`: mobile numbers, see [bd_phone()]. A cell with several
#'   numbers counts as invalid, because it needs a decision;
#' - `"amount"`: money amounts, see [bd_amount()]. Ranges and approximate
#'   amounts count as invalid for the same reason;
#' - `"number"`: plain numbers, see [bd_number()];
#' - `"district"`, `"division"`, `"thana"`: place names, see [bd_place()]
#'   (`"thana"` also for upazila names);
#' - `"address"`: free-text addresses, see [bd_address()]. An address is
#'   valid when its district is found and nothing in it contradicts it.
#'
#' @param data A data frame.
#' @param spec A named character vector (or list) giving the type of each
#'   column to check, for example `c(mobile = "phone", zila = "district")`.
#' @param details If `FALSE` (the default), return one summary row per
#'   column. If `TRUE`, return one row per value that cannot be read.
#' @param district Name of the column that holds the district of each row.
#'   Used for `"thana"` and `"address"` columns: it decides between thanas
#'   with the same name (see [bd_place()]).
#' @param id Optional name of an identifier column, included when
#'   `details = TRUE`.
#' @param markers Missing-value markers, passed to [bd_missing()]. `NULL`
#'   (the default) uses the built-in list; `character(0)` counts only `NA`
#'   and blank values as missing.
#' @param fuzzy Correct typing errors in place names? Default `TRUE`, as in
#'   [bd_place()].
#' @return With `details = FALSE`, a data frame with one row per checked
#'   column: `column`, `type`, `n`, `n_missing`, `n_valid`, `n_invalid`,
#'   `pct_valid` (valid values as a percentage of non-missing values) and
#'   `invalid_examples` (up to three different values that failed). With
#'   `details = TRUE`, a tibble (a data frame if tibble is not installed)
#'   with `id` (if given), `row`, `column`, `type`, `value` and `reason`.
#' @seealso [bd_clean()].
#' @examples
#' df <- data.frame(
#'   mobile = c("01712345678", "+880 1812-345678", "12345", "N/A"),
#'   zila   = c("Chittagong", "Jessore", "Dhaka", "Atlantis"),
#'   income = c("Tk 25,000", "1.5 lakh", "15-20k", "30000"),
#'   stringsAsFactors = FALSE
#' )
#' spec <- c(mobile = "phone", zila = "district", income = "amount")
#' bd_check(df, spec)
#' bd_check(df, spec, details = TRUE)
#' @export
bd_check <- function(data, spec, details = FALSE, district = NULL, id = NULL,
                     markers = NULL, fuzzy = TRUE) {
  spec <- .validate_spec(data, spec)
  .check_flag(details, "details")
  .check_flag(fuzzy, "fuzzy")
  if (!is.null(id) && (!is.character(id) || length(id) != 1L || !id %in% names(data))) {
    stop("`id` must be the name of one column in `data`.", call. = FALSE)
  }
  dist <- .district_column(data, district)
  rows <- lapply(names(spec), function(col) {
    x <- bd_missing(data[[col]], markers = markers)
    present <- .nonblank(x)
    r <- .clean_one(x, spec[[col]], dist, fuzzy = fuzzy)
    ok <- if (!is.null(r$ok)) r$ok else !is.na(r$value)
    invalid <- present & !ok
    if (details) {
      bad <- which(invalid)
      out <- data.frame(row = bad, column = rep(col, length(bad)),
                        type = rep(spec[[col]], length(bad)),
                        value = .as_utf8_chr(data[[col]])[bad], reason = r$reason[bad],
                        stringsAsFactors = FALSE)
      if (!is.null(id)) out <- data.frame(id = data[[id]][bad], out, stringsAsFactors = FALSE)
      return(out)
    }
    data.frame(
      column = col, type = spec[[col]], n = length(x),
      n_missing = sum(!present), n_valid = sum(present & ok), n_invalid = sum(invalid),
      pct_valid = if (any(present)) round(100 * sum(present & ok) / sum(present), 1) else NA_real_,
      invalid_examples = paste(utils::head(unique(.as_utf8_chr(x)[invalid]), 3L), collapse = "; "),
      stringsAsFactors = FALSE)
  })
  .as_tbl(do.call(rbind, rows))
}

#' Clean the columns of a data frame
#'
#' Cleans each column named in `spec` with the matching bdscrub function (see
#' [bd_check()] for the types) and adds the result as new columns, so the
#' original values stay available for checking:
#' - `"phone"`, `"amount"`, `"number"`, `"district"`, `"division"`,
#'   `"thana"`: one column `<column><suffix>`;
#' - `"address"`: `<column>_postcode`, `_thana`, `_district`, `_division`
#'   and `_status` (see [bd_address()]).
#'
#' Missing-value markers ("N/A", "-", ...) become `NA` first. Values that
#' cannot be cleaned are `NA`; [bd_check()] with `details = TRUE` lists them
#' with the reason. Changed place names are listed in a message, one per
#' column, as in [bd_place()].
#'
#' @inheritParams bd_check
#' @param district Name of the column that holds the district of each row
#'   (used for `"thana"` and `"address"` columns). It may be
#'   one of the columns being cleaned; it is read in any spelling.
#' @param suffix Text added to the column name for the cleaned column.
#'   Default `"_clean"`; `""` replaces the original column.
#' @param phone_format Format for phone numbers, as in [bd_phone()].
#' @param style Spelling of place names, as in [bd_place()].
#' @param report Which changed place names to list: `"fixes"` (the
#'   default), `"all"` or `"none"`, as in [bd_place()].
#' @return `data` with the cleaned columns added.
#' @seealso [bd_check()].
#' @examples
#' df <- data.frame(
#'   mobile  = c("01712345678", "+880 1812-345678", "12345"),
#'   zila    = c("Chittagong", "Jessore", "Dhaka"),
#'   thana   = c("Double Mooring", "Kotwali", "Ashulia"),
#'   income  = c("Tk 25,000", "1.5 lakh", "unknown"),
#'   address = c("Agrabad, Chattogram 4100", "Kotwali, Jessore", "Ashulia, Savar"),
#'   stringsAsFactors = FALSE
#' )
#' bd_clean(df, c(mobile = "phone", zila = "district", thana = "thana",
#'                income = "amount", address = "address"), district = "zila")
#' @export
bd_clean <- function(data, spec, district = NULL, suffix = "_clean",
                     phone_format = c("local", "e164", "international"),
                     style = c("current", "legacy", "bangla", "map"), fuzzy = TRUE,
                     report = c("fixes", "all", "none"), markers = NULL) {
  spec <- .validate_spec(data, spec)
  phone_format <- match.arg(phone_format)
  style <- match.arg(style)
  report <- match.arg(report)
  .check_flag(fuzzy, "fuzzy")
  if (!is.character(suffix) || length(suffix) != 1L || is.na(suffix)) {
    stop("`suffix` must be a single string.", call. = FALSE)
  }
  dist <- .district_column(data, district)
  for (col in names(spec)) {
    x <- bd_missing(data[[col]], markers = markers)
    r <- .clean_one(x, spec[[col]], dist, phone_format, style, fuzzy, report, label = col)
    if (!is.null(r$value)) data[[paste0(col, suffix)]] <- r$value
    for (nm in names(r$extra)) data[[paste0(col, "_", nm)]] <- r$extra[[nm]]
  }
  data
}
