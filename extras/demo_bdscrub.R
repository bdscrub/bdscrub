# =============================================================================
# bdscrub demonstration on a large messy survey file
# =============================================================================
#
# What this script does
#   1. reads data/bdscrub_messy_survey.csv (3,030 rows, every column messy);
#   2. checks the file with bd_check();
#   3. cleans each kind of field with its function and compares the result
#      with the answer key (data/bdscrub_answer_key.csv), which holds the true
#      value of every cell;
#   4. cleans everything at once with bd_clean() and saves the result.
#
# How to run
#   Open this file in RStudio, set the working directory to the folder that
#   holds this script (Session > Set Working Directory > To Source File
#   Location) and source it. It uses the installed bdscrub package if there
#   is one, otherwise the one-file version bdscrub_standalone.R next to it.
#
# Requirements: R >= 4.1. No other package is needed.
#
# How to read the scores
#   correct    the cleaned value equals the true value (also: NA where the
#              cell was empty or a missing-value marker);
#   flagged    the cleaned value is NA although there was a true value: the
#              function refused to guess (for example a typing error too
#              far from any name, or "15-20k" in the exact-amount column).
#              These are listed by bd_check(details = TRUE) for fixing by hand;
#   wrong      the cleaned value is a different value. This is the dangerous
#              kind of error; it should be (close to) zero.
# =============================================================================

# ---- 0. Setup ----------------------------------------------------------------
# The folder that holds data/ and bdscrub_standalone.R: this works when the
# working directory is extras/ or the top folder of the repository.
demo_dir <- if (exists("demo_dir")) demo_dir else {
  here <- c(".", "extras", "..")
  here[file.exists(file.path(here, "data", "bdscrub_messy_survey.csv"))][1]
}
if (is.na(demo_dir)) stop("Set the working directory to the folder that holds this script.")
if (!exists("bd_phone")) {
  if (requireNamespace("bdscrub", quietly = TRUE)) {
    library(bdscrub)
  } else {
    source(file.path(demo_dir, "bdscrub_standalone.R"), encoding = "UTF-8")
  }
}

read_utf8 <- function(file) {
  utils::read.csv(file.path(demo_dir, "data", file), colClasses = "character",
                  na.strings = character(0), fileEncoding = "UTF-8-BOM",
                  stringsAsFactors = FALSE, check.names = FALSE)
}
survey <- read_utf8("bdscrub_messy_survey.csv")
key <- read_utf8("bdscrub_answer_key.csv")
stopifnot(identical(survey$id, key$id))
cat("Rows:", nrow(survey), "\n\n")
print(utils::head(survey, 8))

# Compare cleaned values with the truth. `want` is "" where the cell had no
# true value.
score <- function(got, want, label) {
  want[want == ""] <- NA
  if (is.numeric(got)) want <- as.numeric(want)
  res <- ifelse(is.na(got) & is.na(want), "correct",
         ifelse(is.na(got), "flagged",
         ifelse(!is.na(want) & got == want, "correct", "wrong")))
  tab <- table(factor(res, levels = c("correct", "flagged", "wrong")))
  cat(sprintf("%-28s correct %5.1f%%   flagged (NA) %5.1f%%   wrong %5.1f%%\n", label,
              100 * tab[["correct"]] / length(res), 100 * tab[["flagged"]] / length(res),
              100 * tab[["wrong"]] / length(res)))
  invisible(res)
}
show_wrong <- function(res, ...) {
  d <- data.frame(..., stringsAsFactors = FALSE)[res == "wrong", , drop = FALSE]
  if (nrow(d)) print(utils::head(d, 10), row.names = FALSE) else cat("  (no wrong values)\n")
}

# ---- 1. What is in the file? -------------------------------------------------
spec <- c(mobile = "phone", age = "number", monthly_income = "amount",
          division = "division", district = "district", thana = "thana",
          address = "address")
cat("\n== 1. bd_check(): how many values can be read ==\n")
print(bd_check(survey, spec, district = "district"))

cat("\nSome values that cannot be read, with the reason:\n")
failures <- bd_check(survey, spec, details = TRUE, district = "district", id = "id")
print(utils::head(failures[order(failures$column), ], 15), row.names = FALSE)

# ---- 2. Mobile numbers ---------------------------------------------------------
cat("\n== 2. bd_phone() ==\n")
ph <- bd_phone(survey$mobile, details = TRUE, multiple = "first", warn = FALSE)
print(utils::head(ph[ph$input != ph$phone | is.na(ph$phone), ], 10))
res <- score(ph$phone, key$phone, "mobile (multiple = 'first')")
show_wrong(res, input = survey$mobile, got = ph$phone, want = key$phone)
cat("Cells with two numbers:", sum(ph$n_numbers > 1), "\n")
cat("Why numbers could not be read:\n")
print(sort(table(ph$note[is.na(ph$phone)]), decreasing = TRUE))
cat("\nWith plus881 = TRUE the '+881' typing error is read too:\n")
print(utils::head(bd_phone(survey$mobile[grepl("^[+]881", survey$mobile)], plus881 = TRUE), 5))

# ---- 3. Age and income -----------------------------------------------------------
cat("\n== 3. bd_number() and bd_amount() ==\n")
age <- bd_number(bd_missing(survey$age), warn = FALSE)
res <- score(age, key$age, "age")
inc <- bd_amount(bd_missing(survey$monthly_income), details = TRUE, warn = FALSE)
res <- score(inc$amount, key$income, "income, exact amount")
show_wrong(res, input = survey$monthly_income, got = inc$amount, want = key$income)
score(inc$low, key$income_low, "income, lower end")
score(inc$high, key$income_high, "income, upper end")
cat("Ranges:", sum(inc$low < inc$high, na.rm = TRUE),
    "  approximate:", sum(inc$approximate, na.rm = TRUE), "\n")
cat("Formatted back with lakh grouping:\n")
print(utils::head(bd_taka(inc$amount[!is.na(inc$amount) & inc$amount >= 1e5], prefix = "Tk "), 5))

# ---- 4. Divisions and districts ----------------------------------------------------
cat("\n== 4. bd_place(): divisions and districts ==\n")
div <- bd_place(bd_missing(survey$division), level = "division", warn = FALSE)
res <- score(div, ifelse(key$division_written_as == "missing", "", key$division), "division")
dist <- bd_place(bd_missing(survey$district), warn = FALSE)
res <- score(dist, ifelse(key$district_written_as == "missing", "", key$district), "district")
show_wrong(res, input = survey$district, got = dist, want = key$district)
cat("District score by how it was written:\n")
print(round(100 * prop.table(table(key$district_written_as, res), 1), 1))
cat("\nFor maps: the boundary-file spelling of each district\n")
print(utils::head(unique(data.frame(district = dist,
                                    map = bd_place(dist, style = "map", report = "none", warn = FALSE))
                         [dist != bd_place(dist, style = "map", report = "none", warn = FALSE) &
                            !is.na(dist), ]), 4))

# ---- 5. Thanas (the thana / upazila column) -----------------------------------
cat("\n== 5. bd_place(): thanas, with the district column ==\n")
want_th <- ifelse(key$thana_written_as == "missing", "", key$thana)
th <- bd_place(bd_missing(survey$thana), level = "thana", district = dist, details = TRUE,
               report = "none", warn = FALSE)
res <- score(th$thana, want_th, "thana (district given)")
show_wrong(res, input = survey$thana, district = dist, got = th$thana, want = want_th)
th0 <- bd_place(bd_missing(survey$thana), level = "thana", report = "none", warn = FALSE)
score(th0, want_th, "thana (no district)")
cat("  -> names used in several districts need the district column\n")
cat("Thana types found:\n")
print(table(th$type, useNA = "ifany"))
cat("For maps: thanas inside a larger area get the map name of that area\n")
print(utils::head(unique(data.frame(thana = th$thana, type = th$type, map_name = th$map_name)
                         [th$type %in% "within", ]), 6))

# ---- 6. Addresses ------------------------------------------------------------------
cat("\n== 6. bd_address() ==\n")
ad <- bd_address(survey$address, district = dist)
print(table(ad$status))
has <- !ad$status %in% c("missing", "not found")
res <- score(ad$district[has], key$district[has], "address -> district")
show_wrong(res, address = survey$address[has], got = ad$district[has], want = key$district[has])
got <- has & !is.na(ad$thana)
res <- score(ad$thana[got], key$thana[got], "address -> thana (when found)")
show_wrong(res, address = survey$address[got], got = ad$thana[got], want = key$thana[got])
print(utils::head(ad[ad$status == "conflict", c("input", "district", "status", "note")], 5))

# ---- 7. Everything at once ----------------------------------------------------------
cat("\n== 7. bd_clean(): all columns at once ==\n")
clean <- bd_clean(survey, spec, district = "district", report = "none")
print(utils::head(clean[, c("id", "mobile_clean", "age_clean", "monthly_income_clean",
                            "district_clean", "thana_clean",
                            "address_district", "address_status")], 8))
cat("\nExact duplicate rows (submitted twice):", sum(duplicated(survey)), "\n")
out_file <- file.path(tempdir(), "bdscrub_survey_clean.csv")
utils::write.csv(clean, out_file, row.names = FALSE, fileEncoding = "UTF-8")
cat("Cleaned data written to", out_file, "\n")
