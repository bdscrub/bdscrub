test_that("bd_squish handles ordinary and Unicode whitespace", {
  x <- c("  Dhaka   District ", "Tk\u00a0500", "\ufeff01712345678", "a\u200bb",
         "Tk\u2009500", "\tline\nbreak ", NA, "")
  expect_identical(bd_squish(x),
                   c("Dhaka District", "Tk 500", "01712345678", "ab", "Tk 500",
                     "line break", NA, ""))
})

test_that("bd_squish strips wrapping quotes only on request", {
  x <- c("'01712345678", "\"Dhaka\"", "\u201cDhaka\u201d", "Cox's Bazar")
  expect_identical(bd_squish(x), x)
  expect_identical(bd_squish(x, strip_quotes = TRUE),
                   c("01712345678", "Dhaka", "Dhaka", "Cox's Bazar"))
  expect_error(bd_squish("a", strip_quotes = NA), "TRUE or FALSE")
})

test_that("bd_na_if replaces whole-value markers only", {
  x <- c("23", "N/A", " - ", "#N/A", "none", "", "  ", "not applicable", "41",
         "N/A at time of survey", "NULL", "na", "n.a.", "#DIV/0!", "?", NA)
  expect_identical(bd_missing(x),
                   c("23", NA, NA, NA, "none", NA, NA, NA, "41",
                     "N/A at time of survey", NA, NA, NA, NA, NA, NA))
})

test_that("bd_na_if leaves real answers alone unless asked", {
  x <- c("none", "nil", "0", "don't know", "unknown", "no")
  expect_identical(bd_missing(x), x)
  expect_identical(bd_missing(x, extra = c("None", "don't know")),
                   c(NA, "nil", "0", NA, "unknown", "no"))
  expect_identical(bd_missing(c("", "x"), blank = FALSE), c("", "x"))
  expect_identical(bd_missing(c("N/A", "x"), markers = character(0)), c("N/A", "x"))
})

test_that("bd_na_if knows the Bangla markers, in any Unicode form", {
  na_bn <- "\u09aa\u09cd\u09b0\u09af\u09cb\u099c\u09cd\u09af \u09a8\u09af\u09bc"
  na_bn_one_point <- "\u09aa\u09cd\u09b0\u09af\u09cb\u099c\u09cd\u09af \u09a8\u09df"
  no_info <- "\u09a4\u09a5\u09cd\u09af  \u09a8\u09c7\u0987 "
  expect_identical(bd_missing(c(na_bn, na_bn_one_point, no_info, "x")),
                   c(NA, NA, NA, "x"))
})

test_that("bd_na_if keeps factors as factors and drops marker levels", {
  f <- factor(c("a", "N/A", "b", "N/A"))
  out <- bd_missing(f)
  expect_true(is.factor(out))
  expect_identical(levels(out), c("a", "b"))
  expect_identical(as.character(out), c("a", NA, "b", NA))
  expect_error(bd_missing("x", markers = 1), "character vectors")
})

test_that("bd_check counts markers as missing, not invalid", {
  df <- data.frame(age = c("23", "N/A", "#N/A", "abc", "-"), stringsAsFactors = FALSE)
  res <- bd_check(df, c(age = "number"))
  expect_identical(c(res$n_missing, res$n_valid, res$n_invalid), c(3L, 1L, 1L))
  res0 <- bd_check(df, c(age = "number"), markers = character(0))
  expect_identical(c(res0$n_missing, res0$n_valid, res0$n_invalid), c(0L, 1L, 4L))
})
