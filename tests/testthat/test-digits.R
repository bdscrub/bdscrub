bn <- function(...) intToUtf8(c(...))

test_that("Bangla digits convert both ways", {
  all_bn <- bn(0x09E6:0x09EF)
  expect_identical(bd_digits(all_bn), "0123456789")
  expect_identical(bd_digits("0123456789", to = "bn"), all_bn)
  expect_identical(bd_digits(bd_digits("Tk 1,250.75", to = "bn")), "Tk 1,250.75")
})

test_that("digit conversion keeps NA, length and other text", {
  expect_identical(bd_digits(c("abc", NA)), c("abc", NA))
  expect_identical(bd_digits(character(0)), character(0))
  expect_identical(bd_digits(factor("12"), to = "bn"), bn(0x09E7, 0x09E8))
  expect_error(bd_digits(list(1)), "must be")
})

test_that("bd_number handles grouping, signs and Bangla digits", {
  expect_equal(bd_number(c("1,25,000", "125,000", "12,34,567", "1,234,567",
                                 "-3.5", "+.5", " 42 ", "7.", "1,250.75")),
               c(125000, 125000, 1234567, 1234567, -3.5, 0.5, 42, 7, 1250.75))
  expect_equal(bd_number(bn(0x09E7, 0x002C, 0x09E8, 0x09EB, 0x09E6)), 1250)
  expect_equal(bd_number(1500), 1500)
})

test_that("wrongly grouped digits and spaced digits are rejected, not guessed", {
  x <- c("1,2,3", "1234,567", "12,3456", "12 34", "1,25,00", "12, 500")
  expect_warning(out <- bd_number(x), "6 non-missing values")
  expect_true(all(is.na(out)))
})

test_that("bd_number rejects non-numbers with a warning", {
  expect_warning(out <- bd_number(c("12 kg", "1.2.3", "", NA, "5")),
                 "2 non-missing values")
  expect_equal(out, c(NA, NA, NA, NA, 5))
  expect_silent(bd_number("abc", warn = FALSE))
  expect_error(bd_number("1", warn = NA), "TRUE or FALSE")
})

test_that("missing values stay missing through every parser", {
  expect_identical(bd_number(c(NA, "1,250")), c(NA, 1250))
  expect_identical(bd_amount(c(NA, "Tk 1,250")), c(NA, 1250))
  expect_silent(bd_number(c(NA, NA)))
})

test_that("strings with several grouped numbers are handled", {
  expect_identical(.ungroup_digits(c("1,25,000 or 1,2,3", "x", NA, "12,500")),
                   c("125000 or 1,2,3", "x", NA, "12500"))
})

test_that("text with invalid UTF-8 bytes never crashes any function", {
  bad <- "01712345678\xff"
  Encoding(bad) <- "unknown"
  expect_silent(out <- bd_digits(bad))
  expect_true(validUTF8(out))
  expect_identical(suppressWarnings(bd_number(bad)), NA_real_)
  expect_identical(suppressWarnings(bd_amount(bad)), NA_real_)
  # the number is still found next to the bad byte
  expect_identical(suppressWarnings(bd_phone(bad)), "01712345678")
  expect_identical(suppressWarnings(bd_place(bad)), NA_character_)
})

test_that("invisible characters and byte-order marks are ignored", {
  expect_identical(bd_number("\ufeff1,250"), 1250)
  expect_identical(bd_amount("Tk\u200b 500"), 500)
})

test_that("zero-length input gives zero-length output everywhere", {
  expect_identical(bd_digits(character(0)), character(0))
  expect_identical(bd_number(character(0)), numeric(0))
  expect_identical(bd_amount(character(0)), numeric(0))
  expect_identical(phone_valid(character(0)), logical(0))
  expect_identical(bd_phone(character(0)), character(0))
  expect_identical(phone_operator(character(0)), character(0))
  expect_identical(bd_place(character(0)), character(0))
  expect_identical(nrow(bd_place(character(0), details = TRUE)), 0L)
  expect_identical(nrow(bd_address(character(0))), 0L)
  expect_identical(bd_taka(numeric(0)), character(0))
})

test_that("the Unicode minus sign is read as a minus", {
  expect_identical(bd_number(c("\u22121,250", "\uff0d5")), c(-1250, -5))
})
