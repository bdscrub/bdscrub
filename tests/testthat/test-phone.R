test_that("common valid formats are recognised", {
  x <- c("01712345678", "+8801712345678", "8801712345678", "008801712345678",
         "+880 1712-345678", "+88 01712 345678", "(017) 1234-5678",
         "017.1234.5678", "1712345678", "88001712345678")
  expect_true(all(phone_valid(x)))
  expect_identical(unique(bd_phone(x)), "01712345678")
})

test_that("every allocated prefix 013-019 is valid and 010-012 are not", {
  ok <- paste0("01", 3:9, "12345678")
  bad <- paste0("01", 0:2, "12345678")
  expect_true(all(phone_valid(ok)))
  expect_false(any(phone_valid(bad)))
})

test_that("malformed numbers are rejected", {
  x <- c("0171234567", "017123456789", "02-9123456", "+8811712345678",
         "+801712345678", "0801712345678", "abc")
  expect_false(any(phone_valid(x)))
  # a number inside other text is found, like "phone: 01712345678"
  expect_identical(bd_phone("01712345678x"), "01712345678")
})

test_that("the character-class bug from the 2024 slides is not reproduced", {
  # "^[+880]+\\d{10}$" accepted these; the package must not.
  expect_false(any(phone_valid(c("0000000000000", "88888888888", "+0+0+1712345678"))))
})

test_that("missing leading zero can be disallowed", {
  expect_true(phone_valid("1712345678"))
  expect_false(phone_valid("1712345678", allow_missing_zero = FALSE))
  expect_true(phone_valid("+8801712345678", allow_missing_zero = FALSE))
})

test_that("numbers stored as numeric and Bangla digits work", {
  expect_true(phone_valid(1712345678))
  expect_true(phone_valid(8801712345678))
  bn <- bd_digits("01712345678", to = "bn")
  expect_identical(bd_phone(bn), "01712345678")
})

test_that("output formats", {
  x <- "01812345678"
  expect_identical(bd_phone(x, "local"), "01812345678")
  expect_identical(bd_phone(x, "e164"), "+8801812345678")
  expect_identical(bd_phone(x, "international"), "+880 1812-345678")
})

test_that("NA handling and warnings", {
  expect_identical(phone_valid(c(NA, "01712345678", "", "  ")), c(NA, TRUE, NA, NA))
  expect_warning(out <- bd_phone(c("123", NA, "", "01712345678")),
                 "1 non-missing value")
  expect_identical(out, c(NA, NA, NA, "01712345678"))
})

test_that("prefix operator follows the allocation table", {
  x <- paste0("01", 3:9, "12345678")
  expect_identical(phone_operator(x),
                   c("Grameenphone", "Banglalink", "Teletalk", "Robi",
                     "Grameenphone", "Robi", "Banglalink"))
  expect_identical(phone_operator(c("123", NA)), c(NA_character_, NA_character_))
})

test_that("invisible characters inside phone numbers are ignored", {
  x <- c("\ufeff01712345678", "017\u200b12345678", "+880 (0) 1712-345678")
  expect_identical(bd_phone(x), rep("01712345678", 3))
})

test_that("Unicode dashes, spaces and quotes are separators", {
  x <- c("017\u20131234\u20135678", "017\u20141234\u20145678", "017\u20101234\u20105678",
         "017\u20021234\u20025678", "017\u20091234\u20095678", "017\u202f1234\u202f5678",
         "\uff0b880 1712345678", "\u201c01712345678\u201d", "'01712345678", "\"01712345678\"")
  expect_identical(bd_phone(x), rep("01712345678", length(x)))
})
