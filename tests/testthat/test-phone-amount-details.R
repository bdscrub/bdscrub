test_that("several numbers, reasons and +881 in bd_phone(details = TRUE)", {
  x <- c("01712345678 / 01812345678", "017-1234", "+881712345678",
         "1.71234E+09", "01212345678", "not given", "017123456789",
         "phone: 01912345678, alt 01612345678", "01712345678 or 01712345678",
         "", NA, "+880 1712-345678")
  p <- bd_phone(x, details = TRUE, multiple = "first", warn = FALSE)
  expect_identical(p$phone, c("01712345678", NA, NA, NA, NA, NA, NA, "01912345678", "01712345678",
                              NA, NA, "01712345678"))
  expect_identical(p$n_numbers, c(2L, 0L, 0L, 0L, 0L, 0L, 0L, 2L, 1L, 0L, 0L, 1L))
  expect_identical(p$valid, c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,
                              NA, NA, TRUE))
  expect_identical(p$all_numbers[1], "01712345678; 01812345678")
  expect_identical(p$note[2], "too few digits")
  expect_match(p$note[3], "^country code \\+881")
  expect_identical(p$note[4:7], c("Excel scientific notation: the digits are lost",
                                  "not a mobile prefix (013 to 019)", "no digits", "too many digits"))
  # the default does not choose between two numbers
  expect_identical(bd_phone(x[1], warn = FALSE), NA_character_)
  expect_identical(bd_phone(x[8], multiple = "first", format = "e164"), "+8801912345678")
  expect_identical(bd_phone("+881712345678", warn = FALSE), NA_character_)
  expect_identical(bd_phone("+881712345678", plus881 = TRUE), "01712345678")
  expect_match(bd_phone("+881712345678", plus881 = TRUE, details = TRUE)$note, "read \\+881")
  expect_identical(bd_phone(character(0), details = TRUE)$phone, character(0))
})

test_that("ranges and approximate amounts in bd_amount(details = TRUE)", {
  x <- c("15000-20000", "15k-20k", "15-20k", "about 5 lakh", "approx 30000",
         "2 lakh to 3 lakh", "12 hajar 1335", "Tk 25,00", "varies", "25000",
         "-5000", "20000-15000", "", NA)
  a <- bd_amount(x, details = TRUE, warn = FALSE)
  expect_equal(a$low, c(15000, 15000, 15000, 5e5, 30000, 2e5, NA, NA, NA, 25000, -5000, NA, NA, NA))
  expect_equal(a$high, c(20000, 20000, 20000, 5e5, 30000, 3e5, NA, NA, NA, 25000, -5000, NA, NA, NA))
  expect_identical(a$amount[c(1, 4, 10, 11)], c(NA, NA, 25000, -5000))
  expect_identical(a$approximate, c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
                                    FALSE, FALSE, FALSE, NA, NA))
  expect_identical(a$note[c(3, 7, 8, 9, 12)], c("a range; unit of the upper end used for both ends",
    "parts that do not add up (for example 12 hajar 1335)", "commas in unusual places",
    "no number", "range with the lower end not below the upper end"))
  single <- c("Tk 1,25,000", "1.5 lakh", "BDT 1 crore 20 lakh", "30000/-")
  expect_identical(bd_amount(single, details = TRUE)$amount, bd_amount(single))
  # "2 lakh theke 3 lakh" in Bangla words
  th <- intToUtf8(c(0x09E8, 0x20, 0x09B2, 0x09BE, 0x0996, 0x20, 0x09A5, 0x09C7, 0x0995, 0x09C7,
                    0x20, 0x09E9, 0x20, 0x09B2, 0x09BE, 0x0996))
  expect_equal(unlist(bd_amount(th, details = TRUE)[, c("low", "high")]), c(low = 2e5, high = 3e5))
})
