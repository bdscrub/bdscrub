# Regression tests for the review of 8 November 2026.

test_that("phone numbers separated only by spaces, and '.0' from spreadsheets", {
  p <- bd_phone(c("01712345678 01812345678", "Mob 01712345678 Home 01812-345678",
                  "8801712345678.0", "1712345678.0", "017123456780171234567"),
                details = TRUE, multiple = "first", warn = FALSE)
  expect_identical(p$n_numbers, c(2L, 2L, 1L, 1L, 0L))
  expect_identical(p$all_numbers[1], "01712345678; 01812345678")
  expect_identical(p$phone[3:4], c("01712345678", "01712345678"))
  expect_identical(bd_phone("01712345678 01812345678", warn = FALSE), NA_character_)
})

test_that("a time period after an amount is ignored and noted", {
  x <- c("5000 taka per month", "Tk 25,000/month", "1.5 lakh yearly", "20000 monthly",
         "15-20k per month")
  expect_equal(bd_amount(x, warn = FALSE), c(5000, 25000, 150000, 20000, NA))
  d <- bd_amount(x, details = TRUE)
  expect_match(d$note[1], "\"per month\" ignored")
  expect_equal(c(d$low[5], d$high[5]), c(15000, 20000))
  expect_match(d$note[5], "a range.*ignored")
  # Bangla "mase" (per month)
  expect_equal(bd_amount(paste("5000", intToUtf8(c(0x09AE, 0x09BE, 0x09B8, 0x09C7)))), 5000)
})

test_that("'City' and 'City Corporation' do not stop a district match", {
  expect_identical(bd_place(c("Dhaka City", "Chattogram City Corporation", "Khulna city corp"),
                            report = "none"), c("Dhaka", "Chattogram", "Khulna"))
})

test_that("a typing error close to two districts is refused", {
  # "Bagura" is one edit from Bogura and from Magura
  expect_identical(bd_place("Bagura", warn = FALSE), NA_character_)
})

test_that("bd_address treats markers as missing and prefers the smaller thana", {
  r <- bd_address(c("N/A", "-", "Savar, Ashulia, Dhaka", "Ashulia, Savar, Dhaka", "Tongi, Gazipur"))
  expect_identical(r$status[1:2], c("missing", "missing"))
  expect_identical(r$thana[3:5], c("Ashulia", "Ashulia", "Tongi"))
})

test_that("post office names with typing errors on the source pages are corrected", {
  expect_true("Dhaka Cantt. TSO" %in% bd_postcodes$post_office)
  expect_false(any(grepl(",", bd_postcodes$post_office)))
})

test_that("a dash after the currency is not a minus sign", {
  expect_equal(bd_amount(c("BDT-500", "Tk.-500", "Tk - 1,500", "-Tk 500", "-500", "Tk 500")),
               c(500, 500, 1500, -500, -500, 500))
})
