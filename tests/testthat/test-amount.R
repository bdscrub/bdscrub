bn <- function(...) intToUtf8(c(...))

test_that("plain and grouped amounts parse", {
  x <- c("Tk 1,25,000", "BDT 125,000", "Tk.500", "500/-", "taka 75.50 only", "1000")
  expect_equal(bd_amount(x), c(125000, 125000, 500, 500, 75.5, 1000))
})

test_that("units multiply and combine", {
  x <- c("1.5 lakh", "2 lac", "3.2 crore", "1 cr", "25k", "4 thousand",
         "1 crore 25 lakh", "2 crore 5 lakh 300", "1.2 million", "2 bn", "5LAKH")
  expect_equal(bd_amount(x),
               c(150000, 2e5, 3.2e7, 1e7, 25000, 4000, 1.25e7, 20500300, 1.2e6, 2e9, 5e5))
})

test_that("Bangla units and currency words parse", {
  five <- 0x09EB
  sp <- 0x0020
  lakh <- c(0x09B2, 0x09BE, 0x0996)
  lokkho <- c(0x09B2, 0x0995, 0x09CD, 0x09B7)
  koti <- c(0x0995, 0x09CB, 0x099F, 0x09BF)
  taka <- c(0x099F, 0x09BE, 0x0995, 0x09BE)
  expect_equal(bd_amount(bn(five, sp, lakh, sp, taka)), 5e5)
  expect_equal(bd_amount(bn(five, sp, lokkho)), 5e5)
  expect_equal(bd_amount(bn(five, sp, koti)), 5e7)
  expect_equal(bd_amount(bn(0x09F3, five, 0x09E6, 0x09E6)), 500)
  # o-kar typed as two code points (U+09C7 U+09BE) is repaired
  koti_split <- c(0x0995, 0x09C7, 0x09BE, 0x099F, 0x09BF)
  expect_equal(bd_amount(bn(five, sp, koti_split)), 5e7)
})

test_that("negative amounts keep their sign", {
  expect_equal(bd_amount(c("-500", "-1 crore 25 lakh", "-Tk 500", "- BDT 2 lakh")),
               c(-500, -1.25e7, -500, -2e5))
})

test_that("amounts with units are exact, not approximately right", {
  expect_identical(bd_amount("1.1 lakh"), 110000)
  expect_identical(bd_amount("3.2 crore"), 32000000)
  expect_identical(bd_amount("2.35 crore 1.1 lakh"), 23610000)
  expect_identical(bd_amount("0.07 crore"), 7e5)
})

test_that("trailing decimal point and wrong grouping", {
  expect_identical(bd_amount("Tk 500."), 500)
  expect_warning(out <- bd_amount(c("Tk 1,2,5", "1234,567 taka")), "2 non-missing")
  expect_identical(out, c(NA_real_, NA_real_))
  expect_identical(bd_amount("Tk 1,00,00,000"), 1e7)
})

test_that("ambiguous text becomes NA with a warning", {
  x <- c("100 200", "about 5 lakh", "5-10 lakh", "25 lakh 1 crore", "lakh",
         "1 crore -5 lakh", "", NA, "12 kg")
  expect_warning(out <- bd_amount(x), "7 non-missing values")
  expect_true(all(is.na(out)))
})

test_that("lakh grouping is formatted correctly", {
  expect_identical(bd_taka(c(0, 5, 999, 1000, 12345, 125000, 1234567, 12500000)),
                   c("0", "5", "999", "1,000", "12,345", "1,25,000", "12,34,567", "1,25,00,000"))
  expect_identical(bd_taka(-9876543.216, digits = 2), "-98,76,543.22")
  expect_identical(bd_taka(c(NA, Inf, 1500), prefix = "Tk "), c(NA, NA, "Tk 1,500"))
  expect_identical(bd_taka(-0.4), "0")
  expect_identical(bd_taka(125000, bangla = TRUE),
                   bd_digits("1,25,000", to = "bn"))
})

test_that("format and parse round-trip", {
  v <- c(1, 99999, 125000.5, 98765432.25)
  expect_equal(bd_amount(bd_taka(v, digits = 2, prefix = "Tk ")), v)
})

test_that("bd_format_amount validates arguments", {
  expect_error(bd_taka("1"), "numeric")
  expect_error(bd_taka(1, digits = -1), "digits")
  expect_error(bd_taka(1, prefix = c("a", "b")), "single strings")
})

test_that("combined amounts with a remainder after the last unit", {
  expect_identical(bd_amount(c("12 hajar 335", "12hajar335", "Tk 12 hazar 335",
                                     "12 thousand 335", "12k 335", "1 lakh 12 hajar 335",
                                     "2 crore 5 lakh 12 hajar 335", "-12 hajar 335")),
                   c(12335, 12335, 12335, 12335, 12335, 112335, 20512335, -12335))
  expect_identical(bd_amount("12 hajar 335.50"), 12335.5)
  # "12 hajar 335" in Bangla
  expect_identical(bd_amount("\u09e7\u09e8 \u09b9\u09be\u099c\u09be\u09b0 \u09e9\u09e9\u09eb"),
                   12335)
})

test_that("a part as large as the unit before it is rejected as a likely typo", {
  x <- c("12 hajar 1335", "1 crore 150 lakh", "1 lakh 100 hajar", "5 hajar 1000")
  expect_warning(out <- bd_amount(x), "4 non-missing values")
  expect_true(all(is.na(out)))
  expect_identical(bd_amount(c("12 hajar 999", "1 crore 99 lakh")), c(12999, 19900000))
})

test_that("the endings /= and the Bangla word for only are removed", {
  expect_identical(bd_amount(c("Tk. 25,000/=", "500/=")), c(25000, 500))
  # "1,25,000 taka matro" in Bangla
  expect_identical(bd_amount("\u09e7,\u09e8\u09eb,\u09e6\u09e6\u09e6 \u099f\u09be\u0995\u09be \u09ae\u09be\u09a4\u09cd\u09b0"),
                   125000)
})

test_that("Unicode minus, thin spaces and sentence punctuation are handled", {
  x <- c("\u22125,000", "Tk \u2212500", "\u09eb\u09e6\u09e6 \u099f\u09be\u0995\u09be\u0964", "5 lakh.",
         "Tk 1,25,000.", "\u09f3\u00a0\u09e7\u09e8\u09eb", "Tk\u2009500", "25,000\u00a0Tk",
         "1,25,000 \u099f\u09be\u0995\u09be,", "'500")
  expect_identical(bd_amount(x), c(-5000, -500, 500, 5e5, 125000, 125, 500, 25000, 125000, 500))
})

test_that("very large numbers keep lakh grouping", {
  expect_identical(bd_taka(c(1e15, 123456789012345)),
                   c("1,00,00,00,00,00,00,000", "12,34,56,78,90,12,345"))
})
