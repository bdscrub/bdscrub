df <- data.frame(
  id = c("a", "b", "c", "d", "e"),
  mobile = c("01712345678", "+880 1812-345678", "12345", "N/A", "01712345678 / 01812345678"),
  zila = c("Chittagong", "Jessore", "Dhaka", "Atlantis", "Nababganj"),
  thana = c("Double Mooring", "Kotwali", "Ashulia", "Savar", "Shibganj"),
  income = c("Tk 25,000", "1.5 lakh", "15-20k", "30000", "-"),
  address = c("Agrabad, Chattogram 4100", "Kotwali, Jessore", "Ashulia, Savar", "Nowhere", NA),
  stringsAsFactors = FALSE
)
spec <- c(mobile = "phone", zila = "district", thana = "thana", income = "amount",
          address = "address")

test_that("bd_check counts valid, invalid and missing values", {
  res <- suppressWarnings(bd_check(df, spec, district = "zila"))
  expect_identical(res$column, names(spec))
  expect_identical(res$n_missing, c(1L, 0L, 0L, 1L, 1L))
  expect_identical(res$n_invalid, c(2L, 1L, 0L, 1L, 1L))
  expect_identical(res$n_valid + res$n_invalid + res$n_missing, rep(5L, 5))
})

test_that("bd_check(details = TRUE) lists every failure with a reason", {
  f <- suppressWarnings(bd_check(df, spec, details = TRUE, district = "zila", id = "id"))
  res <- suppressWarnings(bd_check(df, spec, district = "zila"))
  expect_identical(as.vector(table(factor(f$column, levels = names(spec)))), res$n_invalid)
  expect_false(anyNA(f$reason))
  expect_identical(names(f), c("id", "row", "column", "type", "value", "reason"))
  expect_match(f$reason[f$column == "mobile" & f$row == 5], "2 numbers")
})

test_that("bd_clean adds the cleaned columns", {
  out <- suppressWarnings(suppressMessages(bd_clean(df, spec, district = "zila")))
  expect_identical(out$zila_clean, c("Chattogram", "Jashore", "Dhaka", NA, "Chapai Nawabganj"))
  expect_identical(out$thana_clean, c("Double Mooring", "Jashore Sadar", "Ashulia", "Savar", "Shibganj"))
  expect_false("thana_upazila" %in% names(out))
  expect_identical(out$income_clean, c(25000, 150000, NA, 30000, NA))
  expect_identical(out$address_district[1:3], c("Chattogram", "Jashore", "Dhaka"))
  expect_true(all(c("address_postcode", "address_thana", "address_division",
                    "address_status") %in% names(out)))
  expect_identical(bd_clean(df, c(zila = "district"), suffix = "", report = "none")$zila[1],
                   "Chattogram")
})

test_that("bd_check and bd_clean validate their arguments", {
  expect_error(bd_check(df, c(nope = "phone")), "not found")
  expect_error(bd_check(df, c(mobile = "email")), "Unknown type")
  expect_error(bd_check(df, c(thana = "upazila")), "type \"thana\"")
  expect_error(bd_check(df, "phone"), "named")
  expect_error(bd_clean(df, spec, district = "nope"), "one column")
  expect_error(bd_check(list(a = 1), c(a = "number")), "data frame")
})
