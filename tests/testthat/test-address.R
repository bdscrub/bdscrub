test_that("addresses give the expected thana, district and status", {
  cases <- utils::read.table(text = "
  input|thana|district|status
  House 12, Road 5, Mirpur 10, Dhaka-1216|Mirpur|Dhaka|complete
  Vill: Char Kalia, P.O: Kalia, Narail - 7520|Kalia|Narail|complete
  Kaliganj, Gazipur|Kaliganj|Gazipur|complete
  Kaliganj|||ambiguous
  Kishoreganj, Nilphamari|Kishoreganj|Nilphamari|complete
  Kishoreganj||Kishoreganj|district only
  Bhairab, Kishoreganj|Bhairab|Kishoreganj|complete
  Sadar, Kushtia|Kushtia Sadar|Kushtia|complete
  Jessore Sadar|Jashore Sadar|Jashore|complete
  Kotwali, Jessore|Jashore Sadar|Jashore|complete
  Dhanmondi 27, Dhaka|Dhanmondi|Dhaka|complete
  Agrabad, Double Mooring, Chittagong|Double Mooring|Chattogram|complete
  Savar, Dhaka 1340|Savar|Dhaka|complete
  Dhaka Road, Mymensingh||Mymensingh|district only
  Mirpur, Kushtia - 7030|Mirpur|Kushtia|complete
  Mirpur, Dhaka-7030|Mirpur|Dhaka|conflict
  Rajshahi Division|||division only
  Nowhere street|||not found
  Faridpur||Faridpur|district only
  Faridpur, Pabna|Faridpur|Pabna|complete
  Sherpur, Bogura|Sherpur|Bogura|complete
  Sherpur, Bogra|Sherpur|Bogura|complete
  Mohammadpur, Dhaka|Mohammadpur|Dhaka|complete
  Mohammadpur, Magura|Mohammadpur|Magura|complete
  Mohammadpur|||ambiguous
  kalihati tangail|Kalihati|Tangail|complete
  PO Bhulta, Rupganj, Narayanganj|Rupganj|Narayanganj|complete
  Chandina, Comilla|Chandina|Cumilla|complete
  Uttara Sector 10, Dhaka 1230|Uttara|Dhaka|complete
  Gulshan-2, Dhaka|Gulshan|Dhaka|complete
  Cox's Bazar Sadar|Cox's Bazar Sadar|Cox's Bazar|complete
  Coxs bazar||Cox's Bazar|district only
  B.Baria|||not found
  Sarail, B. Baria|Sarail|Brahmanbaria|complete
  Teknaf|Teknaf|Cox's Bazar|complete
  Bhola, Barisal Division||Bhola|district only
  Bhola, Rangpur Division||Bhola|conflict
  Sylhet-3100|Sylhet Sadar|Sylhet|complete
  Ishwardi, Pabna 6620|Ishwardi|Pabna|complete
  Iswardi, Pabna|Ishwardi|Pabna|complete
  Haziganj, Chandpur|Haziganj|Chandpur|complete
  Hajiganj, Chandpur|Haziganj|Chandpur|complete
  Vill Islampur, Golapganj, Sylhet|Golapganj|Sylhet|complete
  Jamalpur Sadar|Jamalpur Sadar|Jamalpur|complete
  Dakshin Sunamganj|Shantiganj|Sunamganj|complete
  Eidgaon, Cox's Bazar|Eidgaon|Cox's Bazar|complete
  ||| missing
  ", sep = "|", header = TRUE, colClasses = "character", na.strings = character(0),
     strip.white = TRUE, quote = "")
  cases$status[cases$input == ""] <- "missing"

  # "B.Baria" alone is the district Brahmanbaria in the district matcher; the
  # extractor treats a lone abbreviation the same way.
  cases[cases$input == "B.Baria", c("district", "status")] <- c("Brahmanbaria", "district only")
  r <- bd_address(cases$input)
  na2 <- function(v) ifelse(is.na(v), "", v)
  expect_identical(na2(r$thana), cases$thana)
  expect_identical(na2(r$district), cases$district)
  expect_identical(r$status, cases$status)
})

test_that("thanas inside a larger area are found", {
  r <- bd_address(c("Ashulia, Dhaka", "Fatulla, Narayanganj", "Siddirgonj"))
  expect_identical(r$thana, c("Ashulia", "Fatullah", "Siddhirganj"))
  expect_identical(r$district, c("Dhaka", "Narayanganj", "Narayanganj"))
  expect_identical(r$status, rep("complete", 3))
})

test_that("the district argument settles and checks addresses", {
  r <- bd_address(c("Kaliganj", "Kaliganj", "Teknaf, Coxs bazar", "Sadar"),
                  district = c("Jhenaidah", "Satkhira", "Chittagong", "Feni"))
  expect_identical(r$district, c("Jhenaidah", "Satkhira", "Chattogram", "Feni"))
  expect_identical(r$thana, c("Kaliganj", "Kaliganj", NA, "Feni Sadar"))
  expect_identical(r$status, c("complete", "complete", "conflict", "complete"))
  expect_identical(bd_address("Savar", district = "Dhaka")$thana, "Savar")
  # a district name that is a thana of the given district is no conflict
  r2 <- bd_address(c("Faridpur", "Sherpur"), district = c("Pabna", "Bogura"))
  expect_identical(r2$thana, c("Faridpur", "Sherpur"))
  expect_identical(r2$status, c("complete", "complete"))
  # the last thana named is used
  expect_identical(bd_address("PO Ashugonj, Brahmanbaria Sadar, Brahmanbaria")$thana,
                   "Brahmanbaria Sadar")
  expect_error(bd_address(c("a", "b", "c"), district = c("Dhaka", "Feni")), "length")
  expect_warning(bd_address("Savar", district = "Atlantis"), "could not be matched")
})

test_that("results keep order, duplicates and the input", {
  x <- c("Savar, Dhaka", NA, "", "Savar, Dhaka")
  r <- bd_address(x)
  expect_identical(r$input, x)
  expect_identical(r$status, c("complete", "missing", "missing", "complete"))
  expect_identical(names(r), c("input", "postcode", "post_office", "thana",
                               "district", "division", "status", "note"))
})
