test_that("thanas match with and without a district", {
  out <- suppressWarnings(bd_place(c("Kaliganj", "Kaliganj", "Sadar", "Savar", "Teknaf"),
                                   level = "thana", district = c("Gazipur", NA, "Feni", NA, NA)))
  expect_identical(out, c("Kaliganj", NA, "Feni Sadar", "Savar", "Teknaf"))
  d <- bd_place(c("Kaliganj", "sadar"), level = "thana", details = TRUE, warn = FALSE)
  expect_identical(d$match, c("ambiguous", "ambiguous"))
  expect_match(d$candidates[1], "Kaliganj \\(Gazipur\\)")
  expect_match(d$candidates[2], "give the district")
})

test_that("upazila names and words are read as thanas", {
  out <- bd_place(c("Savar Upazila", "savar upazilla", "Savar P.S.", "Savar Thana"),
                  level = "thana", report = "none")
  expect_identical(out, rep("Savar", 4))
  expect_error(bd_place("Savar", level = "upazila"), "should be one of")
})

test_that("a thana outside the given district is not matched", {
  expect_warning(out <- bd_place("Teknaf", level = "thana", district = "Dhaka"), "1 non-missing")
  expect_identical(out, NA_character_)
})

test_that("thana spellings, typing errors and map names", {
  out <- suppressMessages(bd_place(c("Iswardi", "Hajiganj", "Savr", "Brahmanbaria Sadar"),
                                   level = "thana",
                                   district = c("Pabna", "Chandpur", "Dhaka", "B. Baria")))
  expect_identical(out, c("Ishwardi", "Haziganj", "Savar", "Brahmanbaria Sadar"))
  expect_message(bd_place("Savr", level = "thana", district = "Dhaka"), "typing error")
  expect_false(is.na(bd_place("Brahmanbaria Sadar", level = "thana", style = "map")))
  expect_identical(suppressWarnings(bd_place("Eidgaon", level = "thana", style = "map")),
                   NA_character_)
  expect_warning(bd_place("Savar", level = "thana", style = "bangla"), "Bangla names")
  # "within" thanas get the map area they lie in, with a message
  expect_message(m <- bd_place("Ashulia", level = "thana", style = "map"), "area it lies in")
  expect_identical(m, bd_thanas$map_name[bd_thanas$thana == "Savar"])
})

test_that("thana details: rural, within and city thanas", {
  d <- bd_place(c("Ashulia", "Fatulla", "Savar", "Mirpur", "Double Mooring"), level = "thana",
                district = c("Dhaka", "Narayanganj", "Dhaka", "Dhaka", "Chittagong"),
                details = TRUE, report = "none")
  expect_identical(d$thana, c("Ashulia", "Fatullah", "Savar", "Mirpur", "Double Mooring"))
  expect_identical(d$type, c("within", "within", "rural", "city", "city"))
  expect_match(d$note[1], "Savar area")
  expect_match(d$note[4], "city corporation")
  expect_true(is.na(d$note[3]))
  expect_identical(names(d), c("input", "thana", "district", "division", "type", "map_name",
                               "pcode", "match", "matched_as", "candidates", "note"))
})

test_that("reference tables are consistent", {
  expect_identical(nrow(bd_thanas), 570L)
  expect_identical(as.vector(table(bd_thanas$type)[c("rural", "city", "within")]), c(503L, 61L, 6L))
  expect_false(anyDuplicated(paste(bd_thanas$district, bd_thanas$thana)) > 0)
  expect_true(all(bd_postcodes$thana[bd_postcodes$thana != ""] %in% bd_thanas$thana))
  expect_true(all(grepl("^[0-9]{4}$", bd_postcodes$postcode)))
  expect_true(all(bd_postcodes$district %in% bd_districts$district))
  expect_identical(bd_na_words$marker, .tab_na_words)
  expect_false(exists("bd_upazilas"))
})
