test_that("all canonical and legacy names map to themselves", {
  d <- bd_districts
  expect_identical(bd_place(d$district), d$district)
  expect_identical(bd_place(d$district_legacy), d$district)
  expect_identical(bd_place(d$district, style = "legacy"), d$district_legacy)
  expect_identical(div_place(bd_divisions$division_legacy),
                   bd_divisions$division)
})

test_that("the five 2018 renamings are handled", {
  old <- c("Chittagong", "Barisal", "Comilla", "Jessore", "Bogra")
  new <- c("Chattogram", "Barishal", "Cumilla", "Jashore", "Bogura")
  expect_identical(bd_place(old), new)
  expect_identical(bd_place(new, style = "legacy"), old)
})

test_that("case, punctuation, suffixes and -gonj variants are ignored", {
  x <- c("DHAKA", " dhaka district ", "Coxs Bazar", "COX'S BAZAR", "cox bazar",
         "Narayangonj Zila", "Munshigonj", "Chapai Nawabganj", "Chapainawabganj",
         "Jhalokati", "Netrokona", "Maulvi Bazar", "Nawabganj")
  expect_identical(bd_place(x),
                   c("Dhaka", "Dhaka", "Cox's Bazar", "Cox's Bazar", "Cox's Bazar",
                     "Narayanganj", "Munshiganj", "Chapai Nawabganj", "Chapai Nawabganj",
                     "Jhalakathi", "Netrakona", "Moulvibazar", "Chapai Nawabganj"))
  expect_identical(div_place(c("Barisal Division", "chittagong div", "RANGPUR")),
                   c("Barishal", "Chattogram", "Rangpur"))
})

test_that("unknown names are NA with a warning, never guessed", {
  expect_warning(out <- bd_place(c("Atlantis", "Kishorgnj", NA, ""), fuzzy = FALSE),
                 "2 non-missing values")
  expect_identical(out, c(NA_character_, NA, NA, NA))
  # "London" in Bangla is not a district
  expect_warning(out_bn <- bd_place(intToUtf8(c(0x09B2, 0x09A8, 0x09CD,
                                                                 0x09A1, 0x09A8))),
                 "1 non-missing value")
  expect_identical(out_bn, NA_character_)
})

test_that("fuzzy matching accepts only close, unique matches", {
  expect_identical(bd_place(c("Kishorgnj", "Narayangnj", "Sylht"), fuzzy = TRUE),
                   c("Kishoreganj", "Narayanganj", "Sylhet"))
  expect_warning(out <- bd_place("Xyzabc", fuzzy = TRUE), "1 non-missing")
  expect_identical(out, NA_character_)
  expect_warning(bd_place("Sylht", fuzzy = TRUE, max_distance = 0))
  expect_error(bd_place("x", max_distance = -1), "max_distance")
})

test_that("details work from names and each kind of code", {
  info <- bd_place(c("Comilla", "26", "26.0", "BD4041", "bd-13", "4", "Nowhere"),
                   details = TRUE, warn = FALSE)
  expect_identical(info$district,
                   c("Cumilla", "Dhaka", NA, "Jashore", "Dhaka", "Barguna", NA))
  expect_identical(info$division[1:2], c("Chattogram", "Dhaka"))
  expect_identical(info$match, c("variant", "code", "not found", "code", "code", "code", "not found"))
  expect_identical(names(info),
                   c("input", "district", "division", "bbs_code", "pcode", "iso_code",
                     "match", "matched_as", "candidates"))
  expect_identical(bd_place(26, details = TRUE)$district, "Dhaka")
  expect_identical(bd_place(26, details = TRUE)$iso_code, "BD-13")
})

test_that("reference tables are internally consistent", {
  d <- bd_districts
  expect_identical(nrow(d), 64L)
  expect_identical(nrow(bd_divisions), 8L)
  expect_false(anyDuplicated(d$bbs_code) > 0)
  div_code <- bd_divisions$bbs_code[match(d$division, bd_divisions$division)]
  expect_identical(d$pcode, paste0("BD", div_code, d$bbs_code))
  expect_true(all(grepl("^BD-[0-9]{2}$", d$iso_code)))
  expect_identical(sort(as.integer(sub("BD-", "", d$iso_code))), 1:64)
})

test_that("Bangla names map to districts and divisions", {
  d <- bd_districts
  expect_identical(bd_place(d$district_bn), d$district)
  expect_identical(div_place(bd_divisions$division_bn), bd_divisions$division)
  expect_identical(bd_place(d$district, style = "bangla"), d$district_bn)
  expect_identical(bd_place(c("Jessore", "Bogra"), style = "bangla"),
                   c("\u09af\u09b6\u09cb\u09b0", "\u09ac\u0997\u09c1\u09a1\u09bc\u09be"))
})

test_that("Bangla suffixes, spaces and spelling variants are ignored", {
  x <- c("\u09a2\u09be\u0995\u09be \u099c\u09c7\u09b2\u09be", "\u099a\u099f\u09cd\u099f\u0997\u09cd\u09b0\u09be\u09ae \u09ac\u09bf\u09ad\u09be\u0997", "\u09b0\u09be\u0999\u09be\u09ae\u09be\u099f\u09bf", "\u09b0\u09be\u0999\u09cd\u0997\u09be\u09ae\u09be\u099f\u09bf",
         "\u09ae\u09be\u09a6\u09be\u09b0\u09bf\u09aa\u09c1\u09b0", "\u09ae\u09be\u09a6\u09be\u09b0\u09c0\u09aa\u09c1\u09b0", "\u09ae\u09c1\u09a8\u09cd\u09b8\u09bf\u0997\u099e\u09cd\u099c", "\u09a8\u09c7\u09a4\u09cd\u09b0\u0995\u09cb\u09a8\u09be",
         "\u099a\u09be\u0981\u09aa\u09be\u0987 \u09a8\u09ac\u09be\u09ac\u0997\u099e\u09cd\u099c", " \u0995\u0995\u09cd\u09b8 \u09ac\u09be\u099c\u09be\u09b0 ")
  expect_identical(bd_place(x),
                   c("Dhaka", "Chattogram", "Rangamati", "Rangamati", "Madaripur",
                     "Madaripur", "Munshiganj", "Netrakona", "Chapai Nawabganj",
                     "Cox's Bazar"))
  expect_identical(div_place("\u09b0\u09be\u099c\u09b6\u09be\u09b9\u09c0 \u09ac\u09bf\u09ad\u09be\u0997"), "Rajshahi")
})

test_that("Bangla letters typed in different Unicode forms still match", {
  # bogura with rra as one code point (U+09DC) and as dda + nukta
  one <- intToUtf8(c(0x09AC, 0x0997, 0x09C1, 0x09DC, 0x09BE))
  two <- intToUtf8(c(0x09AC, 0x0997, 0x09C1, 0x09A1, 0x09BC, 0x09BE))
  expect_identical(bd_place(c(one, two)), c("Bogura", "Bogura"))
  # joypurhat with ya + nukta as one code point (U+09DF)
  joy <- intToUtf8(c(0x099C, 0x09DF, 0x09AA, 0x09C1, 0x09B0, 0x09B9, 0x09BE, 0x099F))
  expect_identical(bd_place(joy), "Joypurhat")
  # noakhali with o-kar typed as e-kar + aa-kar, and a zero-width joiner
  noa <- intToUtf8(c(0x09A8, 0x09C7, 0x09BE, 0x09AF, 0x09BC, 0x09BE, 0x200D,
                     0x0996, 0x09BE, 0x09B2, 0x09C0))
  expect_identical(bd_place(noa), "Noakhali")
})

test_that("details use the chosen style", {
  info <- bd_place("Comilla", style = "bangla", details = TRUE)
  expect_identical(info$district, "\u0995\u09c1\u09ae\u09bf\u09b2\u09cd\u09b2\u09be")
  expect_identical(bd_place("Brahmanbaria", style = "map"), "Brahamanbaria")
})

test_that("no two districts share a matching key", {
  d <- bd_districts
  keys <- c(d$district, d$district_legacy, d$district_bn)
  out <- bd_place(keys)
  expect_identical(out, rep(d$district, 3))
})

test_that("punctuation between words acts as a separator", {
  x <- c("Dhaka-Zila", "Dhaka(District)", "dhaka/zila", "Chapai-Nawabganj", "Kishor-ganj",
         "COX\u2019S BAZAR", "Cox's Bazar", "Dhaka.", "\ufeffDhaka")
  expect_identical(bd_place(x),
                   c("Dhaka", "Dhaka", "Dhaka", "Chapai Nawabganj", "Kishoreganj",
                     "Cox's Bazar", "Cox's Bazar", "Dhaka", "Dhaka"))
  expect_identical(div_place("Barisal-Division"), "Barishal")
})

test_that("common abbreviations and division variants are recognised", {
  expect_identical(bd_place(c("B.Baria", "B. Baria", "Ctg", "Cox's Bazaar")),
                   c("Brahmanbaria", "Brahmanbaria", "Chattogram", "Cox's Bazar"))
  expect_identical(div_place(c("Rongpur", "Dacca", "Ctg")),
                   c("Rangpur", "Dhaka", "Chattogram"))
})

test_that("fuzzy matching does not match unrelated short words", {
  x <- c("city", "wntg", "diaza", "road", "bank")
  expect_warning(out <- bd_place(x, fuzzy = TRUE), "5 non-missing")
  expect_true(all(is.na(out)))
  expect_identical(bd_place(c("tangile", "sylet", "chittagang"), fuzzy = TRUE),
                   c("Tangail", "Sylhet", "Chattogram"))
})

test_that("changed names are reported in a message", {
  expect_message(out <- bd_place(c("Nababganj", "Nababganj", "Dhaka"),
                                                fuzzy = TRUE),
                 "\"Nababganj\" -> \"Chapai Nawabganj\" \\(typing error, corrected by fuzzy matching; 2 values\\)")
  expect_identical(out, c("Chapai Nawabganj", "Chapai Nawabganj", "Dhaka"))
  expect_message(bd_place("Jhalokati"), "1 value was not written")
  expect_message(div_place("Rongpur"), "\"Rongpur\" -> \"Rangpur\"")
  # Official names, case, spaces and suffixes are not reported.
  expect_silent(bd_place(c("Dhaka", "khulna district", " BOGURA ")))
  # Old spellings and Bangla only with report = "all".
  expect_silent(bd_place("Chittagong"))
  expect_message(bd_place("Chittagong", report = "all"), "spelling used before 2018")
  expect_silent(bd_place("Chittagong", style = "legacy", report = "all"))
  expect_silent(bd_place("Nawabganj", report = "none"))
  expect_error(bd_place("Dhaka", report = "some"))
  # The result carries no extra attributes.
  expect_null(attributes(suppressMessages(bd_place("Nawabganj"))))
  expect_identical(bd_place(character(0)), character(0))
})

test_that("bd_clean() reports per column and bd_check() stays silent", {
  df <- data.frame(zila = c("Nawabganj", "Dhaka"), div = c("Dacca", "Khulna"),
                   stringsAsFactors = FALSE)
  expect_message(bd_clean(df, c(zila = "district")), "^zila: 1 value")
  expect_message(bd_clean(df, c(div = "division")), "^div: 1 value")
  expect_silent(bd_clean(df, c(zila = "district", div = "division"), report = "none"))
  expect_silent(bd_check(df, c(zila = "district", div = "division")))
})

test_that("Bangla spellings with the other na or sa letter match", {
  # as written on the Bangladesh Post website: Feni and Barguna with nna,
  # Kushtia with sa instead of ssa, Narayanganj with na, Magura with rra
  x <- c("\u09ab\u09c7\u09a3\u09c0", "\u09ac\u09b0\u0997\u09c1\u09a3\u09be",
         "\u0995\u09c1\u09b8\u09cd\u099f\u09bf\u09af\u09bc\u09be",
         "\u09a8\u09be\u09b0\u09be\u09af\u09bc\u09a8\u0997\u099e\u09cd\u099c",
         "\u09ae\u09be\u0997\u09c1\u09dc\u09be")
  expect_identical(suppressMessages(bd_place(x)),
                   c("Feni", "Barguna", "Kushtia", "Narayanganj", "Magura"))
  # the folding never makes two districts share a key
  k <- .name_key(c(bd_districts$district, bd_districts$district_bn))
  expect_true(all(tapply(rep(bd_districts$district, 2), k, function(z) length(unique(z))) == 1L))
})
