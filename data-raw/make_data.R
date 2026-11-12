# Builds the package data from the CSV files in data-raw/.
# Run from the package root:  source("data-raw/make_data.R")
#
# Where the CSV files come from (details in R/data.R and in data-raw/sources/):
#   districts.csv, divisions.csv      BBS codes, pcodes, ISO 3166-2, Bangla names
#   district_aliases.csv,
#   division_aliases.csv              spellings found in survey and admin data
#   phone_prefixes.csv                BTRC mobile prefix allocations
#   upazilas.csv                      rural and city thanas: boundary-map names (CRAN
#                                     package 'bangladesh', where rural thanas appear
#                                     as upazilas) reconciled with Wikipedia
#                                     (sources/make_upazilas.py)
#   upazila_aliases.csv               boundary-map and post office spellings of them
#   postcodes.csv, postcode_areas.csv Bangladesh Post district pages
#                                     (sources/make_postcodes.py, links checked by hand)
#   thana_within.csv                  thanas inside the area of a larger rural thana
#   na_words.csv                      missing-value markers, with their meaning

read_tab <- function(file) {
  utils::read.csv(file.path("data-raw", file), colClasses = "character",
                  stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = character(0))
}

bd_districts <- read_tab("districts.csv")
bd_divisions <- read_tab("divisions.csv")
bd_phone_prefixes <- read_tab("phone_prefixes.csv")
dist_alias <- read_tab("district_aliases.csv")
div_alias <- read_tab("division_aliases.csv")
upazilas <- read_tab("upazilas.csv")
upz_alias <- read_tab("upazila_aliases.csv")
postcodes <- read_tab("postcodes.csv")
post_areas <- read_tab("postcode_areas.csv")
within <- read_tab("thana_within.csv")
na_words <- read_tab("na_words.csv")

# ---- Thanas ---------------------------------------------------------------------
# The package works with thanas (police stations) below the district. The raw
# tables come from upazila sources, so they are turned into one thana table:
#   rural  - a rural thana; it has the same name and area as the upazila of
#            the boundary files (503);
#   within - a thana inside the area of a larger rural thana, for example
#            Ashulia inside Savar (thana_within.csv; not in the boundary
#            files, so its map name is that of the area it lies in);
#   city   - a thana of a city corporation (61).
city <- upazilas[upazilas$type == "city thana", ]
rural <- upazilas[upazilas$type == "upazila", ]
area_map <- rural$map_name[match(paste(within$district, within$upazila),
                                 paste(rural$district, rural$upazila))]
area_map[area_map %in% ""] <- within$upazila[area_map %in% ""]
bd_thanas <- rbind(
  data.frame(thana = rural$upazila, district = rural$district, type = "rural",
             map_name = rural$map_name, pcode = rural$pcode, note = "",
             source = rural$source, stringsAsFactors = FALSE),
  data.frame(thana = within$thana, district = within$district, type = "within",
             map_name = area_map, pcode = "",
             note = ifelse(area_map == within$upazila,
                           paste0("part of the ", within$upazila, " area"),
                           paste0("part of the ", within$upazila, " area (", area_map,
                                  " on maps)")),
             source = "manual", stringsAsFactors = FALSE),
  data.frame(thana = city$upazila, district = city$district, type = "city",
             map_name = city$map_name, pcode = city$pcode,
             note = paste(city$district, "city corporation area"),
             source = city$source, stringsAsFactors = FALSE)
)
bd_thanas$division <- bd_districts$division[match(bd_thanas$district, bd_districts$district)]
bd_thanas <- bd_thanas[, c("thana", "district", "division", "type", "map_name", "pcode",
                           "note", "source")]
# Other spellings: boundary-file and post office spellings of the rural and
# city thanas, and the spellings of the "within" thanas.
thana_alias <- rbind(
  data.frame(alias = upz_alias$alias, thana = upz_alias$upazila, district = upz_alias$district,
             kind = ifelse(upz_alias$source == "postal spelling", "postal spelling", "variant"),
             stringsAsFactors = FALSE),
  do.call(rbind, lapply(seq_len(nrow(within)), function(i) {
    w <- strsplit(within$also_written[i], ";", fixed = TRUE)[[1]]
    if (!length(w)) return(NULL)
    data.frame(alias = w, thana = within$thana[i], district = within$district[i],
               kind = "variant", stringsAsFactors = FALSE)
  }))
)

# ---- Corrections to the post office list ------------------------------------------
# Typing errors on the Bangladesh Post pages (the raw file keeps them as read).
po_fix <- c("Dhaka Cen,ft. TSO" = "Dhaka Cantt. TSO", "Brah,anbaria HO" = "Brahmanbaria HO")
hit <- postcodes$post_office %in% names(po_fix)
postcodes$note[hit] <- paste0("name corrected from \"", postcodes$post_office[hit],
                              "\" on the Bangladesh Post page")
postcodes$post_office[hit] <- po_fix[postcodes$post_office[hit]]

# ---- Exported tables ------------------------------------------------------------
bd_postcodes <- postcodes[, c("post_code", "post_office", "postal_upazila", "district",
                              "upazila", "upazila_district", "note")]
names(bd_postcodes) <- c("postcode", "post_office", "postal_thana", "postal_district",
                         "thana", "district", "note")
no_link <- bd_postcodes$district == ""
bd_postcodes$district[no_link] <- bd_postcodes$postal_district[no_link]
bd_postcodes$division <- bd_districts$division[match(bd_postcodes$district, bd_districts$district)]
bd_postcodes <- bd_postcodes[, c("postcode", "post_office", "postal_thana", "postal_district",
                                 "thana", "district", "division", "note")]
bd_na_words <- na_words

# ---- Consistency checks: fail loudly rather than ship wrong reference data ------
stopifnot(
  nrow(bd_districts) == 64L, nrow(bd_divisions) == 8L,
  !anyDuplicated(bd_districts$district), !anyDuplicated(bd_districts$bbs_code),
  !anyDuplicated(bd_districts$pcode), !anyDuplicated(bd_districts$iso_code),
  all(bd_districts$division %in% bd_divisions$division),
  all(dist_alias$district %in% bd_districts$district),
  all(div_alias$division %in% bd_divisions$division),
  all(grepl("^[\u0980-\u09ff ]+$", bd_districts$district_bn, perl = TRUE)),
  all(grepl("^[\u0980-\u09ff ]+$", bd_divisions$division_bn, perl = TRUE)),
  !anyDuplicated(bd_districts$district_bn),
  all(bd_districts$pcode == paste0(
    "BD", bd_divisions$bbs_code[match(bd_districts$division, bd_divisions$division)],
    bd_districts$bbs_code)),
  # thanas
  all(bd_thanas$district %in% bd_districts$district),
  !anyDuplicated(paste(bd_thanas$district, bd_thanas$thana)),
  !anyDuplicated(bd_thanas$pcode[bd_thanas$pcode != ""]),
  sum(bd_thanas$type == "rural") == 503L, sum(bd_thanas$type == "city") == 61L,
  all(paste(within$district, within$upazila) %in% paste(rural$district, rural$upazila)),
  all(paste(thana_alias$district, thana_alias$thana) %in% paste(bd_thanas$district, bd_thanas$thana)),
  # post codes
  nrow(bd_postcodes) == 1329L,
  all(grepl("^[0-9]{4}$", bd_postcodes$postcode)),
  all(bd_postcodes$district %in% bd_districts$district),
  all(paste(bd_postcodes$district, bd_postcodes$thana)[bd_postcodes$thana != ""] %in%
        paste(bd_thanas$district, bd_thanas$thana)),
  !anyNA(bd_postcodes$division)
)

# Exported data sets (data/)
for (obj in c("bd_districts", "bd_divisions", "bd_thanas", "bd_postcodes",
              "bd_phone_prefixes", "bd_na_words")) {
  save(list = obj, file = file.path("data", paste0(obj, ".rda")), compress = "bzip2",
       version = 2)
}

# Internal copies used by the functions (R/sysdata.rda)
.tab_districts <- bd_districts
.tab_divisions <- bd_divisions
.tab_prefixes <- bd_phone_prefixes
.tab_dist_alias <- dist_alias
.tab_div_alias <- div_alias
.tab_thanas <- bd_thanas
# Internal only: for "within" thanas, the rural thana whose area they lie in
# (used by bd_address() when an address names both, "Ashulia, Savar").
.tab_thanas$area <- ""
.tab_thanas$area[.tab_thanas$type == "within"] <- within$upazila
.tab_thana_alias <- thana_alias
.tab_postcodes <- bd_postcodes
.tab_na_words <- na_words$marker
save(.tab_districts, .tab_divisions, .tab_prefixes, .tab_dist_alias, .tab_div_alias,
     .tab_thanas, .tab_thana_alias, .tab_postcodes,
     .tab_na_words, file = "R/sysdata.rda", compress = "bzip2", version = 2)
cat("data built:", nrow(bd_thanas), "thanas (", paste(names(table(bd_thanas$type)), table(bd_thanas$type), collapse = ", "), "),",
    nrow(bd_postcodes), "post offices\n")
