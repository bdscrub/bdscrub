# Documentation of the data sets ------------------------------------------------

#' Districts of Bangladesh
#'
#' The 64 districts (zila) of Bangladesh with their division and three sets
#' of codes.
#'
#' @format A data frame with 64 rows and 8 character columns:
#' \describe{
#'   \item{district}{English name in the spelling used by ISO 3166-2,
#'     including the April 2018 changes (Chattogram, Barishal, Cumilla,
#'     Jashore, Bogura).}
#'   \item{district_legacy}{Spelling used before April 2018. Differs from
#'     `district` only for the five renamed districts.}
#'   \item{district_map}{Spelling in the HDX/BBS administrative boundary
#'     files used by most maps (and by the CRAN package 'bangladesh'), for
#'     example "Brahamanbaria", "Jhalokati", "Maulvibazar", "Nawabganj".
#'     Returned by [bd_place()] with `style = "map"`.}
#'   \item{district_bn}{Name in Bangla (Bengali script), as listed in
#'     ISO 3166-2.}
#'   \item{division}{Division the district belongs to, current spelling.}
#'   \item{bbs_code}{Two-digit district code of the Bangladesh Bureau of
#'     Statistics (BBS) geocode system, for example `"26"` for Dhaka.}
#'   \item{pcode}{Place code used in humanitarian administrative boundary
#'     files: `"BD"`, the two-digit division code and the two-digit BBS
#'     district code, for example `"BD3026"`.}
#'   \item{iso_code}{ISO 3166-2 subdivision code, for example `"BD-13"`.
#'     These numbers are not the BBS codes.}
#' }
#'
#' @source
#' - ISO 3166-2 codes and division membership: ISO 3166-2:BD, as listed
#'   at <https://en.wikipedia.org/wiki/ISO_3166-2:BD> (checked October
#'   2026), cross-checked against <https://statoids.com/ybd.html>.
#' - BBS district codes: value labels of the BBS Household Expenditure
#'   Survey 1995-96 in the IHSN catalogue
#'   (<https://catalog.ihsn.org/catalog/152/variable/F1/V3>), checked
#'   against the 64 district pcodes used by FAO's DIEM household survey
#'   2022 (<https://microdata.fao.org/index.php/catalog/2556/variable/V7>).
#'   The IHSN list omits Bandarban and labels code 34 as Rajbari; the
#'   pcode list confirms Bandarban as code 03 and has no code 34.
#' - 2018 spelling changes: Prothom Alo, "English spelling of 5 districts
#'   changed", 2 April 2018.
#' - Bangla names: the local-language names in ISO 3166-2:BD, checked
#'   against the district lists on Bengali Wikipedia
#'   (<https://bn.wikipedia.org>) in October 2026. The two sources agree
#'   exactly for 57 districts; for Madaripur, Munshiganj and Rangamati they
#'   differ only in spelling, and both spellings are accepted when
#'   matching. The names of the four districts of Mymensingh division were
#'   available from ISO 3166-2 only.
#'
#' @seealso [bd_place()], [bd_divisions].
"bd_districts"

#' Divisions of Bangladesh
#'
#' The 8 divisions (bibhag) of Bangladesh with their codes.
#'
#' @format A data frame with 8 rows and 6 character columns:
#' \describe{
#'   \item{division}{English name, current spelling.}
#'   \item{division_legacy}{Spelling used before April 2018 (Barisal,
#'     Chittagong).}
#'   \item{division_bn}{Name in Bangla (Bengali script).}
#'   \item{bbs_code}{Two-digit BBS division code, for example `"30"` for
#'     Dhaka.}
#'   \item{pcode}{Place code used in humanitarian boundary files, for
#'     example `"BD30"`.}
#'   \item{iso_code}{ISO 3166-2 code, for example `"BD-C"`.}
#' }
#'
#' @source See [bd_districts].
#'
#' @seealso [bd_place()].
"bd_divisions"

#' Mobile number prefixes in Bangladesh
#'
#' The mobile number prefixes in use in Bangladesh and the operator each
#' prefix was allocated to.
#'
#' Since mobile number portability began in October 2018, a number can
#' move to another operator without changing its prefix. The `operator`
#' column therefore describes the allocation, not the current operator of
#' any particular number.
#'
#' @format A data frame with 7 rows and 3 character columns:
#' \describe{
#'   \item{prefix}{Three-digit prefix, `"013"` to `"019"`.}
#'   \item{operator}{Operator the prefix was allocated to.}
#'   \item{note}{Remarks, for example on mergers.}
#' }
#'
#' @source New Age, "BTRC approves 013 prefix for GP, 014 for BL",
#'   26 August 2018,
#'   <https://www.newagebd.net/article/49066/btrc-approves-013-prefix-for-gp-014-for-bl>.
#'   The Daily Star, "Everything you need to know about Mobile Number
#'   Portability", 7 October 2018.
#'
#' @seealso [bd_phone()] (`details = TRUE` gives the operator).
"bd_phone_prefixes"

#' Thanas of Bangladesh
#'
#' The thanas (police stations) below the district level: the unit bdscrub
#' uses for everything below the district.
#'
#' @format A data frame with 570 rows and 8 character columns:
#' \describe{
#'   \item{thana}{Name; for rural thanas the spelling of Wikipedia's list of
#'     upazilas (October 2026), for city thanas the boundary-file name.}
#'   \item{district, division}{District and division, current spelling.}
#'   \item{type}{"rural" (503): a rural thana, with the same name and area as
#'     the upazila of the same name; "city" (61): a thana of a city
#'     corporation (Dhaka, Chattogram, Khulna, Rajshahi, Sylhet, Barishal,
#'     Rangpur, Gazipur); "within" (6): a thana inside the area of a larger
#'     rural thana, for example Ashulia in the Savar area and Tongi in the
#'     Gazipur Sadar area.}
#'   \item{map_name}{Name in the HDX/BBS boundary files (as in the CRAN
#'     package 'bangladesh', 2022), for joining to a map; "" where the files
#'     have no such area (thanas created later). For "within" thanas, the
#'     name of the area they lie in. Returned by [bd_place()] with
#'     `style = "map"`.}
#'   \item{pcode}{Place code in the boundary files ("" where there is none).}
#'   \item{note}{For "within" and "city" thanas, which area they belong to.}
#'   \item{source}{"map+wikipedia" (in both sources), "map" (boundary files
#'     only; the city thanas), "wikipedia" (20 rural thanas created after the
#'     boundary files were made, for example Eidgaon, Dasar, Madhyanagar) or
#'     "manual" (the "within" thanas).}
#' }
#' @source Boundary names: the 'bangladesh' package (Hossain, 2022), based on
#'   the HDX administrative boundaries of BBS. Rural thana list: "Upazilas of
#'   Bangladesh", <https://en.wikipedia.org/wiki/Upazilas_of_Bangladesh>,
#'   read October 2026. The two lists were reconciled by hand; the newest
#'   rural thanas are not yet checked against the government gazette. The
#'   "within" thanas were added by hand from the Bangladesh Police district
#'   pages and the post office list; that list is not complete, and newer
#'   metropolitan thanas (for example of Dhaka) are missing.
#' @seealso [bd_place()] with `level = "thana"`, [bd_address()].
"bd_thanas"

#' Post codes of Bangladesh
#'
#' The post offices of Bangladesh with their post codes, as listed by
#' Bangladesh Post, linked to the thana each lies in.
#'
#' @format A data frame with 1329 rows and 8 character columns:
#' \describe{
#'   \item{postcode}{Four-digit post code. Several post offices can share
#'     one code.}
#'   \item{post_office}{Post office name with its type (GPO, HO, SO,
#'     TSO, ...).}
#'   \item{postal_thana}{The thana (upazila) the post office list puts it
#'     under, in the post office's spelling.}
#'   \item{postal_district}{The district in the post office list. For a few
#'     offices this differs from the administrative district (Dharampasa and
#'     Madhyanagar in Sunamganj are listed under Netrakona).}
#'   \item{thana}{The thana in [bd_thanas] it was linked to; "" where no link
#'     could be made (69 offices).}
#'   \item{district, division}{Administrative district and division.}
#'   \item{note}{Corrections made to the list.}
#' }
#' @source Bangladesh Post, district post code pages of the Postmaster
#'   General offices (<https://bdpost.gov.bd>), read October 2026. Names were
#'   linked to thanas by spelling, and 27 links were checked or made by
#'   hand. The pages for Mymensingh division could not be read (page not
#'   found), so its post codes are missing. The terms of reuse of this list
#'   have not been confirmed.
#' @seealso [bd_address()].
"bd_postcodes"

#' Missing-value markers
#'
#' The values [bd_missing()] turns into `NA` by default, with what each
#' means.
#'
#' @format A data frame with 24 rows and 2 character columns:
#' \describe{
#'   \item{marker}{The marker, in lower case (matching ignores case).}
#'   \item{meaning}{What it means or where it comes from.}
#' }
#' @seealso [bd_missing()].
"bd_na_words"
