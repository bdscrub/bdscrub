#!/usr/bin/env python3
"""Build data-raw/postcodes.csv and data-raw/postcode_areas.csv from
data-raw/postalcodes/bdpost_postcodes_raw.tsv.

The raw file was read from the 64 district post-code pages of bdpost.gov.bd
(Bangladesh Post Office) on 5 Nov 2026; each page has the columns
SL. No | Upazila | Post Office (English) | Post Office (Bangla) | Post Code.
Rows without a post code list villages or areas served by the post office in
the row above; they are kept in postcode_areas.csv with that office's code.

Run from the package folder:  python3 data-raw/make_postcodes.py
"""
import csv, os, re, sys, collections

HERE = os.path.dirname(os.path.abspath(__file__))
RAW = os.path.join(HERE, "postalcodes", "bdpost_postcodes_raw.tsv")
BN = str.maketrans("০১২৩৪৫৬৭৮৯", "0123456789")

# District names as written on the bdpost index page -> standard district.
districts = list(csv.DictReader(open(os.path.join(HERE, "districts.csv"), encoding="utf-8")))
bn2en = {d["district_bn"]: d["district"] for d in districts}
bn2en.update({
    "নারায়নগঞ্জ": "Narayanganj",   # written with dental na
    "নারায়নগঞ্জ": "Narayanganj",
    "ফেণী": "Feni",
    "কুস্টিয়া": "Kushtia",
    "কুস্টিয়া": "Kushtia",
    "মাগুড়া": "Magura",
    "মাগুড়া": "Magura",
    "বরগুণা": "Barguna",
})
div_of = {d["district"]: d["division"] for d in districts}

def norm_bn(s):
    # nukta letters typed as two code points -> one; long and short i and u,
    # and the two na letters, made equal (common typing differences)
    s = (s.replace("\u09a1\u09bc", "\u09dc").replace("\u09a2\u09bc", "\u09dd")
          .replace("\u09af\u09bc", "\u09df"))
    return s.translate(str.maketrans("\u09c0\u09c2\u09a3", "\u09bf\u09c1\u09a8"))
bn2en = {norm_bn(k): v for k, v in bn2en.items()}

def clean(s):
    s = s.replace(" ", " ").replace("`", " ")
    return re.sub(r"\s+", " ", s).strip()

# Corrections to errors on the source pages. Each one is recorded in the
# `note` column of postcodes.csv.
CORRECT = {
    # Listed on the Joypurhat page, but these are Rajshahi city offices: their
    # codes are in the Rajshahi series (6xxx), Joypurhat's are 59xx.
    "6000": dict(district="Rajshahi", postal_upazila="Sadar",
                 note="listed on the Joypurhat page; Rajshahi city office"),
    "6100": dict(district="Rajshahi", postal_upazila="Sadar",
                 note="listed on the Joypurhat page; Rajshahi city office"),
}

offices, areas, issues = [], [], []
pages = collections.OrderedDict()
for line in open(RAW, encoding="utf-8-sig"):
    line = line.rstrip("\n")
    if line.startswith("# ") and "\t" in line:
        name, pid, n = line[2:].split("\t")
        pages[name] = (pid, int(n))
        continue
    if not line or line.startswith("#") or line.startswith("district_bn_page"):
        continue
    cells = line.split("\t")
    page = cells[0]
    rest = [clean(c) for c in cells[1:]]
    if any("Post Code" in c for c in rest):
        last = None
        continue
    if not any(rest):
        continue
    district = bn2en.get(norm_bn(page))
    if district is None:
        sys.exit("unknown district page: " + page)
    upazila, po_en, po_bn, code = rest[-4:]
    code = code.translate(BN).strip()
    if code:
        if not re.fullmatch(r"[0-9]{4}", code):
            issues.append((district, upazila, po_en, code, "post code is not 4 digits"))
            continue
        last = dict(post_code=code, post_office=po_en, post_office_bn=po_bn,
                    postal_upazila=upazila, district=district,
                    division=div_of[district], note="")
        if code in CORRECT:
            last.update(CORRECT[code])
            last["division"] = div_of[last["district"]]
        offices.append(last)
    else:
        if last is None or last["district"] != district:
            issues.append((district, upazila, po_en, "", "area row with no office above it"))
            continue
        if upazila and upazila != last["postal_upazila"]:
            issues.append((district, upazila, po_en, last["post_code"],
                           "area row upazila differs from its office (%s)" % last["postal_upazila"]))
        areas.append(dict(area=po_en, post_code=last["post_code"],
                          postal_upazila=upazila or last["postal_upazila"],
                          district=district))

# ---- checks
codes = collections.Counter(o["post_code"] for o in offices)
for o in offices:
    if o["note"]:
        issues.append((o["district"], o["postal_upazila"], o["post_office"], o["post_code"],
                       "corrected: " + o["note"]))
for c, n in codes.items():
    if n > 1:
        rows = [o for o in offices if o["post_code"] == c]
        issues.append((rows[0]["district"], "", " / ".join(r["post_office"] for r in rows), c,
                       "post code used by %d offices" % n))
# Post codes are grouped by district: the first two digits should be shared
# by the offices of one district (Dhaka city uses 10xx-13xx).
pref = collections.defaultdict(collections.Counter)
for o in offices:
    pref[o["district"]][o["post_code"][:2]] += 1
for o in offices:
    common = pref[o["district"]].most_common(1)[0][0]
    # GPOs and head offices have round codes (1000, 4100, 9000), so skip those.
    if (o["post_code"][:2] != common and pref[o["district"]][o["post_code"][:2]] == 1
            and not o["post_code"].endswith("00")):
        issues.append((o["district"], o["postal_upazila"], o["post_office"], o["post_code"],
                       "first two digits differ from the rest of the district (%s..)" % common))

got = sorted(set(o["district"] for o in offices))
missing = sorted(set(div_of) - set(got))

def write(path, rows, cols):
    with open(path, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=cols)
        w.writeheader(); w.writerows(rows)

write(os.path.join(HERE, "postcodes.csv"), offices,
      ["post_code", "post_office", "post_office_bn", "postal_upazila", "district", "division", "note"])
write(os.path.join(HERE, "postcode_areas.csv"), areas,
      ["area", "post_code", "postal_upazila", "district"])
with open(os.path.join(HERE, "postalcodes", "postcode_issues.csv"), "w", newline="", encoding="utf-8") as fh:
    w = csv.writer(fh); w.writerow(["district", "postal_upazila", "post_office", "post_code", "issue"])
    w.writerows(issues)

print("offices:", len(offices), "| distinct codes:", len(codes), "| area rows:", len(areas))
print("districts covered:", len(got), "| missing:", missing)
print("issues:", len(issues))
for i in issues: print("  ", i)
