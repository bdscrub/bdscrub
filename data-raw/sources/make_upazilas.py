#!/usr/bin/env python3
"""Build data-raw/upazilas.csv and data-raw/upazila_aliases.csv.

Sources (both in data-raw/upazila_sources/):
  upazila_map.csv        name table of the upazila boundary map in the CRAN
                         package 'bangladesh' 1.0.0 (2022; BBS/HDX boundaries),
                         544 upazilas and city thanas with ADM3 pcodes;
  wikipedia_upazilas.txt the upazila lists on en.wikipedia.org/wiki/Upazila,
                         read 5 Nov 2026 (503 upazilas, includes recent ones).

The standard spelling is Wikipedia's for upazilas (it includes renamings)
and the map's for city thanas (Wikipedia does not list them). Every other
spelling becomes an alias. The pcode comes from the map; upazilas created
after the map was made have none.

Run from the package folder:  python3 data-raw/make_upazilas.py
"""
import csv, os, re, collections

HERE = os.path.dirname(os.path.abspath(__file__))
SRC = os.path.join(HERE, "upazila_sources")

districts = list(csv.DictReader(open(os.path.join(HERE, "districts.csv"), encoding="utf-8")))
div_of = {d["district"]: d["division"] for d in districts}
DISTRICT = {d["district"].lower(): d["district"] for d in districts}
DISTRICT.update({d["district_legacy"].lower(): d["district"] for d in districts})
DISTRICT.update({"chittagong": "Chattogram", "jhenaida": "Jhenaidah", "jhalokati": "Jhalakathi",
                 "netrokona": "Netrakona", "nawabganj": "Chapai Nawabganj",
                 "maulvibazar": "Moulvibazar", "khagrachari": "Khagrachhari",
                 "brahamanbaria": "Brahmanbaria"})

def district(s):
    return DISTRICT[s.replace(" District", "").strip().lower()]

# Same upazila, different spelling: Wikipedia name -> map name (checked by hand).
SAME = {
    ("Naogaon", "Mohadevpur"): "Mahadebpur", ("Rajshahi", "Bagmara"): "Baghmara",
    ("Sirajganj", "Raiganj"): "Royganj", ("Dinajpur", "Phulbari"): "Fulbari",
    ("Gaibandha", "Phulchhari"): "Fulchhari", ("Gaibandha", "Sughatta"): "Saghatta",
    ("Kurigram", "Raomari"): "Raumari", ("Panchagarh", "Tetulia"): "Tentulia",
    ("Rangpur", "Gangachhara"): "Gangachara", ("Jamalpur", "Baksiganj"): "Bakshiganj",
    ("Mymensingh", "Gafargaon"): "Gaffargaon", ("Jhalakathi", "Kathalia"): "Kanthalia",
    ("Brahmanbaria", "Bancharampur"): "Banchharampur", ("Chandpur", "Haziganj"): "Hajiganj",
    ("Chattogram", "Anwara"): "Anowara", ("Cumilla", "Monohargonj"): "Manoharganj",
    ("Lakshmipur", "Raipur"): "Roypur", ("Rangamati", "Bagaichhari"): "Baghai Chhari",
    ("Rangamati", "Naniyachar"): "Naniarchar", ("Manikganj", "Shivalaya"): "Shibalaya",
    ("Munshiganj", "Sirajdikhan"): "Serajdikhan", ("Narsingdi", "Monohardi"): "Manohardi",
    ("Narsingdi", "Raipura"): "Roypura", ("Shariatpur", "Zajira"): "Zanjira",
    ("Khulna", "Rupsha"): "Rupsa", ("Habiganj", "Baniyachong"): "Baniachong",
    ("Moulvibazar", "Moulvibazar Sadar"): "Maulvi Bazar Sadar",
    ("Sunamganj", "Bishwamvarpur"): "Bishwambarpur", ("Sunamganj", "Dharamapasha"): "Dharampasha",
    ("Sunamganj", "Sullah"): "Sulla", ("Sylhet", "Golapganj"): "Golabganj",
    # renamed since the map was made
    ("Jashore", "Jashore Sadar"): "Kotwali",            # Jashore Sadar, "Kotwali" in BBS files
    ("Pirojpur", "Indurkani"): "Zianagar",              # renamed Indurkani
    ("Sunamganj", "Shantiganj"): "Dakshin Sunamganj",   # renamed Shantiganj
}

def key(name, dist):
    s = re.sub(r"\(.*?\)", "", name.lower()).replace("gonj", "ganj")
    s = s.replace("sadar", "").replace("adarsha", "")
    s = re.sub(r"[^a-z]", "", s)
    for old, new in [("bogra", "bogura"), ("jessore", "jashore"), ("barisal", "barishal"),
                     ("comilla", "cumilla"), ("chittagong", "chattogram"), ("jhalokati", "jhalakathi")]:
        s = s.replace(old, new)
    return "SADAR" if s in ("", re.sub(r"[^a-z]", "", dist.lower())) else s

mp = list(csv.DictReader(open(os.path.join(SRC, "upazila_map.csv"), encoding="utf-8")))
for r in mp:
    r["dist"] = district(r["District"])
map_by_key = {(r["dist"], key(r["Upazila"], r["dist"])): r for r in mp}
map_by_name = {(r["dist"], r["Upazila"]): r for r in mp}

rows, used = [], set()
for line in open(os.path.join(SRC, "wikipedia_upazilas.txt"), encoding="utf-8"):
    if line.startswith("#") or not line.strip():
        continue
    d, ups = line.rstrip("\n").split(" || ")
    dist = district(d)
    for u in ups.split(" ; "):
        name = u.replace(" Upazila", "").strip()
        m = map_by_name.get((dist, SAME[(dist, name)])) if (dist, name) in SAME else \
            map_by_key.get((dist, key(name, dist)))
        if m:
            used.add(m["ADM3_PCODE"])
        rows.append(dict(upazila=name, district=dist, division=div_of[dist],
                         type="upazila", pcode=m["ADM3_PCODE"] if m else "",
                         map_name=m["Upazila"] if m else "",
                         source="map+wikipedia" if m else "wikipedia"))
for r in mp:
    if r["ADM3_PCODE"] not in used:
        rows.append(dict(upazila=r["Upazila"], district=r["dist"], division=div_of[r["dist"]],
                         type="city thana", pcode=r["ADM3_PCODE"], map_name=r["Upazila"],
                         source="map"))

# Checks
pc = collections.Counter(r["pcode"] for r in rows if r["pcode"])
assert all(n == 1 for n in pc.values()), "pcode used twice"
assert len({(r["district"], r["upazila"]) for r in rows}) == len(rows), "duplicate name"
thanas = [r for r in rows if r["type"] == "city thana"]
assert {r["district"] for r in thanas} <= {"Dhaka", "Chattogram", "Khulna", "Rajshahi"}, \
    sorted({r["district"] for r in thanas})
assert all(r["pcode"][:4] == next(d["pcode"][2:] for d in districts if d["district"] == r["district"])
           for r in rows if r["pcode"]), "pcode does not start with the district code"

rows.sort(key=lambda r: (r["division"], r["district"], r["type"], r["upazila"]))
cols = ["upazila", "district", "division", "type", "pcode", "map_name", "source"]
with open(os.path.join(HERE, "upazilas.csv"), "w", newline="", encoding="utf-8") as fh:
    w = csv.DictWriter(fh, fieldnames=cols); w.writeheader(); w.writerows(rows)

aliases = [dict(alias=r["map_name"], upazila=r["upazila"], district=r["district"],
                source="boundary map spelling")
           for r in rows if r["map_name"] and r["map_name"] != r["upazila"]]
with open(os.path.join(HERE, "upazila_aliases.csv"), "w", newline="", encoding="utf-8") as fh:
    w = csv.DictWriter(fh, fieldnames=["alias", "upazila", "district", "source"])
    w.writeheader(); w.writerows(aliases)

c = collections.Counter((r["type"], r["source"]) for r in rows)
print("rows:", len(rows), dict(c))
print("aliases:", len(aliases))
print("new since the map (no pcode):", sorted(r["district"] + "/" + r["upazila"] for r in rows if not r["pcode"]))
shared = collections.defaultdict(set)
for r in rows:
    shared[r["upazila"]].add(r["district"])
print("names in more than one district:", {k: sorted(v) for k, v in shared.items() if len(v) > 1})
