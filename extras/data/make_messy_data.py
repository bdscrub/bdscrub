"""Make the messy demonstration data for bdscrub, with its answer key.

Every row starts from a true record drawn from the package's own reference
tables (a real district, thana and post code that belong together,
and a made-up person). The true values go to the answer key; the survey file
gets the same values written the ways they are written in real Bangladeshi
data: other spellings, typing errors, Bangla script, suffixes, several phone
numbers in one cell, lakh and crore, ranges, missing-value markers.

The names of people are random combinations of common first names and
surnames; they are not real people. Phone numbers are random.

Usage:  python3 make_messy_data.py <bdscrub/data-raw folder> <output folder>
Output: bdscrub_messy_survey.csv (the data) and bdscrub_answer_key.csv.
"""
import csv
import os
import random
import sys

SEED = 2026
N = 3000

raw, out = sys.argv[1], sys.argv[2]
rng = random.Random(SEED)


def read(name):
    with open(os.path.join(raw, name), encoding="utf-8") as f:
        return list(csv.DictReader(f))


districts = read("districts.csv")
divisions = read("divisions.csv")
dist_alias = read("district_aliases.csv")
div_alias = read("division_aliases.csv")
upazilas = read("upazilas.csv")
upz_alias = read("upazila_aliases.csv")
within = read("thana_within.csv")
postcodes = read("postcodes.csv")

dist_by = {d["district"]: d for d in districts}
div_by = {d["division"]: d for d in divisions}
aliases_of_dist = {}
for a in dist_alias:
    aliases_of_dist.setdefault(a["district"], []).append(a["alias"])
aliases_of_div = {}
for a in div_alias:
    aliases_of_div.setdefault(a["division"], []).append(a["alias"])
aliases_of_upz = {}
for a in upz_alias:
    aliases_of_upz.setdefault((a["district"], a["upazila"]), []).append(a["alias"])
# Post office spellings that are really the name of a thana inside the area
# ("Fatulla" for Narayanganj Sadar) are not used as spellings of the area.
within_names = set()
for w in within:
    within_names.add(w["thana"].lower())
    within_names.update(x.lower() for x in w["also_written"].split(";") if x)
for k in list(aliases_of_upz):
    aliases_of_upz[k] = [x for x in aliases_of_upz[k] if x.lower() not in within_names]
pc_of_upz = {}
for p in postcodes:
    if p["upazila"]:
        pc_of_upz.setdefault((p["upazila_district"] or p["district"], p["upazila"]), []).append(p)

# Upazila names used in several districts: an address with only such a name
# is ambiguous, which the answer key must allow for.
upz_count = {}
for u in upazilas:
    upz_count[u["upazila"]] = upz_count.get(u["upazila"], 0) + 1
for w in within:
    upz_count[w["thana"]] = upz_count.get(w["thana"], 0) + 1

BN_DIGITS = "০১২৩৪৫৬৭৮৯"


def bn(s):
    return "".join(BN_DIGITS[int(c)] if c.isdigit() else c for c in s)


def typo(s):
    """One typing error (drop, double or swap a letter) inside a long word."""
    if len(s) < 7:
        return s
    i = rng.randrange(1, len(s) - 2)
    kind = rng.choice(["drop", "double", "swap"])
    if kind == "drop":
        return s[:i] + s[i + 1:]
    if kind == "double":
        return s[:i] + s[i] + s[i:]
    return s[:i] + s[i + 1] + s[i] + s[i + 2:]


def case(s):
    return rng.choice([s, s, s.lower(), s.upper(), " " + s + "  "])


# ---- true records -------------------------------------------------------------
# Thanas: rural thanas (same name and area as the upazila), "within" and city thanas.
rural = [u for u in upazilas if u["type"] == "upazila"]
city = [u for u in upazilas if u["type"] == "city thana"]


def draw_place():
    r = rng.random()
    if r < 0.80:
        u = rng.choice(rural)
        return dict(area=u["upazila"], thana=u["upazila"], district=u["district"],
                    relation="rural", map_name=u["map_name"])
    if r < 0.85:
        w = rng.choice(within)
        return dict(area=w["upazila"], thana=w["thana"], district=w["district"],
                    relation="within", map_name="", also=w["also_written"])
    u = rng.choice(city)
    return dict(area=u["upazila"], thana=u["upazila"], district=u["district"], relation="city",
                map_name=u["map_name"])


FIRST = ["Rahim", "Karim", "Fatema", "Ayesha", "Nusrat", "Tanvir", "Sabbir", "Mehedi", "Rafiq",
         "Shirin", "Jannatul", "Sumaiya", "Arif", "Habib", "Rina", "Shahana", "Kamal", "Jamal",
         "Farhana", "Mahmud", "Nasrin", "Rubel", "Sharmin", "Imran", "Taslima", "Sohel", "Moushumi",
         "Anwar", "Rokeya", "Shafiq", "Tania", "Masud", "Lipi", "Ripon", "Nazma", "Hasan"]
LAST = ["Hossain", "Ahmed", "Islam", "Rahman", "Khatun", "Akter", "Begum", "Uddin", "Chowdhury",
        "Sarkar", "Mia", "Das", "Roy", "Sheikh", "Talukder", "Mollah", "Bhuiyan", "Haque",
        "Karim", "Biswas", "Paul", "Sultana", "Alam", "Kabir"]
PREFIXES = ["013", "014", "015", "016", "017", "018", "019"]


def true_phone():
    return rng.choice(PREFIXES) + "".join(str(rng.randrange(10)) for _ in range(8))


# ---- messy versions --------------------------------------------------------------
def messy_phone(p):
    """Return (written value, phone the cleaner should give, note)."""
    r = rng.random()
    core = p[1:]
    if r < 0.25:
        return p, p, ""
    if r < 0.35:
        return "+880 " + core[:4] + "-" + core[4:], p, ""
    if r < 0.42:
        return "880" + core, p, ""
    if r < 0.50:
        return core, p, "0 dropped by a spreadsheet"
    if r < 0.56:
        return p[:3] + "-" + p[3:7] + "-" + p[7:], p, ""
    if r < 0.61:
        return bn(p), p, "Bangla digits"
    if r < 0.66:
        return "'" + p, p, "Excel text apostrophe"
    if r < 0.72:
        q = true_phone()
        sep = rng.choice([" / ", ", ", "; ", " or ", "/", " "])
        return p + sep + q, p, "two numbers (key = first)"
    if r < 0.74:
        return "Mobile: " + p, p, ""
    if r < 0.75:
        return "880" + core + ".0", p, "spreadsheet number with .0"
    if r < 0.78:
        return "+88" + core, "", "+881 typing error"
    if r < 0.82:
        return p[:-1], "", "one digit missing"
    if r < 0.84:
        return "012" + p[3:], "", "not a mobile prefix"
    if r < 0.86:
        return "%.5E" % float(core), "", "Excel scientific notation"
    if r < 0.92:
        return rng.choice(["N/A", "-", "", "n/a", "#N/A", "not given"]), "", "missing"
    return p, p, ""


def messy_amount():
    """Return (written value, exact amount or '', low, high, note)."""
    r = rng.random()
    v = rng.choice([8000, 12000, 15000, 18000, 20000, 22000, 25000, 30000, 35000, 40000,
                    45000, 50000, 60000, 75000, 80000, 100000, 120000, 150000, 250000])

    def grp(x):
        s = str(x)
        if len(s) <= 3:
            return s
        head, tail = s[:-3], s[-3:]
        parts = []
        while len(head) > 2:
            parts.insert(0, head[-2:])
            head = head[:-2]
        if head:
            parts.insert(0, head)
        return ",".join(parts) + "," + tail

    if r < 0.12:
        return str(v), v, v, v, ""
    if r < 0.15:
        return str(v) + rng.choice([" per month", " taka monthly", "/month"]), v, v, v, "time period"
    if r < 0.28:
        return "Tk " + grp(v), v, v, v, ""
    if r < 0.36:
        return grp(v) + "/-", v, v, v, ""
    if r < 0.44 and v % 1000 == 0:
        return "%dk" % (v // 1000), v, v, v, ""
    if r < 0.52 and v >= 100000:
        return ("%g lakh" % (v / 100000)), v, v, v, ""
    if r < 0.52:
        return "BDT " + grp(v), v, v, v, ""
    if r < 0.58:
        return bn(grp(v)) + " টাকা", v, v, v, "Bangla digits and taka"
    if r < 0.62:
        return "%d hajar" % (v // 1000) if v % 1000 == 0 else str(v), v, v, v, ""
    if r < 0.70:
        lo = v
        hi = v + rng.choice([5000, 10000])
        if lo % 1000 == 0 and hi % 1000 == 0 and rng.random() < 0.5:
            return "%d-%dk" % (lo // 1000, hi // 1000), "", lo, hi, "range"
        return "%d-%d" % (lo, hi), "", lo, hi, "range"
    if r < 0.75:
        return rng.choice(["about ", "approx ", "~"]) + str(v), "", v, v, "approximate"
    if r < 0.78:
        s = str(v)
        return "Tk " + s[:-2] + "," + s[-2:], "", "", "", "commas in a wrong place"
    if r < 0.88:
        return rng.choice(["N/A", "-", "", "unknown", "don't know", "varies"]), "", "", "", "missing or no amount"
    return str(v), v, v, v, ""


def messy_age(a):
    r = rng.random()
    if r < 0.70:
        return str(a), a, ""
    if r < 0.78:
        return bn(str(a)), a, "Bangla digits"
    if r < 0.84:
        return " %d " % a, a, ""
    if r < 0.88:
        return "%d years" % a, "", "text after the number (not a plain number)"
    if r < 0.94:
        return rng.choice(["N/A", "-", "", "?"]), "", "missing"
    return str(a), a, ""


def messy_district(d):
    row = dist_by[d]
    r = rng.random()
    if r < 0.30:
        return case(d), "exact"
    if r < 0.45:
        return row["district_legacy"], "old spelling"
    if r < 0.55:
        return d + rng.choice([" District", " Zila", " zilla", "-Zila", " dist."]), "suffix"
    if r < 0.63:
        return row["district_bn"], "Bangla"
    if r < 0.73 and aliases_of_dist.get(d):
        return rng.choice(aliases_of_dist[d]), "variant"
    if r < 0.78 and row["district_map"] != d:
        return row["district_map"], "map spelling"
    if r < 0.86 and len(d) >= 7:
        return typo(d), "typing error"
    if r < 0.92:
        return rng.choice(["", "N/A", "-"]), "missing"
    return d, "exact"


def messy_division(v):
    row = div_by[v]
    r = rng.random()
    if r < 0.45:
        return case(v), "exact"
    if r < 0.65:
        return row["division_legacy"], "old spelling"
    if r < 0.75:
        return v + " Division", "suffix"
    if r < 0.83:
        return row["division_bn"], "Bangla"
    if r < 0.88 and aliases_of_div.get(v):
        return rng.choice(aliases_of_div[v]), "variant"
    if r < 0.94:
        return rng.choice(["", "N/A"]), "missing"
    return v, "exact"


def messy_thana(p):
    """The thana (or upazila) column, written as in real data."""
    t = p["thana"]
    r = rng.random()
    al = aliases_of_upz.get((p["district"], t), [])
    if p["relation"] == "within" and p.get("also") and r < 0.35:
        return rng.choice(p["also"].split(";")), "variant"
    if r < 0.40:
        return case(t), "exact"
    if r < 0.52:
        sfx = [" Thana", " P.S.", " PS"]
        if p["relation"] == "rural":
            sfx += [" Upazila", " upazilla"]
        return t + rng.choice(sfx), "suffix"
    if r < 0.64 and al:
        return rng.choice(al), "variant"
    if r < 0.68 and p["map_name"] and p["map_name"] != t and p["relation"] != "within":
        return p["map_name"], "map spelling"
    if r < 0.76 and len(t) >= 7:
        return typo(t), "typing error"
    if r < 0.88:
        return rng.choice(["", "N/A", "-"]), "missing"
    return t, "exact"


def messy_address(p, dist_written):
    """A free-text address; returns (address, post code used or '')."""
    key = (p["district"], p["area"])
    pcs = pc_of_upz.get(key, [])
    pc = rng.choice(pcs) if pcs else None
    vill = rng.choice(["Char", "Uttar", "Dakshin", "Purba", "Paschim", "Nayapara", "Kazipara",
                       "Mollapara", "Bhuiyanpara", "Shantibagh"]) + " " + \
        rng.choice(["Para", "Gram", "Bari", "Tola", "Kandi"])
    if p["relation"] == "city":
        place = p["thana"]
    elif p["relation"] == "within":
        place = rng.choice([p["thana"]] + [x for x in p.get("also", "").split(";") if x])
    else:
        place = rng.choice([p["thana"]] + aliases_of_upz.get(key, [])[:2])
    d = rng.choice([p["district"], dist_by[p["district"]]["district_legacy"], dist_written or p["district"]])
    r = rng.random()
    if p["relation"] == "city":
        hn = "House %d, Road %d, " % (rng.randrange(1, 120), rng.randrange(1, 30))
        if pc and r < 0.6:
            return hn + place + ", " + d + "-" + pc["post_code"], pc["post_code"]
        return hn + place + ", " + d, ""
    if r < 0.35 and pc:
        po = pc["post_office"].rsplit(" ", 1)[0] if " " in pc["post_office"] else pc["post_office"]
        return "Vill: %s, P.O: %s, %s, %s - %s" % (vill, po, place, d, pc["post_code"]), pc["post_code"]
    if r < 0.55:
        return "%s, %s" % (place, d), ""
    if r < 0.65:
        return "Vill %s, Upazila %s, Dist %s" % (vill, place, d), ""
    if r < 0.72:
        return "%s %s" % (place.lower(), d.lower()), ""
    if r < 0.78:
        return place, ""
    if r < 0.84:
        return d, ""
    if r < 0.90:
        return rng.choice(["", "N/A", "same as above"]), ""
    return "%s, %s" % (place, d), ""


rows, key = [], []
for i in range(1, N + 1):
    p = draw_place()
    division = dist_by[p["district"]]["division"]
    phone = true_phone()
    age = rng.randrange(18, 80)
    m_phone, k_phone, n_phone = messy_phone(phone)
    m_inc, k_inc, lo, hi, n_inc = messy_amount()
    m_age, k_age, n_age = messy_age(age)
    m_dist, h_dist = messy_district(p["district"])
    m_div, h_div = messy_division(division)
    m_th, h_th = messy_thana(p)
    m_addr, k_pc = messy_address(p, m_dist if h_dist not in ("missing", "typing error") else "")
    rid = "R%04d" % i
    rows.append(dict(id=rid, name=rng.choice(FIRST) + " " + rng.choice(LAST),
                     mobile=m_phone, age=m_age, monthly_income=m_inc,
                     division=m_div, district=m_dist, thana=m_th,
                     address=m_addr))
    key.append(dict(id=rid, phone=k_phone, phone_note=n_phone, age=k_age, age_note=n_age,
                    income=k_inc, income_low=lo, income_high=hi, income_note=n_inc,
                    division=division, division_written_as=h_div,
                    district=p["district"], district_written_as=h_dist,
                    thana=p["thana"], thana_type=p["relation"], thana_written_as=h_th,
                    address_postcode=k_pc,
                    thana_name_in_several_districts="yes" if upz_count.get(p["thana"], 0) > 1 else "no"))

# A few exact duplicate rows, as happen when forms are submitted twice.
for j in rng.sample(range(N), 30):
    d = dict(rows[j])
    rows.append(d)
    key.append(dict(key[j]))

os.makedirs(out, exist_ok=True)
for name, data in [("bdscrub_messy_survey.csv", rows), ("bdscrub_answer_key.csv", key)]:
    with open(os.path.join(out, name), "w", encoding="utf-8-sig", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(data[0].keys()))
        w.writeheader()
        w.writerows(data)
print("rows:", len(rows))
