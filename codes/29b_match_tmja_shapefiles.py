#!/usr/bin/env python3
"""Match baseline camera sites to official TMJA polyline geometries.

Requires pyshp 2.3.1. The source shapefiles remain under Open Licence 2.0.
No spatial interpolation is performed: a site must share the normalized road
number and lie within the reported distance of an observed traffic section.
"""

import csv
import glob
import math
import os
import re
import sys

try:
    import shapefile
except ImportError as exc:
    raise SystemExit("Install pyshp==2.3.1 to run this script") from exc


ROOT = os.path.abspath(".")
OUT = os.path.join(ROOT, "results", "current")
SHAPE_ROOT = os.path.join(ROOT, "data", "traffic", "shapefiles")


def as_bool(value):
    return str(value).strip().upper() in {"TRUE", "T", "1"}


def normalize_route(value):
    cleaned = re.sub(r"[^A-Z0-9]", "", (value or "").upper())
    matched = re.match(r"^(?:RN|N|A)0*([0-9]{1,4})", cleaned)
    if not matched:
        return None
    prefix = "A" if cleaned.startswith("A") else "N"
    return f"{prefix}{int(matched.group(1)):04d}"


def wgs84_to_lambert93(latitude, longitude):
    eccentricity = 0.081819191042816
    exponent = 0.725607765053267
    constant = 11754255.426096
    phi = math.radians(latitude)
    radius = constant * math.exp(
        -exponent
        * (math.atanh(math.sin(phi)) - eccentricity * math.atanh(eccentricity * math.sin(phi)))
    )
    angle = exponent * math.radians(longitude - 3)
    return 700000 + radius * math.sin(angle), 12655612.049876 - radius * math.cos(angle)


def segment_distance(px, py, x1, y1, x2, y2):
    dx, dy = x2 - x1, y2 - y1
    denominator = dx * dx + dy * dy
    fraction = 0.0 if denominator == 0 else ((px - x1) * dx + (py - y1) * dy) / denominator
    fraction = min(1.0, max(0.0, fraction))
    return math.hypot(px - x1 - fraction * dx, py - y1 - fraction * dy)


def bbox_distance(px, py, bbox):
    xmin, ymin, xmax, ymax = bbox
    dx = max(xmin - px, 0, px - xmax)
    dy = max(ymin - py, 0, py - ymax)
    return math.hypot(dx, dy)


def polyline_distance(px, py, shape, current_best=float("inf")):
    points = shape.points
    if len(points) < 2:
        return current_best
    bbox = (
        min(point[0] for point in points), min(point[1] for point in points),
        max(point[0] for point in points), max(point[1] for point in points),
    )
    if bbox_distance(px, py, bbox) >= current_best:
        return current_best
    starts = list(shape.parts) + [len(points)]
    best = current_best
    for start, end in zip(starts[:-1], starts[1:]):
        for first, second in zip(points[start : end - 1], points[start + 1 : end]):
            distance = segment_distance(px, py, first[0], first[1], second[0], second[1])
            if distance < best:
                best = distance
    return best


def read_csv(path):
    with open(path, newline="", encoding="utf-8-sig") as handle:
        return list(csv.DictReader(handle))


membership = read_csv(os.path.join(OUT, "baseline_radar_sample_membership.csv"))
roads = {
    row["cle_radar"]: row
    for row in read_csv(os.path.join(OUT, "radar_ign_road_matches.csv"))
    if row["match_status"] == "matched"
}

sites = []
for row in membership:
    if not (as_bool(row["baseline_treated"]) or as_bool(row["baseline_control"])):
        continue
    road = roads.get(row["cle_radar"])
    normalized = normalize_route(road["road_number"] if road else None)
    if not normalized:
        continue
    x, y = wgs84_to_lambert93(float(row["latitude"]), float(row["longitude"]))
    sites.append(
        {
            "cle_radar": row["cle_radar"],
            "installation_year": int(row["installation_year"]),
            "baseline_treated": as_bool(row["baseline_treated"]),
            "baseline_control": as_bool(row["baseline_control"]),
            "road_number": road["road_number"],
            "road_match_method": road["match_method"],
            "normalized_route": normalized,
            "x": x,
            "y": y,
        }
    )

matches = []
for year in range(2011, 2020):
    candidates = glob.glob(os.path.join(SHAPE_ROOT, f"tmja_{year}_shp.zip"))
    if len(candidates) != 1:
        raise SystemExit(f"Expected one shapefile archive for {year}, found {len(candidates)}")
    reader = shapefile.Reader(candidates[0])
    field_names = [field[0] for field in reader.fields[1:]]
    route_field = field_names.index("route")
    tmja_name = "TMJA" if "TMJA" in field_names else "tmja"
    tmja_field = field_names.index(tmja_name)
    by_route = {}
    for index, shape_record in enumerate(reader.iterShapeRecords()):
        route = normalize_route(shape_record.record[route_field])
        tmja = shape_record.record[tmja_field]
        if route and tmja not in (None, "") and float(tmja) > 0:
            by_route.setdefault(route, []).append((index, float(tmja), shape_record.shape))
    for site in sites:
        route_sections = by_route.get(site["normalized_route"], [])
        best_distance = float("inf")
        best_row = None
        for shape_row, tmja, shape in route_sections:
            distance = polyline_distance(site["x"], site["y"], shape, best_distance)
            if distance < best_distance:
                best_distance = distance
                best_row = (shape_row, tmja)
        if best_row is None:
            continue
        matches.append(
            {
                "cle_radar": site["cle_radar"],
                "traffic_year": year,
                "normalized_route": site["normalized_route"],
                "tmja": best_row[1],
                "tmja_distance_m": best_distance,
                "traffic_section_row": best_row[0] + 1,
                "installation_year": site["installation_year"],
                "baseline_treated": site["baseline_treated"],
                "baseline_control": site["baseline_control"],
                "road_number": site["road_number"],
                "road_match_method": site["road_match_method"],
                "pre_installation_year": year == site["installation_year"] - 1,
            }
        )

match_path = os.path.join(OUT, "baseline_tmja_site_year_matches_geometry.csv")
with open(match_path, "w", newline="", encoding="utf-8") as handle:
    writer = csv.DictWriter(handle, fieldnames=list(matches[0]))
    writer.writeheader()
    writer.writerows(matches)

site_counts = {
    "treated": len({site["cle_radar"] for site in sites if site["baseline_treated"]}),
    "control": len({site["cle_radar"] for site in sites if site["baseline_control"]}),
}
summary = []
for threshold in (25, 50, 100, 300):
    eligible = [
        row for row in matches
        if row["pre_installation_year"] and row["tmja_distance_m"] <= threshold
    ]
    summary.append(
        {
            "maximum_distance_m": threshold,
            "treated_national_sites": site_counts["treated"],
            "treated_with_pre_tmja": len({row["cle_radar"] for row in eligible if row["baseline_treated"]}),
            "control_national_sites": site_counts["control"],
            "controls_with_own_pre_tmja": len({row["cle_radar"] for row in eligible if row["baseline_control"]}),
            "site_year_matches": sum(row["tmja_distance_m"] <= threshold for row in matches),
        }
    )

summary_path = os.path.join(OUT, "baseline_tmja_geometry_coverage_summary.csv")
with open(summary_path, "w", newline="", encoding="utf-8") as handle:
    writer = csv.DictWriter(handle, fieldnames=list(summary[0]))
    writer.writeheader()
    writer.writerows(summary)

for row in summary:
    print(row)
