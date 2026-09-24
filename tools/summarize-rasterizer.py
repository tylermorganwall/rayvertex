"""Aggregate retained samples without dependencies.
Usage: python3 tools/summarize-rasterizer.py RUN_DIR OUTPUT_DIR
"""
import csv
import json
import pathlib
import statistics
import sys

root, out = map(pathlib.Path, sys.argv[1:])
out.mkdir(parents=True, exist_ok=True)

def percentile(values, p):
    values = sorted(values)
    idx = (len(values) - 1) * p
    lo = int(idx)
    hi = min(lo + 1, len(values) - 1)
    return values[lo] + (values[hi] - values[lo]) * (idx - lo)

rows, phases = [], []
for path in sorted(root.glob("*-times.csv")):
    key = path.name[:-len("-times.csv")]
    samples = list(csv.DictReader(path.open()))
    warm = [float(r["elapsed_ms"]) for r in samples if r["regime"] == "warm"]
    native = [float(r["native_ms"]) for r in csv.DictReader((root / (key + "-native.csv")).open())]
    rss = json.loads((root / (key + "-rss.json")).read_text())
    alloc = next(csv.DictReader((root / (key + "-allocations.csv")).open()))
    row = {k: samples[0][k] for k in ("case", "width", "height", "fsaa", "cores")}
    row.update(samples=len(warm), cold_ms=samples[0]["elapsed_ms"],
               warm_median_ms=statistics.median(warm), warm_p95_ms=percentile(warm, .95),
               fps=1000/statistics.median(warm), native_median_ms=statistics.median(native),
               native_p95_ms=percentile(native, .95), peak_rss_bytes=rss["peak_process_rss_bytes"],
               R_allocation_bytes=alloc["R_allocation_bytes"])
    overhead = root / (key + "-profile-overhead.csv")
    profiled = [float(r["profiled_ms"]) for r in csv.DictReader(overhead.open())] if overhead.exists() else []
    row["profiled_native_median_ms"] = statistics.median(profiled) if profiled else ""
    row["profiled_native_samples"] = len(profiled)
    render_rss = root / (key + "-render-only-rss.json")
    row["single_render_peak_rss_bytes"] = json.loads(render_rss.read_text())["peak_process_rss_bytes"] if render_rss.exists() else ""
    rows.append(row)
    profile = root / (key + "-native-phases.csv")
    if profile.exists():
        grouped = {}
        for phase, value in csv.reader(profile.open()):
            grouped.setdefault(phase, []).append(float(value))
        for phase, values in grouped.items():
            phases.append(dict(key=key, phase=phase, median=statistics.median(values),
                               p95=percentile(values, .95), samples=len(values)))
for name, data in [("summary", rows), ("phases", phases)]:
    if data:
        with (out / (name + ".csv")).open("w") as f:
            writer = csv.DictWriter(f, fieldnames=data[0].keys(), lineterminator="\n")
            writer.writeheader()
            writer.writerows(data)
