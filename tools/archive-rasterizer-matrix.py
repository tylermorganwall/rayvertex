"""Archive matrix comparisons and all diagnostic records, excluding temporary RDS files.
Usage: python3 tools/archive-rasterizer-matrix.py RUN OUTPUT BEFORE_SHA AFTER_SHA
"""
import csv
import json
import pathlib
import shutil
import sys

run, out = map(pathlib.Path, sys.argv[1:3])
before_sha, after_sha = sys.argv[3:5]
out.mkdir(parents=True, exist_ok=True)
for path in run.rglob('*'):
    if path.is_file() and path.suffix in ('.csv', '.json', '.log', '.txt'):
        target = out/path.relative_to(run)
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(path,target)
def read(path):
    return list(csv.DictReader(path.open()))
def key(row):
    return tuple(row[k] for k in ('case','width','height','fsaa','cores'))
rows={variant:{key(row):row for row in read(run/(variant+'-summary')/'summary.csv')}
      for variant in ('before','after')}
settings=json.loads((run/'settings.json').read_text())
lines=[f'# {settings["suite"]} sweep: `{before_sha}` → `{after_sha}`','',
       'Three warm public calls and three isolated native samples per setting, with GC outside their timers. One separate diagnostic sample per setting supplies phase timings/counters; its p95 is not a distribution. Variant order alternates between settings. Quality is identical within each pair. Cold time, R allocations and diagnostic-process RSS remain in the CSV summaries. These short samples do not establish stable tail latency.','',
       '| Scene | Output / FSAA / workers | Public median / p95 ms | Native median / p95 ms | Fresh RSS MiB |',
       '|---|---|---:|---:|---:|']
for k in sorted(rows['before'], key=lambda x: (x[0], *map(int, x[1:]))):
    def pair(field,scale=1):
        return ' → '.join(f'{float(rows[v][k][field])/scale:.1f}' if rows[v][k][field] else '—' for v in rows)
    lines.append(f'| {k[0]} | {k[1]}×{k[2]} / {k[3]} / {k[4]} | {pair("warm_median_ms")} / {pair("warm_p95_ms")} | {pair("native_median_ms")} / {pair("native_p95_ms")} | {pair("single_render_peak_rss_bytes",2**20)} |')
diffs=[row for path in sorted(run.glob('*-differences.csv')) for row in read(path)]
lines+=['',f'Exact image/buffer comparisons: {sum(row["exact"]=="TRUE" for row in diffs)} / {len(diffs)}; maximum absolute difference {max(float(row["max_absolute"]) for row in diffs):g}. Differences include metadata, nonfinite patterns and changed-sample counts.','',
        'Fresh RSS was measured separately for selected high-sample-count and deep-layer cases only. A dash means unmeasured, not zero. Whole diagnostic-process RSS includes retained outputs and profiling; it is not a fresh-render memory result. No plotting or file encoding is included.']
(out/'results.md').write_text('\n'.join(lines)+'\n')
print(out/'results.md')
