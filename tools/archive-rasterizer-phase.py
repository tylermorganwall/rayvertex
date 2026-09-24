"""Archive measured structural-phase evidence without multi-GB RDS intermediates.
Usage: python3 tools/archive-rasterizer-phase.py RUN OUTPUT BEFORE_SHA AFTER_SHA
"""
import csv
import json
import pathlib
import shutil
import sys

run, out = map(pathlib.Path, sys.argv[1:3])
before_sha, after_sha = sys.argv[3:5]
out.mkdir(parents=True, exist_ok=True)
for f in run.rglob('*'):
    if f.is_file() and f.suffix in ['.csv', '.json', '.log', '.txt']:
        target = out / f.relative_to(run)
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(f, target)
def read(file):
    return list(csv.DictReader(file.open()))
rows = {v:{r['case']:r for r in read(run/(v+'-summary')/'summary.csv')} for v in ['before','after']}
prepared_all = json.loads((run/'settings.json').read_text()).get('prepared_all') == '1'
def pair(c, key, scale=1):
    return ' → '.join(f'{float(rows[v][c][key])/scale:.1f}' for v in rows)
lines = [f'# Measured phase: {before_sha} → {after_sha}', '',
         '800×800; FSAA 1; one worker; unchanged fixture/lighting/quality. Three warm samples; three separate uninstrumented native samples; three diagnostic profiles. Cold is the first render in a fresh R process including preparation/decode, with OS file caches uncontrolled. Fresh-render RSS is a separate one-render process. Sustained mean covers five ordinary calls plus final release/GC; warm/native samples force GC outside each timer.', '',
         '| Scene | Cold ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |',
         '|---|---:|---:|---:|---:|---:|']
for c in rows['before']:
    loops=[]
    for v in rows:
        f=run/v/(c+'-sustained-loop.csv')
        loops.append(f"{float(read(f)[0]['mean_with_cleanup_ms']):.1f}")
    lines.append(f"| {c} | {pair(c,'cold_ms')} | {pair(c,'warm_median_ms')} / {pair(c,'warm_p95_ms')} | {pair(c,'native_median_ms')} / {pair(c,'native_p95_ms')} | {' → '.join(loops)} | {pair(c,'single_render_peak_rss_bytes',2**20)} |")
diffs=read(run/'image-differences.csv')
lines += ['', f"Image/buffer comparisons: {len(diffs)}; exact: {sum(r['exact']=='TRUE' for r in diffs)}. Maximum absolute difference: {max(float(r['max_absolute']) for r in diffs):.12g}. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.", '',
          'Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.']
if prepared_all:
    lines[2] = ('800×800; FSAA 1; one worker; unchanged quality. Both variants use explicit prepared handles. '
                'Three warm public samples, three isolated native samples and three diagnostics; '
                'five-frame sustained loops include final output cleanup. The first-render column excludes preparation; '
                'the table below includes preparation plus the first render. Fresh RSS includes fixture construction, preparation and one render. OS file caches are uncontrolled.')
    lines[4] = lines[4].replace('Cold ms', 'First prepared render ms')
    lines += ['', '| Scene | Preparation ms | Preparation + first render ms |', '|---|---:|---:|']
    for c in rows['before']:
        preparation = {v:read(run/v/(c+'-800-800-1-1-preparation.csv'))[0] for v in rows}
        pairs = [' → '.join(f"{float(preparation[v][field]):.1f}" for v in rows)
                 for field in ('preparation_ms', 'prepare_plus_first_ms')]
        lines.append(f"| {c} | {pairs[0]} | {pairs[1]} |")
(out/'results.md').write_text('\n'.join(lines)+'\n')
print(out/'results.md')
