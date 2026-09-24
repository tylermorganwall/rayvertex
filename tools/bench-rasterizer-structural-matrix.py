"""Sequential structural validation sweeps, comparing and releasing large RDS outputs.
Usage: python3 tools/bench-rasterizer-structural-matrix.py BEFORE_LIB AFTER_LIB OUT SUITE
Suites: workers, quality, visibility, bins. Timings keep quality fixed within each pair.
"""
import csv
import json
import os
import pathlib
import subprocess
import sys

before, after, output, suite = sys.argv[1:]
root = pathlib.Path(output)
root.mkdir(parents=True, exist_ok=True)
if suite == 'workers':
    jobs = [(case, 800, 800, 1, cores) for cores in (1, 2, 4, 10)
            for case in ('alpha16', 'overdraw', 'shadow', 'ssao')]
elif suite == 'quality':
    jobs = [(case, width, height, fsaa, 4)
            for width, height, fsaa in ((800, 800, 2), (1920, 1080, 1), (1920, 1080, 2))
            for case in ('small', 'grid100k', 'alpha4')]
    jobs += [('ssao', 1920, 1080, 2, 4), ('alpha129', 159, 241, 1, 4)]
elif suite == 'visibility':
    jobs = [('overdraw', 800, 800, 1, cores) for cores in (1, 2, 4, 10)]
    jobs += [('overdraw', 1920, 1080, 2, 4), ('small', 800, 800, 1, 4)]
elif suite == 'bins':
    jobs = [('grid1m', 800, 800, 1, cores) for cores in (1, 2, 4, 10)]
    jobs += [(case, 800, 800, 1, 4) for case in ('small', 'grid100k', 'grid500k', 'overdraw')]
    jobs += [('grid100k', 1920, 1080, 2, 4)]
else:
    raise SystemExit('Unknown suite')
(root / 'settings.json').write_text(json.dumps(dict(before=before, after=after,
    suite=suite, jobs=jobs, warm_samples=3, native_samples=3, diagnostic_samples=1), indent=2)+'\n')
for number, job in enumerate(jobs):
    key = '-'.join(map(str, job))
    diff = root / (key+'-differences.csv')
    if diff.exists():
        continue
    for variant, lib in ([('before', before), ('after', after)] if number%2==0 else
                         [('after', after), ('before', before)]):
        folder = root / variant
        env = dict(os.environ, RAYVERTEX_DIAGNOSTIC_REPS='1')
        if suite == 'visibility':
            env.pop('RAYVERTEX_VISIBILITY', None)
            if variant == 'after':
                env['RAYVERTEX_VISIBILITY'] = '1'
        if suite == 'bins':
            env.pop('RAYVERTEX_PARALLEL_BINS', None)
            if variant == 'after':
                env['RAYVERTEX_PARALLEL_BINS'] = '1'
        status = folder / (key+'-rss.json')
        if not status.exists() or json.loads(status.read_text())['exit_code'] != 0:
            subprocess.run([sys.executable, 'tools/bench-rasterizer.py', lib, str(folder),
                            *map(str, job), '3'], check=True, env=env)
        # Fresh render RSS for the highest sample count and deep portrait stress.
        if (job[0] in ('grid100k', 'alpha129') and (job[1:4]==(1920,1080,2) or job[0]=='alpha129')) or (suite=='bins' and job==('grid1m',800,800,1,4)):
            subprocess.run([sys.executable, 'tools/bench-rasterizer.py', '--memory', lib,
                            str(folder), *map(str, job), '3'], check=True, env=env)
    subprocess.run(['Rscript', 'tools/compare-rasterizer.R', str(root/'before'),
                    str(root/'after'), str(diff)], check=True)
    rows = list(csv.DictReader(diff.open()))
    if len(rows)!=17 or any(r['exact']!='TRUE' for r in rows):
        raise RuntimeError('Retaining mismatching image outputs: '+key)
    # These are this driver's disposable outputs; golden references stay retained.
    for variant in ('before', 'after'):
        for suffix in ('image', 'buffers'):
            (root/variant/(key+'-'+suffix+'.rds')).unlink()
for variant in ('before', 'after'):
    subprocess.run([sys.executable, 'tools/summarize-rasterizer.py', str(root/variant),
                    str(root/(variant+'-summary'))], check=True)
