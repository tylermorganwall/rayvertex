"""Measure a structural phase against its installed predecessor, sequentially.
Usage: python3 tools/bench-rasterizer-structural.py BEFORE_LIB AFTER_LIB OUTPUT
Each case alternates variant order; ordinary/native/diagnostic runs stay separate.
"""
import json
import os
import pathlib
import subprocess
import sys

if len(sys.argv) != 4:
    raise SystemExit(__doc__)
libs = dict(zip(['before', 'after'], sys.argv[1:3]))
root = pathlib.Path(sys.argv[3])
root.mkdir(parents=True, exist_ok=True)
cases = os.environ.get('RAYVERTEX_STRUCTURAL_CASES', 'small,grid1m,alpha4,alpha16,alpha64,shadow').split(',')
(root / 'settings.json').write_text(json.dumps({
    'libraries': libs, 'cases': cases, 'width': 800, 'height': 800,
    'fsaa': 1, 'cores': 1, 'warm_samples': 3, 'sustained_frames': 5,
    'visibility': os.environ.get('RAYVERTEX_VISIBILITY'),
    'indexed_transforms': os.environ.get('RAYVERTEX_INDEXED_TRANSFORMS'),
    'prepared_after': os.environ.get('RAYVERTEX_PREPARED_AFTER'),
}, indent=2) + '\n')
for i, case in enumerate(cases):
    for variant in (['before', 'after'] if i % 2 == 0 else ['after', 'before']):
        child_env = os.environ.copy()
        if os.environ.get('RAYVERTEX_PREPARED_AFTER') == '1':
            child_env['RAYVERTEX_PREPARED'] = '1' if variant == 'after' else '0'
        out = root / variant
        out.mkdir(exist_ok=True)
        for memory in [False, True]:
            key = f'{case}-800-800-1-1' + ('-render-only' if memory else '')
            if (out / (key + '-rss.json')).exists():
                continue
            subprocess.run([sys.executable, 'tools/bench-rasterizer.py'] +
                           (['--memory'] if memory else []) +
                           [libs[variant], str(out), case, '800', '800', '1', '1', '3'],
                           check=True, env=child_env)
        prefix = out / (case + '-sustained')
        if not pathlib.Path(str(prefix) + '-loop.csv').exists():
            with pathlib.Path(str(prefix) + '.log').open('w') as log:
                subprocess.run(['Rscript', 'tools/bench-rasterizer-sustained.R',
                                libs[variant], str(prefix), case, '5'],
                               stdout=log, stderr=subprocess.STDOUT, check=True, env=child_env)
subprocess.run(['Rscript', 'tools/compare-rasterizer.R', str(root/'before'),
                str(root/'after'), str(root/'image-differences.csv')], check=True)
for variant in libs:
    subprocess.run([sys.executable, 'tools/summarize-rasterizer.py',
                    str(root/variant), str(root/(variant+'-summary'))], check=True)
