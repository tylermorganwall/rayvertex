"""Alternating-order regression repeats, without competing renderer processes.
Usage: python3 tools/bench-rasterizer-repeats.py REFERENCE_LIB FINAL_LIB OUTPUT
Each variant gets three processes per case, five warm/native samples per process.
"""
import csv
import json
import os
import pathlib
import subprocess
import sys

if len(sys.argv) != 4:
    raise SystemExit(__doc__)
libs = dict(zip(['reference', 'final'], sys.argv[1:3]))
root = pathlib.Path(sys.argv[3])
root.mkdir(parents=True, exist_ok=True)
cases = os.environ.get('RAYVERTEX_REPEAT_CASES', 'small,ssao,shadow,toon').split(',')
tangent_after = os.environ.get('RAYVERTEX_TANGENT_AFTER') == '1'
(root/'settings.json').write_text(json.dumps(dict(libraries=libs, cases=cases,
    rounds=3, samples_per_regime=5, width=800, height=800, fsaa=1, cores=1,
    tangent_after=tangent_after), indent=2)+'\n')
for round in range(1, 4):
    for i, case in enumerate(cases):
        variants = ['reference', 'final'] if (round + i) % 2 else ['final', 'reference']
        for variant in variants:
            key = f'{case}-{variant}-{round}'
            out = root / (key + '.csv')
            if out.exists():
                rows = list(csv.DictReader(out.open()))
                if all(sum(r['regime'] == mode for r in rows) == 5 for mode in ['warm', 'native']):
                    continue
            with (root / (key + '.log')).open('w') as log:
                env = dict(os.environ)
                if tangent_after:
                    env.pop('RAYVERTEX_TANGENT_ALGEBRA', None)
                    if variant == 'final':
                        env['RAYVERTEX_TANGENT_ALGEBRA'] = '1'
                subprocess.run(['Rscript', 'tools/bench-rasterizer-repeat.R', libs[variant],
                                str(out), case, '1', '5'], stdout=log,
                               stderr=subprocess.STDOUT, check=True, env=env)
            print(key, flush=True)
