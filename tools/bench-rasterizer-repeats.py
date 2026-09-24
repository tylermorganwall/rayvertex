"""Alternating-order regression repeats, without competing renderer processes.
Usage: python3 tools/bench-rasterizer-repeats.py REFERENCE_LIB FINAL_LIB OUTPUT
Each variant gets three processes per case, five warm/native samples per process.
"""
import csv
import pathlib
import subprocess
import sys

if len(sys.argv) != 4:
    raise SystemExit(__doc__)
libs = dict(zip(['reference', 'final'], sys.argv[1:3]))
root = pathlib.Path(sys.argv[3])
root.mkdir(parents=True, exist_ok=True)
for round in range(1, 4):
    for i, case in enumerate(['small', 'ssao', 'shadow', 'toon']):
        variants = ['reference', 'final'] if (round + i) % 2 else ['final', 'reference']
        for variant in variants:
            key = f'{case}-{variant}-{round}'
            out = root / (key + '.csv')
            if out.exists():
                rows = list(csv.DictReader(out.open()))
                if all(sum(r['regime'] == mode for r in rows) == 5 for mode in ['warm', 'native']):
                    continue
            with (root / (key + '.log')).open('w') as log:
                subprocess.run(['Rscript', 'tools/bench-rasterizer-repeat.R', libs[variant],
                                str(out), case, '1', '5'], stdout=log,
                               stderr=subprocess.STDOUT, check=True)
            print(key, flush=True)
