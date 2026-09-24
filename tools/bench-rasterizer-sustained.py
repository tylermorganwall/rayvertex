"""Interleaved ordinary API loops, including automatic GC and final cleanup.
Usage: python3 tools/bench-rasterizer-sustained.py REFERENCE_LIB FINAL_LIB OUTPUT
"""
import pathlib
import subprocess
import sys

if len(sys.argv) != 4:
    raise SystemExit(__doc__)
libs = dict(zip(['reference', 'final'], sys.argv[1:3]))
root = pathlib.Path(sys.argv[3])
root.mkdir(parents=True, exist_ok=True)
cases = ['small','grid100k','grid500k','grid1m','occluded','alpha4','alpha16',
         'alpha64','ssao','shadow','toon','shared_textures']
for i, case in enumerate(cases):
    for variant in (['reference','final'] if i % 2 == 0 else ['final','reference']):
        key = case + '-' + variant
        prefix = root / key
        if (root / (key + '-loop.csv')).exists():
            continue
        with (root / (key + '.log')).open('w') as log:
            subprocess.run(['Rscript','tools/bench-rasterizer-sustained.R',libs[variant],
                            str(prefix),case,'5' if case == 'alpha64' else '10'],
                           stdout=log,stderr=subprocess.STDOUT,check=True)
        print(key, flush=True)
