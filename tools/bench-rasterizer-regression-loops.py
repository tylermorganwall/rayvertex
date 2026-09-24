"""Three alternating process rounds for public regressions, including cleanup.
Usage: python3 tools/bench-rasterizer-regression-loops.py BEFORE_LIB AFTER_LIB OUT
The full-buffer control is diagnostic, not a recommended rendering preset.
"""
import csv
import os
import pathlib
import subprocess
import sys
before, after, directory=sys.argv[1:]
root=pathlib.Path(directory)
root.mkdir(parents=True,exist_ok=True)
variants=[('before',before),('after',after),('full_buffers',after)]
for round_number in range(1,4):
    for case in ('small','toon','environment'):
        order=variants if round_number%2 else list(reversed(variants))
        for variant,lib in order:
            prefix=root/f'{case}-{variant}-{round_number}'
            if pathlib.Path(str(prefix)+'-loop.csv').exists(): continue
            env=dict(os.environ)
            env.pop('RAYVERTEX_REFERENCE_BUFFERS',None)
            if variant=='full_buffers': env['RAYVERTEX_REFERENCE_BUFFERS']='1'
            with pathlib.Path(str(prefix)+'.log').open('w') as log:
                subprocess.run(['Rscript','tools/bench-rasterizer-sustained.R',lib,str(prefix),case,'10'],
                               env=env,stdout=log,stderr=subprocess.STDOUT,check=True)
            print(prefix.name,flush=True)
