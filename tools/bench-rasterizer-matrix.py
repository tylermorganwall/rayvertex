"""Sequential benchmark suites; never benchmark competing R processes.
Usage: python3 tools/bench-rasterizer-matrix.py LIB OUTPUT_ROOT SUITE
Suites: representative, quality, workers, batches, memory. Restart skips completed cases.
Set RAYVERTEX_DIAGNOSTIC_REPS to reduce separate instrumented diagnostic runs.
"""
import json
import os
import pathlib
import subprocess
import sys

lib, root, suite = sys.argv[1:]
jobs = []
if suite in ("representative", "memory"):
    for case in ["small", "grid100k", "grid500k", "grid1m", "occluded",
                 "alpha4", "alpha16", "alpha64", "ssao", "shadow", "toon", "shared_textures"]:
        jobs.append((case, 800, 800, 1, 1, 5 if case != "alpha64" else 3, {}))
elif suite == "quality":
    for width, height in [(800, 800), (1920, 1080)]:
        for fsaa in [1, 2]:
            for case in ["small", "grid100k", "ssao"]:
                jobs.append((case, width, height, fsaa, 4, 5, {}))
elif suite == "workers":
    for cores in [1, 2, 4, 10]:
        for case in ["small", "grid100k", "occluded", "ssao", "shadow", "toon"]:
            jobs.append((case, 800, 800, 1, cores, 5, {}))
elif suite == "batches":
    for batch in [16, 64, 256]:
        for case in ["small", "grid100k", "occluded"]:
            jobs.append((case, 800, 800, 1, 4, 10, {"RAYVERTEX_BATCH_BLOCKS": str(batch)}))
else:
    raise SystemExit("Unknown suite: " + suite)

for case, width, height, fsaa, cores, reps, overrides in jobs:
    folder = pathlib.Path(root)
    if overrides:
        folder /= "batch-" + overrides["RAYVERTEX_BATCH_BLOCKS"]
    key = "-".join(map(str, [case, width, height, fsaa, cores]))
    suffix = "-render-only-rss.json" if suite == "memory" else "-rss.json"
    status = folder / (key + suffix)
    if status.exists() and json.loads(status.read_text()).get("exit_code") == 0:
        continue
    env = dict(os.environ, **overrides)
    env.setdefault("RAYVERTEX_DIAGNOSTIC_REPS", "3")
    command = [sys.executable, "tools/bench-rasterizer.py"]
    if suite == "memory":
        command.append("--memory")
    command += [lib, str(folder), case,
               *map(str, [width, height, fsaa, cores, reps])]
    result = subprocess.run(command, env=env)
    if result.returncode:
        raise SystemExit(result.returncode)
