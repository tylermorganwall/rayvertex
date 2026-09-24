"""Run one R benchmark in its own process and record OS peak RSS (not R heap).

Usage: python3 tools/bench-rasterizer.py LIB OUT CASE W H FSAA CORES REPS
Uses getrusage instead of platform-specific /usr/bin/time output parsing.
"""
import json
import pathlib
import resource
import subprocess
import sys

args = sys.argv[1:]
if len(args) != 8:
    raise SystemExit(__doc__)
out = pathlib.Path(args[1])
out.mkdir(parents=True, exist_ok=True)
key = "-".join(args[2:7])
with (out / (key + "-process.log")).open("w") as log:
    result = subprocess.run(["Rscript", "tools/bench-rasterizer.R", *args],
                            stdout=log, stderr=subprocess.STDOUT)
usage = resource.getrusage(resource.RUSAGE_CHILDREN)
rss_bytes = usage.ru_maxrss * (1 if sys.platform == "darwin" else 1024)
(out / (key + "-rss.json")).write_text(json.dumps({
    "peak_process_rss_bytes": rss_bytes,
    "includes": "startup, fixtures, warm calls, retained output, allocation and diagnostic runs",
    "exit_code": result.returncode,
}, indent=2) + "\n")
print(key, "exit", result.returncode, "peak RSS MiB", round(rss_bytes / 2**20, 1))
raise SystemExit(result.returncode)
