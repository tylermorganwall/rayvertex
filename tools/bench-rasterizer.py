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
memory_only = bool(args and args[0] == "--memory")
if memory_only:
    args = args[1:]
if len(args) != 8:
    raise SystemExit(__doc__)
out = pathlib.Path(args[1])
out.mkdir(parents=True, exist_ok=True)
key = "-".join(args[2:7]) + ("-render-only" if memory_only else "")
with (out / (key + "-process.log")).open("w") as log:
    # source() parses the entire file before executing, so edits to developer
    # scripts during a long run cannot change a partially evaluated program.
    script = "tools/bench-rasterizer-memory.R" if memory_only else "tools/bench-rasterizer.R"
    result = subprocess.run(["Rscript", "-e", 'source("' + script + '")', *args],
                            stdout=log, stderr=subprocess.STDOUT)
usage = resource.getrusage(resource.RUSAGE_CHILDREN)
rss_bytes = usage.ru_maxrss * (1 if sys.platform == "darwin" else 1024)
(out / (key + "-rss.json")).write_text(json.dumps({
    "peak_process_rss_bytes": rss_bytes,
    "includes": "startup, fixture creation, one render" if memory_only else
        "startup, fixtures, warm calls, retained output, allocation and diagnostic runs",
    "exit_code": result.returncode,
}, indent=2) + "\n")
print(key, "exit", result.returncode, "peak RSS MiB", round(rss_bytes / 2**20, 1))
raise SystemExit(result.returncode)
