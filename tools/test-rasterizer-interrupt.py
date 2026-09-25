"""Interrupt an owned R child after native entry, then verify recovery."""
import os
from pathlib import Path
import signal
import subprocess
import sys
import time

library, output = sys.argv[1:]
output = Path(output)
output.mkdir(parents=True, exist_ok=True)
for mode in ("ordinary", "prepared"):
    marker = output / f"interrupt-{mode}.ready"
    result = output / f"interrupt-{mode}.txt"
    marker.unlink(missing_ok=True)
    result.unlink(missing_ok=True)
    with (output / f"interrupt-{mode}.log").open("w") as log:
        process = subprocess.Popen(
            ["Rscript", "tools/test-rasterizer-interrupt.R", library,
             str(marker), str(result), mode], stdout=log, stderr=subprocess.STDOUT)
        try:
            deadline = time.monotonic() + 90
            while not marker.exists():
                if process.poll() is not None or time.monotonic() > deadline:
                    raise RuntimeError(f"{mode}: native-entry marker missing")
                time.sleep(0.02)
            # Let native geometry/pixel work begin after the entry trace returns.
            time.sleep(0.25)
            os.kill(process.pid, signal.SIGINT)
            code = process.wait(timeout=90)
            if code != 0 or not result.exists():
                raise RuntimeError(f"{mode}: interrupt recovery failed ({code}); see {log.name}")
            print(result.read_text().strip())
        finally:
            if process.poll() is None:
                process.kill()
                process.wait()
