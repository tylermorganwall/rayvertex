# Measured phase: e512c02 → 7671c4f

800×800; FSAA 1; one worker; unchanged fixture/lighting/quality. Three warm samples; three separate uninstrumented native samples; three diagnostic profiles. Cold is the first render in a fresh R process including preparation/decode, with OS file caches uncontrolled. Fresh-render RSS is a separate one-render process. Sustained mean covers five ordinary calls plus final release/GC; warm/native samples force GC outside each timer.

| Scene | Cold ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| alpha16 | 884.0 → 568.0 | 777.0 → 511.0 / 795.0 → 511.0 | 631.0 → 326.0 / 631.9 → 330.5 | 794.0 → 507.8 | 747.2 → 375.5 |
| alpha4 | 465.0 → 327.0 | 325.0 → 271.0 / 346.6 → 274.6 | 178.0 → 86.0 / 179.8 → 86.9 | 343.6 → 273.0 | 461.7 → 378.9 |
| alpha64 | 2343.0 → 1392.0 | 2254.0 → 1332.0 / 2266.6 → 1337.4 | 2128.0 → 1155.0 / 2139.7 → 1158.6 | 2255.0 → 1321.0 | 2038.5 → 1030.4 |
| environment | 356.0 → 304.0 | 226.0 → 243.0 / 241.3 → 247.5 | 72.0 → 51.0 / 72.9 → 55.5 | 240.6 → 246.8 | 455.3 → 373.7 |
| grid1m | 620.0 → 554.0 | 513.0 → 491.0 / 523.8 → 500.0 | 348.0 → 298.0 / 349.8 → 309.7 | 533.8 → 510.4 | 1267.2 → 1083.1 |
| shadow | 344.0 → 293.0 | 235.0 → 230.0 / 239.5 → 247.1 | 68.0 → 45.0 / 68.0 → 45.9 | 235.2 → 216.0 | 431.1 → 368.4 |
| small | 293.0 → 253.0 | 181.0 → 192.0 / 190.0 → 196.5 | 28.0 → 8.0 / 28.9 → 8.0 | 186.4 → 180.2 | 458.3 → 346.6 |
| ssao | 831.0 → 536.0 | 707.0 → 409.0 / 718.7 → 409.9 | 539.0 → 268.0 / 541.7 → 282.4 | 441.4 → 431.2 | 424.0 → 441.9 |
| toon | 429.0 → 407.0 | 318.0 → 327.0 / 318.0 → 327.9 | 157.0 → 143.0 / 160.6 → 143.9 | 323.2 → 335.4 | 449.7 → 455.8 |

Image/buffer comparisons: 153; exact: 153. Maximum absolute difference: 0. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.

The main intended gains are lower ordinary-render storage and transparency cost: alpha64 sustained mean 2255→1321 ms and fresh RSS 2038.5→1030.4 MiB; grid1m RSS 1267.2→1083.1 MiB. All 153 large image/buffer comparisons are exact. Debug-all runs still need auxiliary data; their combined diagnostic-process RSS can be higher than the predecessor and is retained in the summaries.

Public regressions remain visible: environment sustained mean 240.6→246.8 ms, toon 323.2→335.4 ms, with small RSS increases on toon and SSAO. Small-scene warm time rises while its sustained mean falls. The very large forced-GC SSAO difference does not reproduce in sustained loops (441.4→431.2 ms); it is not evidence that the unchanged single-worker SSAO kernel became twice as fast. Final validation includes repeated cases before deciding how to characterize these results.
