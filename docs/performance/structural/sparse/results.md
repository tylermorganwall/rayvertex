# Measured phase: 18751b4 → 2ed69d4

800×800; FSAA 1; one worker; unchanged fixture/lighting/quality. Three warm samples; three separate uninstrumented native samples; three diagnostic profiles. Cold is the first render in a fresh R process including preparation/decode, with OS file caches uncontrolled. Fresh-render RSS is a separate one-render process. Sustained mean covers five ordinary calls plus final release/GC; warm/native samples force GC outside each timer.

| Scene | Cold ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| alpha16 | 1100.0 → 901.0 | 1012.0 → 800.0 / 1015.6 → 809.9 | 858.0 → 655.0 / 858.9 → 660.4 | 1011.4 → 802.0 | 827.8 → 750.7 |
| alpha4 | 503.0 → 461.0 | 381.0 → 334.0 / 404.4 → 355.6 | 229.0 → 193.0 / 229.0 → 193.9 | 400.0 → 348.8 | 436.7 → 472.1 |
| alpha64 | 3219.0 → 2400.0 | 3152.0 → 2260.0 / 3180.8 → 2287.9 | 3005.0 → 2120.0 / 3010.4 → 2129.0 | 3156.4 → 2285.8 | 2257.0 → 2043.1 |
| grid1m | 604.0 → 597.0 | 528.0 → 540.0 / 560.4 → 543.6 | 364.0 → 364.0 / 367.6 → 364.9 | 572.6 → 565.8 | 1136.2 → 1137.0 |
| shadow | 357.0 → 351.0 | 244.0 → 232.0 / 249.4 → 240.1 | 79.0 → 77.0 / 80.8 → 77.9 | 252.6 → 247.2 | 388.3 → 405.8 |
| small | 300.0 → 286.0 | 190.0 → 190.0 / 196.3 → 197.2 | 31.0 → 30.0 / 31.9 → 30.9 | 194.2 → 195.2 | 405.3 → 456.8 |

Image/buffer comparisons: 102; exact: 102. Maximum absolute difference: 0. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.
