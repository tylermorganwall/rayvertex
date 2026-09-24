# Measured phase: 2ed69d4 → 175c43a

800×800; FSAA 1; one worker; unchanged fixture/lighting/quality. Three warm samples; three separate uninstrumented native samples; three diagnostic profiles. Cold is the first render in a fresh R process including preparation/decode, with OS file caches uncontrolled. Fresh-render RSS is a separate one-render process. Sustained mean covers five ordinary calls plus final release/GC; warm/native samples force GC outside each timer.

| Scene | Cold ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| alpha16 | 895.0 → 881.0 | 780.0 → 760.0 / 804.3 → 784.3 | 645.0 → 623.0 / 650.4 → 633.8 | 797.4 → 782.0 | 763.5 → 753.0 |
| alpha4 | 468.0 → 453.0 | 334.0 → 322.0 / 349.3 → 335.5 | 191.0 → 179.0 / 191.9 → 179.9 | 346.2 → 340.0 | 455.9 → 444.8 |
| alpha64 | 2350.0 → 2338.0 | 2256.0 → 2253.0 / 2300.1 → 2258.4 | 2122.0 → 2080.0 / 2122.9 → 2089.9 | 2310.4 → 2282.0 | 2042.2 → 2038.7 |
| grid1m | 593.0 → 561.0 | 532.0 → 490.0 / 541.0 → 499.9 | 363.0 → 331.0 / 390.9 → 349.0 | 578.4 → 531.2 | 1173.6 → 1283.9 |
| shadow | 359.0 → 346.0 | 240.0 → 229.0 / 240.9 → 239.8 | 76.0 → 67.0 / 76.0 → 67.9 | 248.0 → 238.6 | 444.1 → 404.0 |
| small | 284.0 → 280.0 | 188.0 → 177.0 / 190.7 → 197.7 | 31.0 → 26.0 / 32.8 → 28.7 | 195.0 → 189.2 | 428.7 → 426.2 |

Image/buffer comparisons: 102; exact: 102. Maximum absolute difference: 0. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.
