# Measured phase: dbaf8b1 → eec25f7

800×800; FSAA 1; one worker; unchanged fixture/lighting/quality. Three warm samples; three separate uninstrumented native samples; three diagnostic profiles. Cold is the first render in a fresh R process including preparation/decode, with OS file caches uncontrolled. Fresh-render RSS is a separate one-render process. Sustained mean covers five ordinary calls plus final release/GC; warm/native samples force GC outside each timer.

| Scene | Cold ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| alpha16 | 414.0 → 397.0 | 343.0 → 323.0 / 343.9 → 325.7 | 329.0 → 315.0 / 331.7 → 318.6 | 364.4 → 348.0 | 368.9 → 365.3 |
| environment | 150.0 → 157.0 | 77.0 → 78.0 / 77.9 → 78.0 | 50.0 → 50.0 / 52.7 → 52.7 | 99.8 → 98.4 | 186.7 → 180.3 |
| grid100k | 134.0 → 141.0 | 61.0 → 53.0 / 65.5 → 54.8 | 34.0 → 31.0 / 34.0 → 31.0 | 86.2 → 77.8 | 291.0 → 271.9 |
| grid1m | 316.0 → 265.0 | 276.0 → 240.0 / 284.1 → 241.8 | 216.0 → 176.0 / 225.0 → 182.3 | 291.2 → 249.4 | 943.7 → 677.9 |
| point_lights | 156.0 → 156.0 | 80.0 → 79.0 / 83.6 → 82.6 | 60.0 → 59.0 / 60.0 → 59.9 | 106.0 → 103.0 | 207.0 → 201.4 |
| shadow | 114.0 → 113.0 | 38.0 → 37.0 / 38.9 → 38.8 | 20.0 → 19.0 / 20.9 → 19.0 | 60.6 → 59.2 | 207.2 → 200.3 |
| shared_textures | 111.0 → 125.0 | 38.0 → 39.0 / 39.8 → 40.8 | 19.0 → 18.0 / 19.0 → 18.9 | 62.6 → 60.4 | 234.2 → 221.5 |
| small | 97.0 → 99.0 | 22.0 → 23.0 / 23.8 → 23.9 | 8.0 → 7.0 / 8.0 → 7.0 | 45.6 → 44.2 | 199.8 → 204.2 |
| ssao | 223.0 → 225.0 | 153.0 → 150.0 / 153.9 → 152.7 | 107.0 → 110.0 / 107.9 → 110.0 | 157.4 → 157.2 | 254.1 → 252.9 |
| toon | 254.0 → 263.0 | 197.0 → 185.0 / 212.3 → 212.9 | 144.0 → 142.0 / 144.9 → 144.7 | 194.6 → 193.6 | 309.0 → 299.4 |

Image/buffer comparisons: 170; exact: 170. Maximum absolute difference: 0. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.
