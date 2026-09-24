# Measured phase: b755958 → eec25f7

800×800; FSAA 1; one worker; unchanged fixture/lighting/quality. Three warm samples; three separate uninstrumented native samples; three diagnostic profiles. Cold is the first render in a fresh R process including preparation/decode, with OS file caches uncontrolled. Fresh-render RSS is a separate one-render process. Sustained mean covers five ordinary calls plus final release/GC; warm/native samples force GC outside each timer.

| Scene | Cold ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| alpha16 | 573.0 → 403.0 | 508.0 → 327.0 / 509.8 → 327.9 | 328.0 → 310.0 / 332.5 → 312.7 | 509.4 → 350.0 | 408.3 → 369.6 |
| alpha4 | 337.0 → 173.0 | 272.0 → 100.0 / 273.8 → 100.0 | 89.0 → 83.0 / 89.9 → 84.8 | 272.4 → 123.8 | 386.4 → 205.8 |
| alpha64 | 1396.0 → 1201.0 | 1341.0 → 1115.0 / 1351.8 → 1123.1 | 1152.0 → 1095.0 / 1160.1 → 1099.5 | 1324.0 → 1135.4 | 1034.6 → 1033.0 |
| environment | 303.0 → 151.0 | 249.0 → 78.0 / 256.2 → 78.9 | 52.0 → 50.0 / 56.5 → 50.0 | 245.0 → 99.2 | 389.0 → 194.6 |
| grid100k | 281.0 → 131.0 | 212.0 → 54.0 / 221.9 → 54.0 | 38.0 → 30.0 / 38.0 → 30.9 | 205.2 → 79.6 | 461.0 → 277.7 |
| grid1m | 496.0 → 276.0 | 392.0 → 249.0 / 410.9 → 253.5 | 217.0 → 176.0 / 223.3 → 177.8 | 423.2 → 258.8 | 1102.7 → 674.0 |
| grid500k | 385.0 → 198.0 | 275.0 → 147.0 / 312.8 → 152.4 | 126.0 → 103.0 / 127.8 → 103.0 | 304.4 → 158.2 | 656.8 → 448.8 |
| occluded | 358.0 → 206.0 | 269.0 → 113.0 / 286.1 → 116.6 | 94.0 → 87.0 / 94.9 → 87.9 | 283.0 → 142.6 | 421.3 → 274.8 |
| overdraw | 689.0 → 473.0 | 587.0 → 396.0 / 606.8 → 396.9 | 392.0 → 349.0 / 403.7 → 367.9 | 575.0 → 430.2 | 446.1 → 283.0 |
| shadow | 265.0 → 110.0 | 215.0 → 36.0 / 228.5 → 36.9 | 20.0 → 19.0 / 20.0 → 19.0 | 192.0 → 60.6 | 353.4 → 201.4 |
| shared_textures | 275.0 → 117.0 | 204.0 → 36.0 / 210.3 → 36.9 | 20.0 → 19.0 / 20.0 → 19.9 | 202.4 → 60.8 | 396.9 → 210.8 |
| small | 256.0 → 99.0 | 184.0 → 22.0 / 186.7 → 23.8 | 7.0 → 7.0 / 7.0 → 7.0 | 181.0 → 45.2 | 360.2 → 208.2 |
| ssao | 388.0 → 226.0 | 269.0 → 150.0 / 282.5 → 150.9 | 107.0 → 107.0 / 116.9 → 107.0 | 285.4 → 158.2 | 428.4 → 263.4 |
| toon | 409.0 → 255.0 | 331.0 → 187.0 / 334.6 → 203.2 | 146.0 → 142.0 / 146.9 → 142.9 | 334.8 → 195.2 | 467.4 → 315.5 |

Image/buffer comparisons: 238; exact: 238. Maximum absolute difference: 0. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.

The occluded and overdraw rows use a fresh sequential rerun because automatic Git maintenance may have overlapped those two cases during the initial sweep. Their original artifacts remain in [delivery-first-pass](../delivery-first-pass/README.md); the primary raw records and settings identify the replacement. No results were selected by whether they were faster, and samples were not pooled.
