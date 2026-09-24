# Measured phase: 511577a3 → 5eb61c63

800×800; FSAA 1; one worker; unchanged fixture/lighting/quality. Three warm samples; three separate uninstrumented native samples; three diagnostic profiles. Cold is the first render in a fresh R process including preparation/decode, with OS file caches uncontrolled. Fresh-render RSS is a separate one-render process. Sustained mean covers five ordinary calls plus final release/GC; warm/native samples force GC outside each timer.

| Scene | Cold ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| environment | 167.0 → 160.0 | 77.0 → 78.0 / 78.8 → 79.8 | 53.0 → 49.0 / 54.8 → 50.8 | 104.2 → 104.8 | 192.2 → 196.8 |
| grid1m | 317.0 → 289.0 | 253.0 → 239.0 / 261.1 → 245.3 | 182.0 → 195.0 / 184.7 → 199.5 | 263.8 → 260.4 | 713.0 → 710.8 |
| shadow | 122.0 → 111.0 | 38.0 → 32.0 / 38.9 → 32.9 | 19.0 → 19.0 / 19.0 → 19.0 | 64.0 → 56.6 | 204.1 → 185.5 |
| small | 105.0 → 108.0 | 24.0 → 18.0 / 24.0 → 20.7 | 7.0 → 7.0 / 7.9 → 7.0 | 49.2 → 41.8 | 203.4 → 183.2 |
| ssao | 228.0 → 227.0 | 151.0 → 151.0 / 153.7 → 151.0 | 107.0 → 108.0 / 107.9 → 108.0 | 163.2 → 154.8 | 247.3 → 235.0 |
| toon | 274.0 → 265.0 | 186.0 → 178.0 / 204.0 → 203.2 | 144.0 → 144.0 / 145.8 → 146.7 | 200.2 → 215.8 | 317.7 → 300.7 |

Image/buffer comparisons: 102; exact: 102. Maximum absolute difference: 0. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.

The normal reuse experiment is disabled in both variants. The ordinary SSAO/background composition is fused after the unchanged R exponentiation and before the unchanged sRGB decode. Reference and debug routes preserve the old operations. This reduces R RGB intermediates and full-image background writes without changing tone mapping, bloom, FSAA, or output attributes. Native raster time is not expected to improve from this phase. The grid native median increases in this run despite no raster change, and toon sustained time increases; those results are retained rather than explained away.

Validation before measurement: 28 new composition assertions, 42 existing output assertions, and all 245 corrected scalar results pass exactly. A separate HD/FSAA-2 comparison follows.

| Scene | Assembly boundary ms (one diagnostic) | R allocations MiB (one public call) |
|---|---:|---:|
| environment | 25.0 → 25.0 | 107.6 → 107.6 |
| grid1m | 15.0 → 11.0 | 271.5 → 252.9 |
| shadow | 14.0 → 9.0 | 129.7 → 110.6 |
| small | 11.0 → 6.0 | 128.3 → 107.4 |
| ssao | 19.0 → 13.0 | 185.3 → 151.5 |
| toon | 18.0 → 12.0 | 163.2 → 144.2 |

The assembly boundary includes SSAO/background R work, native assembly/decode and rayimage metadata; it is a single instrumented interval, not a warm distribution. Allocation totals include the entire public call, not just this boundary.
