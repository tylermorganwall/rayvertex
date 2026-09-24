# Measured phase: 05cec195 → 511577a3

800×800; FSAA 1; one worker; unchanged fixture/lighting/quality. Three warm samples; three separate uninstrumented native samples; three diagnostic profiles. Cold is the first render in a fresh R process including preparation/decode, with OS file caches uncontrolled. Fresh-render RSS is a separate one-render process. Sustained mean covers five ordinary calls plus final release/GC; warm/native samples force GC outside each timer.

| Scene | Cold ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| grid1m | 266.0 → 263.0 | 236.0 → 234.0 / 236.9 → 234.9 | 176.0 → 171.0 / 176.9 → 172.8 | 274.0 → 255.6 | 714.5 → 712.7 |
| normal_indexed | 313.0 → 315.0 | 259.0 → 258.0 / 264.4 → 258.9 | 196.0 → 195.0 / 196.9 → 200.4 | 278.6 → 283.4 | 656.0 → 645.1 |
| normal_shared | 309.0 → 309.0 | 256.0 → 254.0 / 256.0 → 254.9 | 199.0 → 197.0 / 199.0 → 197.9 | 269.4 → 272.0 | 676.5 → 700.2 |
| shadow | 109.0 → 107.0 | 37.0 → 35.0 / 37.9 → 36.8 | 20.0 → 19.0 / 20.0 → 19.0 | 58.8 → 56.6 | 197.7 → 196.9 |
| small | 95.0 → 96.0 | 23.0 → 22.0 / 23.0 → 22.9 | 8.0 → 7.0 / 8.9 → 7.0 | 46.0 → 43.8 | 192.8 → 204.4 |
| ssao | 245.0 → 236.0 | 154.0 → 156.0 / 158.5 → 156.0 | 106.0 → 108.0 / 106.0 → 108.0 | 161.6 → 157.4 | 269.6 → 262.5 |
| tangent_overdraw | 705.0 → 660.0 | 630.0 → 591.0 / 729.9 → 600.9 | 597.0 → 592.0 / 599.7 → 904.3 | 628.6 → 640.8 | 204.2 → 202.0 |

Image/buffer comparisons: 119; exact: 119. Maximum absolute difference: 0. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.

`RAYVERTEX_NORMAL_CACHE=1` is enabled for the new library; the older baseline ignores it. The default remains off. Indexed normals are cached lazily on the main thread, with a 64 MiB aggregate payload limit and scalar fallback when that budget is insufficient. Caches share only bitwise-identical transformation matrices and identical pre-transform normalization conventions. Every accessed index retains its previous validation. Unused normal rows are not evaluated. Geometric fallback normals reuse the original first-vertex result within the triangle, preserving each shader's vec3/vec4 normalization order. Matrix arithmetic and precision are unchanged.

The shared-normal fixture records one cache miss and 3,007,583 hits in 25 bytes. This removes redundant transforms but does not establish a substantial public-call gain: 256 → 254 ms warm, 269.4 → 272.0 ms sustained. Indexed-normal sustained timing similarly regresses 278.6 → 283.4 ms. The geometric grid improves 176 → 171 ms native, while SSAO regresses 106 → 108 ms. The tangent-overdraw native p95 outlier (904.3 ms) and sustained regression are retained. Small/RSS variation is not assigned a proven cause. These results justify leaving the experiment opt-in.

All 74 focused assertions, 245 corrected scalar outputs and 44 additional shader/debug outputs pass exactly. Focused tests include non-unit normals, separate normal indices, mixed geometric/indexed faces, unused zero normals, mixed shader normalization conventions and prepared scenes. All 119 phase image/buffer comparisons are exact. Logs are in [validation](validation/).
