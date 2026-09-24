# Measured phase: b755958 → 69ae102

800×800; FSAA 1; one worker; unchanged fixture/lighting/quality. Three warm samples; three separate uninstrumented native samples; three diagnostic profiles. Cold is the first render in a fresh R process including preparation/decode, with OS file caches uncontrolled. Fresh-render RSS is a separate one-render process. Sustained mean covers five ordinary calls plus final release/GC; warm/native samples force GC outside each timer.

| Scene | Cold ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| alpha16 | 615.0 → 445.0 | 540.0 → 364.0 / 545.4 → 394.6 | 347.0 → 346.0 / 347.9 → 371.2 | 538.4 → 384.2 | 443.1 → 370.2 |
| environment | 330.0 → 167.0 | 266.0 → 81.0 / 266.0 → 81.9 | 53.0 → 54.0 / 53.9 → 54.0 | 264.4 → 105.6 | 373.3 → 190.2 |
| grid100k | 319.0 → 153.0 | 228.0 → 64.0 / 237.9 → 64.9 | 41.0 → 38.0 / 42.8 → 39.8 | 218.2 → 98.0 | 444.1 → 291.5 |
| grid1m | 536.0 → 338.0 | 439.0 → 297.0 / 439.9 → 316.8 | 234.0 → 226.0 / 237.6 → 226.9 | 437.4 → 313.0 | 1078.8 → 930.4 |
| shadow | 289.0 → 120.0 | 226.0 → 39.0 / 240.4 → 39.9 | 22.0 → 22.0 / 22.9 → 22.9 | 205.0 → 67.4 | 376.7 → 200.2 |
| small | 278.0 → 109.0 | 207.0 → 23.0 / 207.9 → 23.9 | 8.0 → 8.0 / 8.9 → 8.9 | 196.6 → 49.6 | 385.0 → 201.2 |
| ssao | 423.0 → 237.0 | 290.0 → 158.0 / 290.0 → 167.9 | 113.0 → 114.0 / 113.9 → 114.9 | 300.6 → 166.2 | 473.7 → 256.6 |
| toon | 449.0 → 283.0 | 355.0 → 196.0 / 358.6 → 215.8 | 150.0 → 147.0 / 150.9 → 147.9 | 353.8 → 205.4 | 485.1 → 308.2 |

Image/buffer comparisons: 136; exact: 136. Maximum absolute difference: 0. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.


The native rasterizer is unchanged in this phase. `69ae102` composes planar assembly, the existing sRGB decoding arithmetic, and flip/transpose in a serial native traversal, then uses the public rayimage constructor for metadata. A second native traversal clones and clamps only RGB, preserving alpha and resetting camera metadata through rayimage as before. Tone mapping, bloom, Mitchell FSAA resize, plotting and encoding keep their original order and implementations. `RAYVERTEX_REFERENCE_OUTPUT=1` retains the original R output path. One-pixel dimensions use the dependency's existing clamp path, including its current dimension-dropping error; this phase does not silently fix that behavior.

The separate 11-sample stage comparison reports assembly/decode/orient median/p95 105/110 → 16/17.5 ms and clamp 41/42.5 → 5/5.5 ms. Corresponding R allocations are 279,086,680 → 40,960,096 and 153,679,296 → 40,960,096 bytes. These helper-only calls are diagnostic evidence; the table above times complete public calls, and the native raster times correctly show little change. Small/frame timing variation remains visible.

All 1,142 release assertions pass. Forty new assertions cover independent channel values, rectangular/singleton dimensions, threshold/extreme/NA/NaN/infinite samples, alpha, unchanged inputs, metadata, all tone maps, bloom, FSAA, SSAO/shadows, prepared scenes, debug-all and byte-identical PNG encoding. All 245 scalar-corpus results and all 136 phase image/buffer comparisons are exact. The high-resolution quality sweep and final sanitizer checks are recorded separately.
