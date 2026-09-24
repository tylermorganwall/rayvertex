# Measured phase: 5f0853f → fbe4359

800×800; FSAA 1; one worker; unchanged fixture/lighting/quality. Three warm samples; three separate uninstrumented native samples; three diagnostic profiles. Cold is the first render in a fresh R process including preparation/decode, with OS file caches uncontrolled. Fresh-render RSS is a separate one-render process. Sustained mean covers five ordinary calls plus final release/GC; warm/native samples force GC outside each timer.

| Scene | Cold ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| alpha16 | 591.0 → 565.0 | 525.0 → 510.0 / 532.2 → 511.8 | 344.0 → 326.0 / 345.8 → 326.9 | 528.4 → 508.6 | 385.8 → 376.2 |
| environment | 311.0 → 307.0 | 245.0 → 243.0 / 254.0 → 248.4 | 51.0 → 50.0 / 55.5 → 50.0 | 244.0 → 242.4 | 370.3 → 354.7 |
| environment_shared | 347.0 → 311.0 | 281.0 → 259.0 / 299.9 → 278.8 | 85.0 → 54.0 / 86.8 → 54.9 | 282.4 → 251.0 | 474.4 → 355.9 |
| point_lights | 321.0 → 305.0 | 238.0 → 237.0 / 283.0 → 266.7 | 72.0 → 63.0 / 72.9 → 64.8 | 253.6 → 239.8 | 364.2 → 369.9 |
| shadow | 256.0 → 269.0 | 205.0 → 199.0 / 223.0 → 221.5 | 21.0 → 20.0 / 22.8 → 20.0 | 196.0 → 192.2 | 336.5 → 376.9 |
| small | 252.0 → 251.0 | 188.0 → 188.0 / 204.2 → 191.6 | 7.0 → 7.0 / 7.9 → 7.0 | 179.8 → 180.8 | 349.8 → 358.8 |

Image/buffer comparisons: 102; exact: 102. Maximum absolute difference: 0. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.


Shared environment preparation retains the exact down/up cubic B-spline resize. Variants use the original source identity (frame-local) plus resized dimensions; unblurred materials share the immutable original. Background sampling gets a separate variant, so it never mutates material assets. The 16-material gradient-map fixture issues 17 requests but produces one resized variant (6 MiB), including reflection, refraction, unblurred views and two sharpness values that map to identical integer dimensions. Fresh RSS falls 474.4→355.9 MiB; this targeted saving does not imply that distinct variants are free. The single-material environment now retains a separate background variant, increasing explicit variant payload by one image.

Point-light direction and attenuation share one displacement/distance calculation and Phong reuses attenuation for diffuse/specular. Operation order and mixed numeric types stay unchanged. 100,000 randomized samples match both scalar light functions exactly under ASan/UBSan. Coincident point-light directions retain the legacy exceptional normalization fallback; a changed zero-distance lighting convention remains a separate correction, not part of this optimization.

Main/shadow pass templates remove the new depth-only decision from the main color loop. The alpha16 regression observed in the preceding phase disappears in this comparison (528.4→508.6 ms sustained). The default visibility experiment remains disabled. All 245 corrected-reference outputs pass. Source tests include variant request/payload assertions and serial/parallel equality. The first point-light fixture invocation failed because the driver passed eight arguments to the two-argument light builder; it was corrected to combine light rows before measuring. The failed setup log is retained and contributes no timing samples.
