# Prepared scenes: ordinary list → explicit snapshot at `f44b9b6`

800×800; FSAA 1; one worker; unchanged fixture/lighting/quality. Three warm samples; three separate uninstrumented native samples; three diagnostic profiles. Both variants use the same installed revision. The before variant uses ordinary lists; the after variant explicitly calls `prepare_scene()`. The table’s after Cold column times the first render **after preparation**; the complete preparation-plus-first cost is reported below. OS file caches are uncontrolled. Fresh-render RSS is a separate one-render process. Sustained mean covers five calls plus final output release/GC (the prepared handle stays live); warm/native samples force GC outside each timer.

| Scene | Cold ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| alpha16 | 569.0 → 568.0 | 510.0 → 511.0 / 512.7 → 512.8 | 332.0 → 328.0 / 336.5 → 328.9 | 508.0 → 508.4 | 416.1 → 394.3 |
| environment | 305.0 → 302.0 | 240.0 → 241.0 / 251.7 → 259.9 | 50.0 → 53.0 / 50.9 → 53.0 | 243.8 → 242.6 | 402.0 → 386.5 |
| grid1m | 550.0 → 559.0 | 484.0 → 445.0 / 500.2 → 463.0 | 306.0 → 306.0 / 306.0 → 325.8 | 504.6 → 488.6 | 1101.5 → 1033.8 |
| shadow | 288.0 → 280.0 | 224.0 → 227.0 / 251.0 → 236.0 | 44.0 → 45.0 / 44.9 → 45.0 | 213.4 → 216.6 | 347.1 → 402.8 |
| shared_textures | 274.0 → 272.0 | 208.0 → 201.0 / 209.8 → 206.4 | 22.0 → 20.0 / 22.0 → 20.0 | 202.4 → 192.2 | 365.8 → 362.5 |
| small | 241.0 → 240.0 | 186.0 → 188.0 / 189.6 → 191.6 | 7.0 → 8.0 / 7.0 → 8.0 | 177.0 → 178.0 | 349.3 → 350.5 |

Image/buffer comparisons: 102; exact: 102. Maximum absolute difference: 0. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.

## Preparation cost and reuse

| Scene | Preparation ms | Prepare + first render ms | Ordinary first ms | Estimated amortization renders |
|---|---:|---:|---:|---:|
| alpha16 | 2.0 | 575.0 | 569.0 | No demonstrated benefit |
| environment | 2.0 | 311.0 | 305.0 | No demonstrated benefit |
| grid1m | 25.0 | 590.0 | 550.0 | 2 |
| shadow | 1.0 | 288.0 | 288.0 | No demonstrated benefit |
| shared_textures | 7.0 | 286.0 | 274.0 | 1 |
| small | 1.0 | 248.0 | 241.0 | No demonstrated benefit |

Amortization is preparation cost divided by the observed sustained saving, rounded up; it is an estimate from these short loops, not a guaranteed crossover. Differences ≤2 ms are treated as unresolved. Preparation-plus-first includes the driver’s intervening loop setup and has no forced GC between preparation and rendering. Prepared handles retain the merged geometry snapshot and decoded textures, but per-frame ModelInfo, view/light setup, shadow maps, and environment variants still rebuild. This is not a persistent fully packed native geometry cache.

All 245 ordinary and all 245 prepared render/debug corpus outputs match the corrected clipped scalar reference exactly. Tests cover source-list mutation, texture overwrite/delete, explicit rebuild, camera/light/FSAA changes, R callback semantics, serialization/fork rejection, and native owner/texture-byte counters returning to baseline after GC and partial decode failure. These counters verify these owners; they are not a general leak detector. The API is additive and opt-in.
