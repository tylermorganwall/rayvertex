# Measured phase: 811143b → d48a259

800×800; FSAA 1; one worker; unchanged fixture/lighting/quality. Three warm samples; three separate uninstrumented native samples; three diagnostic profiles. Cold is the first render in a fresh R process including preparation/decode, with OS file caches uncontrolled. Fresh-render RSS is a separate one-render process. Sustained mean covers five ordinary calls plus final release/GC; warm/native samples force GC outside each timer.

| Scene | Cold ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| small | 243.0 → 254.0 | 180.0 → 186.0 / 198.9 → 193.2 | 8.0 → 7.0 / 8.0 → 7.9 | 180.0 → 181.4 | 386.0 → 370.5 |
| ssao | 893.0 → 391.0 | 776.0 → 272.0 / 780.5 → 278.3 | 612.0 → 106.0 / 612.0 → 108.7 | 478.6 → 285.6 | 431.0 → 427.8 |
| toon | 406.0 → 402.0 | 326.0 → 324.0 / 329.6 → 324.9 | 145.0 → 146.0 / 151.3 → 148.7 | 331.2 → 332.6 | 458.3 → 450.9 |

Image/buffer comparisons: 51; exact: 51. Maximum absolute difference: 0. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.


The expanded worker sweep exposed slow SSAO in `811143b` (one-worker native median 601 ms, with diagnostic SSAO 590.16 ms). The sweep was stopped before completion and retained as diagnostic evidence. `d48a259` explicitly composes `vp * Projection` once before the sampling loop and retains the original multiplication order for each sample. Kernel size remains 64, the noise sequence and corrected box blur remain unchanged, and no output quality setting changes. All 245 scalar-reference results and all 51 phase image/buffer comparisons are exact.

The native median improves 612→106 ms in this targeted batch, while sustained public mean improves 478.6→285.6 ms. These are distinct regimes: forced-GC/native samples and sustained calls do not exhibit the same ratio. Small and toon sustained means slightly increase (1.4 ms each); their native differences are at timer resolution. Fresh RSS is effectively unchanged for SSAO. Final validation and complete sweeps were restarted on this renderer; the interrupted pre-fix results are not presented as final performance.
