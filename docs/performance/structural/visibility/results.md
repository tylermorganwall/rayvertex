# Measured phase: 7671c4f → 68c986e

800×800; FSAA 1; one worker; unchanged fixture/lighting/quality. Three warm samples; three separate uninstrumented native samples; three diagnostic profiles. Cold is the first render in a fresh R process including preparation/decode, with OS file caches uncontrolled. Fresh-render RSS is a separate one-render process. Sustained mean covers five ordinary calls plus final release/GC; warm/native samples force GC outside each timer.

| Scene | Cold ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| alpha4 | 340.0 → 344.0 | 271.0 → 273.0 / 275.5 → 281.1 | 90.0 → 90.0 / 95.4 → 96.3 | 282.4 → 280.0 | 388.7 → 402.2 |
| environment | 319.0 → 318.0 | 261.0 → 255.0 / 306.0 → 256.8 | 52.0 → 52.0 / 53.8 → 52.9 | 251.6 → 254.4 | 375.5 → 356.2 |
| grid1m | 586.0 → 605.0 | 507.0 → 500.0 / 508.8 → 503.6 | 312.0 → 308.0 / 314.7 → 312.5 | 516.0 → 515.0 | 1097.1 → 1046.6 |
| occluded | 359.0 → 348.0 | 290.0 → 266.0 / 293.6 → 284.0 | 102.0 → 95.0 / 102.0 → 99.5 | 284.2 → 269.8 | 446.9 → 413.0 |
| overdraw | 645.0 → 359.0 | 561.0 → 267.0 / 576.3 → 295.8 | 379.0 → 98.0 / 398.8 → 99.8 | 559.0 → 278.0 | 432.2 → 443.2 |
| shadow | 294.0 → 298.0 | 238.0 → 241.0 / 265.0 → 263.5 | 46.0 → 47.0 / 46.0 → 47.9 | 222.4 → 226.0 | 361.2 → 403.8 |
| small | 280.0 → 262.0 | 196.0 → 191.0 / 200.5 → 197.3 | 8.0 → 8.0 / 8.0 → 8.9 | 187.8 → 187.2 | 348.5 → 346.9 |

Image/buffer comparisons: 119; exact: 119. Maximum absolute difference: 0. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.

After measurements explicitly enable `RAYVERTEX_VISIBILITY=1`; the delivered default remains forward rendering. The predecessor ignores that variable. The overdraw scene improves from 559→278 ms sustained and 379→98 ms native, with exact images. Front-to-back occluded geometry gains much less (284.2→269.8 ms sustained), and the million-triangle scene is essentially unchanged. Shadow/environment sustained means regress slightly; RSS results are mixed. No broad default speedup is claimed from this experiment. Both library paths and the environment setting are retained in `settings.json`.
