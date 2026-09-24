# Historical candidate defaults — measured phase: 05cec195 → 5bdfaaa5

800×800; FSAA 1; one worker; unchanged quality. Both variants use explicit prepared handles. Three warm public samples, three isolated native samples and three diagnostics; five-frame sustained loops include final output cleanup. The first-render column excludes preparation; the table below includes preparation plus the first render. Fresh RSS includes fixture construction, preparation and one render. OS file caches are uncontrolled.

| Scene | First prepared render ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| normal_shared | 328.0 → 339.0 | 250.0 → 248.0 / 251.8 → 248.9 | 204.0 → 210.0 / 205.8 → 213.6 | 274.6 → 268.4 | 713.8 → 658.4 |
| small | 102.0 → 95.0 | 23.0 → 16.0 / 23.0 → 17.8 | 8.0 → 7.0 / 8.0 → 7.9 | 45.4 → 39.2 | 208.2 → 184.9 |
| ssao | 222.0 → 212.0 | 149.0 → 142.0 / 149.9 → 142.9 | 107.0 → 106.0 / 107.9 → 106.0 | 153.6 → 162.0 | 257.9 → 229.3 |

Image/buffer comparisons: 51; exact: 51. Maximum absolute difference: 0. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.

| Scene | Preparation ms | Preparation + first render ms |
|---|---:|---:|
| normal_shared | 24.0 → 26.0 | 358.0 → 371.0 |
| small | 1.0 → 2.0 | 110.0 → 103.0 |
| ssao | 2.0 → 2.0 | 230.0 → 220.0 |

These results measure the now-retired tangent candidate build with its switch off. Accepted final defaults are measured separately in `../accepted-prepared/results.md`; these records are retained for audit, not substituted for final measurements.
