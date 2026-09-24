# Measured phase: b755958 → eec25f7

800×800; FSAA 1; one worker; unchanged quality. Both variants use explicit prepared handles. Three warm public samples, three isolated native samples and three diagnostics; five-frame sustained loops include final output cleanup. The first-render column excludes preparation; the table below includes preparation plus the first render. Fresh RSS includes fixture construction, preparation and one render. OS file caches are uncontrolled.

| Scene | First prepared render ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| environment | 325.0 → 174.0 | 256.0 → 86.0 / 261.4 → 88.7 | 52.0 → 52.0 / 53.8 → 52.0 | 257.4 → 104.8 | 376.1 → 183.3 |
| grid1m | 484.0 → 321.0 | 380.0 → 229.0 / 383.6 → 229.0 | 237.0 → 180.0 / 243.3 → 180.0 | 420.0 → 257.8 | 1107.8 → 774.0 |
| shared_textures | 272.0 → 110.0 | 207.0 → 32.0 / 219.6 → 32.9 | 18.0 → 17.0 / 18.9 → 17.9 | 200.0 → 60.2 | 405.7 → 232.0 |
| small | 263.0 → 106.0 | 208.0 → 23.0 / 211.6 → 26.6 | 8.0 → 8.0 / 8.0 → 8.0 | 192.0 → 49.6 | 378.1 → 205.7 |

Image/buffer comparisons: 68; exact: 68. Maximum absolute difference: 0. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.

| Scene | Preparation ms | Preparation + first render ms |
|---|---:|---:|
| environment | 2.0 → 2.0 | 333.0 → 183.0 |
| grid1m | 26.0 → 31.0 | 516.0 → 358.0 |
| shared_textures | 7.0 → 8.0 | 286.0 → 124.0 |
| small | 1.0 → 2.0 | 271.0 → 115.0 |
