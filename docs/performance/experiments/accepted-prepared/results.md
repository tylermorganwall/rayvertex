# Measured phase: 05cec195 → f1d7e122

800×800; FSAA 1; one worker; unchanged quality. Both variants use explicit prepared handles. Three warm public samples, three isolated native samples and three diagnostics; five-frame sustained loops include final output cleanup. The first-render column excludes preparation; the table below includes preparation plus the first render. Fresh RSS includes fixture construction, preparation and one render. OS file caches are uncontrolled.

| Scene | First prepared render ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| normal_shared | 335.0 → 319.0 | 247.0 → 239.0 / 251.5 → 250.7 | 200.0 → 199.0 / 203.6 → 199.9 | 265.4 → 263.8 | 709.8 → 654.1 |
| small | 95.0 → 98.0 | 23.0 → 16.0 / 30.2 → 17.8 | 8.0 → 7.0 / 8.0 → 7.9 | 45.4 → 39.6 | 199.5 → 186.9 |
| ssao | 222.0 → 216.0 | 151.0 → 147.0 / 154.6 → 148.8 | 108.0 → 108.0 / 109.8 → 108.9 | 160.2 → 155.8 | 251.9 → 230.8 |

Image/buffer comparisons: 51; exact: 51. Maximum absolute difference: 0. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.

| Scene | Preparation ms | Preparation + first render ms |
|---|---:|---:|
| normal_shared | 24.0 → 26.0 | 365.0 → 350.0 |
| small | 2.0 → 2.0 | 103.0 → 106.0 |
| ssao | 2.0 → 1.0 | 230.0 → 224.0 |

The accepted renderer at `f1d7e122` is source-equivalent to the immutable `5eb61c63`/`lib-composition` build used here, ignoring whitespace. All optional experiments are disabled. Both revisions prepare explicit handles; these numbers compare revisions, not prepared versus ordinary renders in different processes. Preparation-plus-first includes elapsed driver overhead and is measured directly rather than reconstructed by adding the displayed columns.
