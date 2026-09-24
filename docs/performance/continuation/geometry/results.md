# geometry sweep: `dbaf8b1` → `eec25f7`

Three warm public calls and three isolated native samples per setting, with GC outside their timers. One separate diagnostic sample per setting supplies phase timings/counters; its p95 is not a distribution. Variant order alternates between settings. Quality is identical within each pair. Cold time, R allocations and diagnostic-process RSS remain in the CSV summaries. These short samples do not establish stable tail latency.

| Scene | Output / FSAA / workers | Public median / p95 ms | Native median / p95 ms | Fresh RSS MiB |
|---|---|---:|---:|---:|
| grid100k | 1920×1080 / 2 / 4 | 1239.0 → 1243.0 / 1251.6 → 1254.7 | 103.0 → 100.0 / 103.9 → 100.9 | 1770.1 → 1746.2 |
| grid1m | 800×800 / 1 / 1 | 285.0 → 239.0 / 312.0 → 239.9 | 228.0 → 176.0 / 229.8 → 177.8 | — → — |
| grid1m | 800×800 / 1 / 2 | 249.0 → 204.0 / 249.0 → 213.0 | 186.0 → 144.0 / 187.8 → 144.9 | — → — |
| grid1m | 800×800 / 1 / 4 | 235.0 → 192.0 / 244.9 → 192.0 | 166.0 → 127.0 / 169.6 → 127.9 | 943.3 → 723.1 |
| grid1m | 800×800 / 1 / 10 | 223.0 → 185.0 / 247.3 → 185.0 | 155.0 → 119.0 / 156.8 → 194.6 | — → — |

Exact image/buffer comparisons: 85 / 85; maximum absolute difference 0. Differences include metadata, nonfinite patterns and changed-sample counts.

Fresh RSS was measured in separate one-render processes for selected settings. A dash means unmeasured, not zero. Whole diagnostic-process RSS includes retained outputs and profiling; it is not a fresh-render memory result. No plotting or file encoding is included.
