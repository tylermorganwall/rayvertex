# quality sweep: `b755958` → `eec25f7`

Three warm public calls and three isolated native samples per setting, with GC outside their timers. One separate diagnostic sample per setting supplies phase timings/counters; its p95 is not a distribution. Variant order alternates between settings. Quality is identical within each pair. Cold time, R allocations and diagnostic-process RSS remain in the CSV summaries. These short samples do not establish stable tail latency.

| Scene | Output / FSAA / workers | Public median / p95 ms | Native median / p95 ms | Fresh RSS MiB |
|---|---|---:|---:|---:|
| alpha129 | 159×241 / 1 / 4 | 63.0 → 56.0 / 63.9 → 56.0 | 57.0 → 57.0 / 57.9 → 61.5 | 248.2 → 231.4 |
| alpha4 | 800×800 / 2 / 4 | 749.0 → 422.0 / 751.7 → 429.2 | 113.0 → 107.0 / 130.1 → 108.8 | — → — |
| alpha4 | 1920×1080 / 1 / 4 | 514.0 → 146.0 / 531.1 → 163.1 | 56.0 → 55.0 / 56.9 → 55.0 | — → — |
| alpha4 | 1920×1080 / 2 / 4 | 2183.0 → 1290.0 / 2188.4 → 1312.5 | 231.0 → 222.0 / 249.9 → 224.7 | — → — |
| grid100k | 800×800 / 2 / 4 | 706.0 → 356.0 / 710.5 → 373.1 | 51.0 → 43.0 / 51.9 → 43.9 | — → — |
| grid100k | 1920×1080 / 1 / 4 | 505.0 → 130.0 / 518.5 → 149.8 | 39.0 → 34.0 / 40.8 → 35.8 | — → — |
| grid100k | 1920×1080 / 2 / 4 | 2080.0 → 1188.0 / 2084.5 → 1188.9 | 97.0 → 92.0 / 97.9 → 95.6 | 3112.9 → 1731.9 |
| small | 800×800 / 2 / 4 | 661.0 → 329.0 / 674.5 → 336.2 | 21.0 → 19.0 / 21.0 → 19.0 | — → — |
| small | 1920×1080 / 1 / 4 | 467.0 → 112.0 / 480.5 → 119.2 | 14.0 → 16.0 / 14.9 → 25.9 | — → — |
| small | 1920×1080 / 2 / 4 | 1996.0 → 1127.0 / 2065.3 → 1128.8 | 57.0 → 57.0 / 57.9 → 59.7 | — → — |
| ssao | 1920×1080 / 2 / 4 | 2418.0 → 1572.0 / 2426.1 → 1575.6 | 398.0 → 399.0 / 400.7 → 399.0 | — → — |

Exact image/buffer comparisons: 187 / 187; maximum absolute difference 0. Differences include metadata, nonfinite patterns and changed-sample counts.

Fresh RSS was measured in separate one-render processes for selected settings. A dash means unmeasured, not zero. Whole diagnostic-process RSS includes retained outputs and profiling; it is not a fresh-render memory result. No plotting or file encoding is included.
