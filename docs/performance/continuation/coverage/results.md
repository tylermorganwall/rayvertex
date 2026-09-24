# coverage sweep: `dbaf8b1` → `dbaf8b1`

Three warm public calls and three isolated native samples per setting, with GC outside their timers. One separate diagnostic sample per setting supplies phase timings/counters; its p95 is not a distribution. Variant order alternates between settings. Quality is identical within each pair. Cold time, R allocations and diagnostic-process RSS remain in the CSV summaries. These short samples do not establish stable tail latency.

| Scene | Output / FSAA / workers | Public median / p95 ms | Native median / p95 ms | Fresh RSS MiB |
|---|---|---:|---:|---:|
| alpha16 | 800×800 / 1 / 1 | 356.0 → 339.0 / 356.9 → 339.0 | 339.0 → 322.0 / 343.5 → 329.2 | — → — |
| alpha16 | 800×800 / 1 / 4 | 112.0 → 109.0 / 112.9 → 113.5 | 96.0 → 94.0 / 96.9 → 94.9 | — → — |
| alpha4 | 800×800 / 1 / 1 | 100.0 → 104.0 / 105.4 → 104.0 | 86.0 → 87.0 / 86.0 → 87.0 | — → — |
| alpha4 | 800×800 / 1 / 4 | 42.0 → 44.0 / 44.7 → 44.9 | 28.0 → 27.0 / 28.0 → 27.9 | — → — |
| grid100k | 800×800 / 1 / 1 | 57.0 → 59.0 / 57.9 → 62.6 | 37.0 → 38.0 / 44.2 → 38.0 | — → — |
| grid100k | 800×800 / 1 / 4 | 45.0 → 47.0 / 47.7 → 47.0 | 23.0 → 22.0 / 23.9 → 23.8 | — → — |
| grid100k | 1920×1080 / 2 / 4 | 1200.0 → 1196.0 / 1205.4 → 1222.1 | 95.0 → 101.0 / 98.6 → 105.5 | 1733.7 → 1772.8 |
| grid1m | 800×800 / 1 / 1 | 280.0 → 279.0 / 299.8 → 282.6 | 220.0 → 218.0 / 247.9 → 219.8 | — → — |
| grid1m | 800×800 / 1 / 4 | 231.0 → 224.0 / 243.6 → 234.8 | 164.0 → 165.0 / 165.8 → 165.9 | — → — |
| shadow | 800×800 / 1 / 1 | 37.0 → 37.0 / 38.8 → 37.9 | 20.0 → 19.0 / 20.0 → 19.0 | — → — |
| shadow | 800×800 / 1 / 4 | 26.0 → 27.0 / 27.8 → 27.0 | 10.0 → 9.0 / 10.0 → 9.9 | — → — |
| slivers | 800×800 / 1 / 1 | 66.0 → 48.0 / 67.8 → 48.0 | 51.0 → 33.0 / 51.0 → 33.0 | — → — |
| slivers | 800×800 / 1 / 4 | 34.0 → 30.0 / 34.9 → 39.0 | 18.0 → 15.0 / 18.0 → 15.0 | 205.3 → 213.5 |
| small | 800×800 / 1 / 1 | 22.0 → 21.0 / 23.8 → 22.8 | 8.0 → 8.0 / 11.6 → 8.0 | — → — |
| small | 800×800 / 1 / 4 | 21.0 → 20.0 / 21.0 → 21.8 | 5.0 → 5.0 / 5.0 → 5.0 | — → — |

Exact image/buffer comparisons: 255 / 255; maximum absolute difference 0. Differences include metadata, nonfinite patterns and changed-sample counts.

Fresh RSS was measured in separate one-render processes for selected settings. A dash means unmeasured, not zero. Whole diagnostic-process RSS includes retained outputs and profiling; it is not a fresh-render memory result. No plotting or file encoding is included.
