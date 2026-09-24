# macrotiles sweep: `888c0d5-flat` → `888c0d5-spatial64`

Three warm public calls and three isolated native samples per setting, with GC outside their timers. One separate diagnostic sample per setting supplies phase timings/counters; its p95 is not a distribution. Variant order alternates between settings. Quality is identical within each pair. Cold time, R allocations and diagnostic-process RSS remain in the CSV summaries. These short samples do not establish stable tail latency.

| Scene | Output / FSAA / workers | Public median / p95 ms | Native median / p95 ms | Fresh RSS MiB |
|---|---|---:|---:|---:|
| alpha16 | 800×800 / 1 / 4 | 111.0 → 112.0 / 114.6 → 112.9 | 97.0 → 95.0 / 97.0 → 95.0 | 370.7 → 369.4 |
| alpha16 | 800×800 / 1 / 10 | 71.0 → 75.0 / 71.9 → 75.9 | 54.0 → 59.0 / 54.9 → 66.2 | — → — |
| grid1m | 800×800 / 1 / 4 | 230.0 → 224.0 / 246.2 → 242.9 | 165.0 → 156.0 / 167.7 → 156.0 | — → — |
| grid1m | 800×800 / 1 / 10 | 220.0 → 212.0 / 245.2 → 229.1 | 154.0 → 149.0 / 155.8 → 149.9 | — → — |
| overdraw | 800×800 / 1 / 4 | 135.0 → 134.0 / 135.0 → 134.0 | 109.0 → 107.0 / 109.0 → 107.9 | — → — |
| overdraw | 800×800 / 1 / 10 | 88.0 → 89.0 / 88.0 → 92.6 | 67.0 → 63.0 / 76.9 → 65.7 | — → — |
| shadow | 800×800 / 1 / 4 | 28.0 → 27.0 / 28.0 → 27.9 | 9.0 → 9.0 / 9.9 → 9.9 | — → — |
| small | 800×800 / 1 / 4 | 21.0 → 21.0 / 21.9 → 21.0 | 5.0 → 5.0 / 5.9 → 5.0 | — → — |
| ssao | 800×800 / 1 / 4 | 77.0 → 77.0 / 78.8 → 77.9 | 35.0 → 34.0 / 35.9 → 34.0 | — → — |

Exact image/buffer comparisons: 153 / 153; maximum absolute difference 0. Differences include metadata, nonfinite patterns and changed-sample counts.

Fresh RSS was measured separately for selected high-sample-count and deep-layer cases only. A dash means unmeasured, not zero. Whole diagnostic-process RSS includes retained outputs and profiling; it is not a fresh-render memory result. No plotting or file encoding is included.


Both variants use `888c0d5`; after sets `RAYVERTEX_MACROTILE_EDGE=64`. Every task owns a spatial group of existing coverage blocks. The nominal edge rounds up to whole microblocks; public block size, coverage boundaries, sample layout, and each pixel's primitive sequence are unchanged. Empty macrotiles are not submitted. The scalar and flat-batch paths remain available. Scheduling scratch bytes, active macrotiles and task counts are recorded in phase counters; task-list construction is included in the coverage/shading wall stage.

This remains opt-in. Across the 16/32/64 sweeps, million-triangle native time improves modestly, while application results vary and some alpha16 settings regress. There is no uniform best edge and no default replacement for batches of 64. False sharing/hardware counters were not measured; no cache-causality claim is made from elapsed times alone.
