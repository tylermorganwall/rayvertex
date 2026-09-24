# macrotiles sweep: `888c0d5-flat` → `888c0d5-spatial32`

Three warm public calls and three isolated native samples per setting, with GC outside their timers. One separate diagnostic sample per setting supplies phase timings/counters; its p95 is not a distribution. Variant order alternates between settings. Quality is identical within each pair. Cold time, R allocations and diagnostic-process RSS remain in the CSV summaries. These short samples do not establish stable tail latency.

| Scene | Output / FSAA / workers | Public median / p95 ms | Native median / p95 ms | Fresh RSS MiB |
|---|---|---:|---:|---:|
| alpha16 | 800×800 / 1 / 4 | 109.0 → 112.0 / 111.7 → 112.9 | 94.0 → 95.0 / 95.8 → 95.9 | 374.5 → 368.4 |
| alpha16 | 800×800 / 1 / 10 | 70.0 → 71.0 / 70.0 → 76.4 | 56.0 → 53.0 / 61.4 → 57.5 | — → — |
| grid1m | 800×800 / 1 / 4 | 234.0 → 229.0 / 256.5 → 242.5 | 166.0 → 157.0 / 166.9 → 159.7 | — → — |
| grid1m | 800×800 / 1 / 10 | 220.0 → 226.0 / 243.4 → 253.9 | 159.0 → 153.0 / 159.0 → 182.7 | — → — |
| overdraw | 800×800 / 1 / 4 | 132.0 → 133.0 / 132.0 → 133.0 | 108.0 → 107.0 / 108.9 → 107.9 | — → — |
| overdraw | 800×800 / 1 / 10 | 90.0 → 86.0 / 90.9 → 86.0 | 64.0 → 61.0 / 64.0 → 61.9 | — → — |
| shadow | 800×800 / 1 / 4 | 26.0 → 27.0 / 28.7 → 27.9 | 9.0 → 10.0 / 9.9 → 10.0 | — → — |
| small | 800×800 / 1 / 4 | 20.0 → 20.0 / 20.9 → 20.9 | 5.0 → 5.0 / 5.0 → 5.0 | — → — |
| ssao | 800×800 / 1 / 4 | 77.0 → 77.0 / 78.8 → 78.8 | 34.0 → 34.0 / 34.9 → 34.0 | — → — |

Exact image/buffer comparisons: 153 / 153; maximum absolute difference 0. Differences include metadata, nonfinite patterns and changed-sample counts.

Fresh RSS was measured separately for selected high-sample-count and deep-layer cases only. A dash means unmeasured, not zero. Whole diagnostic-process RSS includes retained outputs and profiling; it is not a fresh-render memory result. No plotting or file encoding is included.


Both variants use `888c0d5`; after sets `RAYVERTEX_MACROTILE_EDGE=32`. Every task owns a spatial group of existing coverage blocks. The nominal edge rounds up to whole microblocks; public block size, coverage boundaries, sample layout, and each pixel's primitive sequence are unchanged. Empty macrotiles are not submitted. The scalar and flat-batch paths remain available. Scheduling scratch bytes, active macrotiles and task counts are recorded in phase counters; task-list construction is included in the coverage/shading wall stage.

This remains opt-in. Across the 16/32/64 sweeps, million-triangle native time improves modestly, while application results vary and some alpha16 settings regress. There is no uniform best edge and no default replacement for batches of 64. False sharing/hardware counters were not measured; no cache-causality claim is made from elapsed times alone.
