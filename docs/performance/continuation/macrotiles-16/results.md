# macrotiles sweep: `888c0d5-flat` → `888c0d5-spatial16`

Three warm public calls and three isolated native samples per setting, with GC outside their timers. One separate diagnostic sample per setting supplies phase timings/counters; its p95 is not a distribution. Variant order alternates between settings. Quality is identical within each pair. Cold time, R allocations and diagnostic-process RSS remain in the CSV summaries. These short samples do not establish stable tail latency.

| Scene | Output / FSAA / workers | Public median / p95 ms | Native median / p95 ms | Fresh RSS MiB |
|---|---|---:|---:|---:|
| alpha16 | 800×800 / 1 / 4 | 108.0 → 111.0 / 110.7 → 111.9 | 95.0 → 96.0 / 96.8 → 98.7 | 371.8 → 364.5 |
| alpha16 | 800×800 / 1 / 10 | 69.0 → 72.0 / 70.8 → 72.9 | 56.0 → 58.0 / 56.9 → 60.7 | — → — |
| grid1m | 800×800 / 1 / 4 | 228.0 → 212.0 / 243.3 → 233.6 | 166.0 → 159.0 / 178.6 → 159.9 | — → — |
| grid1m | 800×800 / 1 / 10 | 218.0 → 223.0 / 235.1 → 224.8 | 153.0 → 146.0 / 158.4 → 146.9 | — → — |
| overdraw | 800×800 / 1 / 4 | 131.0 → 131.0 / 132.8 → 134.6 | 110.0 → 107.0 / 110.0 → 108.8 | — → — |
| overdraw | 800×800 / 1 / 10 | 88.0 → 88.0 / 90.7 → 95.2 | 64.0 → 61.0 / 64.9 → 61.9 | — → — |
| shadow | 800×800 / 1 / 4 | 28.0 → 27.0 / 28.9 → 28.8 | 9.0 → 10.0 / 9.0 → 10.0 | — → — |
| small | 800×800 / 1 / 4 | 21.0 → 21.0 / 21.9 → 21.9 | 5.0 → 5.0 / 5.0 → 5.0 | — → — |
| ssao | 800×800 / 1 / 4 | 78.0 → 77.0 / 78.9 → 79.7 | 34.0 → 34.0 / 34.9 → 34.9 | — → — |

Exact image/buffer comparisons: 153 / 153; maximum absolute difference 0. Differences include metadata, nonfinite patterns and changed-sample counts.

Fresh RSS was measured in separate one-render processes for selected settings. A dash means unmeasured, not zero. Whole diagnostic-process RSS includes retained outputs and profiling; it is not a fresh-render memory result. No plotting or file encoding is included.


Both variants use `888c0d5`; after sets `RAYVERTEX_MACROTILE_EDGE=16`. Every task owns a spatial group of existing coverage blocks. The nominal edge rounds up to whole microblocks; public block size, coverage boundaries, sample layout, and each pixel's primitive sequence are unchanged. Empty macrotiles are not submitted. The scalar and flat-batch paths remain available. Scheduling scratch bytes, active macrotiles and task counts are recorded in phase counters; task-list construction is included in the coverage/shading wall stage.

This remains opt-in. Across the 16/32/64 sweeps, million-triangle native time improves modestly, while application results vary and some alpha16 settings regress. There is no uniform best edge and no default replacement for batches of 64. False sharing/hardware counters were not measured; no cache-causality claim is made from elapsed times alone.
