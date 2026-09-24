# composition sweep: `511577a3` → `5eb61c63`

Three warm public calls and three isolated native samples per setting, with GC outside their timers. One separate diagnostic sample per setting supplies phase timings/counters; its p95 is not a distribution. Variant order alternates between settings. Quality is identical within each pair. Cold time, R allocations and diagnostic-process RSS remain in the CSV summaries. These short samples do not establish stable tail latency.

| Scene | Output / FSAA / workers | Public median / p95 ms | Native median / p95 ms | Fresh RSS MiB |
|---|---|---:|---:|---:|
| grid100k | 1920×1080 / 2 / 4 | 1277.0 → 1277.0 / 1291.4 → 1283.3 | 102.0 → 104.0 / 106.5 → 113.0 | 1747.3 → 1704.4 |
| small | 1920×1080 / 2 / 4 | 1116.0 → 1083.0 / 1135.8 → 1190.1 | 59.0 → 57.0 / 60.8 → 57.9 | 1668.5 → 1617.6 |
| ssao | 1920×1080 / 2 / 4 | 1655.0 → 1539.0 / 1745.0 → 1547.1 | 413.0 → 412.0 / 413.9 → 417.4 | 2471.9 → 2308.4 |

Exact image/buffer comparisons: 51 / 51; maximum absolute difference 0. Differences include metadata, nonfinite patterns and changed-sample counts.

Fresh RSS was measured in separate one-render processes for selected settings. A dash means unmeasured, not zero. Whole diagnostic-process RSS includes retained outputs and profiling; it is not a fresh-render memory result. No plotting or file encoding is included.

| Scene | Assembly boundary ms (one diagnostic) | Full-call R allocations MiB |
|---|---:|---:|
| small | 291.0 → 240.0 | 2491.5 → 2215.0 |
| grid100k | 338.0 → 288.0 | 2489.2 → 2229.6 |
| ssao | 412.0 → 351.0 | 3238.4 → 2784.6 |

Normal reuse and tangent specialization are disabled in both builds. Resize/filter quality is unchanged. These assembly boundaries are individual diagnostic intervals, not independent timing distributions. The SSAO public benefit occurs with isolated native time nearly unchanged; the HD grid shows no public benefit in this run.
