# quality sweep: `e512c02` → `d48a259`

Three warm public calls and three isolated native samples per setting, with GC outside their timers. One separate diagnostic sample per setting supplies phase timings/counters; its p95 is not a distribution. Variant order alternates between settings. Quality is identical within each pair. Cold time, R allocations and diagnostic-process RSS remain in the CSV summaries. These short samples do not establish stable tail latency.

| Scene | Output / FSAA / workers | Public median / p95 ms | Native median / p95 ms | Fresh RSS MiB |
|---|---|---:|---:|---:|
| alpha129 | 159×241 / 1 / 4 | 290.0 → 66.0 / 298.1 → 66.0 | 292.0 → 57.0 / 293.8 → 57.9 | 388.7 → 248.8 |
| alpha4 | 800×800 / 2 / 4 | 1197.0 → 753.0 / 1206.9 → 762.0 | 548.0 → 118.0 / 550.7 → 118.9 | — → — |
| alpha4 | 1920×1080 / 1 / 4 | 732.0 → 521.0 / 750.0 → 546.2 | 277.0 → 56.0 / 277.0 → 58.7 | — → — |
| alpha4 | 1920×1080 / 2 / 4 | 3125.0 → 2197.0 / 3174.5 → 2206.0 | 1123.0 → 232.0 / 1133.8 → 234.7 | — → — |
| grid100k | 800×800 / 2 / 4 | 750.0 → 697.0 / 775.2 → 709.6 | 135.0 → 47.0 / 158.4 → 47.9 | — → — |
| grid100k | 1920×1080 / 1 / 4 | 549.0 → 486.0 / 564.3 → 510.3 | 107.0 → 38.0 / 107.9 → 38.0 | — → — |
| grid100k | 1920×1080 / 2 / 4 | 2305.0 → 2065.0 / 2323.9 → 2101.0 | 348.0 → 95.0 / 377.7 → 97.7 | 4363.9 → 3149.6 |
| small | 800×800 / 2 / 4 | 717.0 → 655.0 / 730.5 → 676.6 | 91.0 → 21.0 / 91.9 → 21.0 | — → — |
| small | 1920×1080 / 1 / 4 | 536.0 → 467.0 / 545.9 → 482.3 | 70.0 → 15.0 / 73.6 → 15.0 | — → — |
| small | 1920×1080 / 2 / 4 | 2229.0 → 2022.0 / 2255.1 → 2026.5 | 297.0 → 60.0 / 305.1 → 61.8 | — → — |
| ssao | 1920×1080 / 2 / 4 | 3011.0 → 2406.0 / 3019.1 → 2422.2 | 1061.0 → 402.0 / 1073.6 → 402.0 | — → — |

Exact image/buffer comparisons: 187 / 187; maximum absolute difference 0. Differences include metadata, nonfinite patterns and changed-sample counts.

Fresh RSS was measured separately for selected high-sample-count and deep-layer cases only. A dash means unmeasured, not zero. Whole diagnostic-process RSS includes retained outputs and profiling; it is not a fresh-render memory result. No plotting or file encoding is included.


The portrait stress case records 2,130,156 transparent fragments and a maximum of 129 layers at one sample, with no layer cap. At 1920×1080/FSAA 2, the 100k mesh's fresh RSS falls 4363.9→3149.6 MiB. Its native median falls 348→95 ms, but public median only 2305→2065 ms: R-side assembly/color/resize/GC dominates at this quality setting. This is evidence to retain separate native and application timings, not to lower FSAA or filtering. SSAO retains 64 samples, and all comparisons are exact.
