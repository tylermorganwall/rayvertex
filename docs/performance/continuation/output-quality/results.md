# quality sweep: `b755958` → `69ae102`

Three warm public calls and three isolated native samples per setting, with GC outside their timers. One separate diagnostic sample per setting supplies phase timings/counters; its p95 is not a distribution. Variant order alternates between settings. Quality is identical within each pair. Cold time, R allocations and diagnostic-process RSS remain in the CSV summaries. These short samples do not establish stable tail latency.

| Scene | Output / FSAA / workers | Public median / p95 ms | Native median / p95 ms | Fresh RSS MiB |
|---|---|---:|---:|---:|
| alpha129 | 159×241 / 1 / 4 | 68.0 → 60.0 / 70.7 → 61.8 | 58.0 → 62.0 / 60.7 → 62.0 | 238.4 → 230.5 |
| alpha4 | 800×800 / 2 / 4 | 808.0 → 429.0 / 813.4 → 444.3 | 114.0 → 114.0 / 116.7 → 114.9 | — → — |
| alpha4 | 1920×1080 / 1 / 4 | 547.0 → 158.0 / 597.4 → 179.6 | 61.0 → 59.0 / 63.7 → 59.9 | — → — |
| alpha4 | 1920×1080 / 2 / 4 | 3928.0 → 1731.0 / 3961.3 → 1763.4 | 338.0 → 365.0 / 369.5 → 373.1 | — → — |
| grid100k | 800×800 / 2 / 4 | 747.0 → 384.0 / 764.1 → 396.6 | 52.0 → 50.0 / 52.0 → 50.0 | — → — |
| grid100k | 1920×1080 / 1 / 4 | 528.0 → 145.0 / 541.5 → 162.1 | 43.0 → 42.0 / 44.8 → 42.0 | — → — |
| grid100k | 1920×1080 / 2 / 4 | 2223.0 → 1369.0 / 2369.7 → 1412.2 | 133.0 → 106.0 / 147.4 → 107.8 | 3155.6 → 1762.5 |
| small | 800×800 / 2 / 4 | 718.0 → 345.0 / 741.4 → 356.7 | 24.0 → 22.0 / 24.9 → 22.0 | — → — |
| small | 1920×1080 / 1 / 4 | 489.0 → 111.0 / 507.0 → 133.5 | 16.0 → 16.0 / 16.0 → 16.9 | — → — |
| small | 1920×1080 / 2 / 4 | 2097.0 → 1209.0 / 2099.7 → 1244.1 | 60.0 → 61.0 / 62.7 → 62.8 | — → — |
| ssao | 1920×1080 / 2 / 4 | 3055.0 → 2070.0 / 3287.2 → 2178.9 | 403.0 → 549.0 / 405.7 → 571.5 | — → — |

Exact image/buffer comparisons: 187 / 187; maximum absolute difference 0. Differences include metadata, nonfinite patterns and changed-sample counts.

Fresh RSS was measured separately for selected high-sample-count and deep-layer cases only. A dash means unmeasured, not zero. Whole diagnostic-process RSS includes retained outputs and profiling; it is not a fresh-render memory result. No plotting or file encoding is included.


All 187 image/buffer comparisons are exact. At 1920×1080/FSAA 2/four workers, this phase changes output traversal and allocation, while retaining Mitchell filtering and the native rasterizer. R output fusion materially reduces public time and fresh RSS; native differences between libraries are retained without attributing them to faster coverage/shading. In particular, SSAO native median changes 403→549 ms despite unchanged raster source; its cause requires separate repeats. Alpha129 remains an unlimited-layer stress control. See the ordinary API phase table and helper-stage measurements in `../output/` for attribution.

[Three additional process rounds](repeats/README.md) retain smaller native SSAO regressions (410→423, 389→442, 391→397 ms) and reproduce the public improvement in every round. Their simpler allocation history differs from the main sweep. The source-level attribution remains to output work; a claim of unchanged native performance would be too strong.
