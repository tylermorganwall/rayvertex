# workers sweep: `e512c02` → `d48a259`

Three warm public calls and three isolated native samples per setting, with GC outside their timers. One separate diagnostic sample per setting supplies phase timings/counters; its p95 is not a distribution. Variant order alternates between settings. Quality is identical within each pair. Cold time, R allocations and diagnostic-process RSS remain in the CSV summaries. These short samples do not establish stable tail latency.

| Scene | Output / FSAA / workers | Public median / p95 ms | Native median / p95 ms | Fresh RSS MiB |
|---|---|---:|---:|---:|
| alpha16 | 800×800 / 1 / 1 | 783.0 → 511.0 / 794.7 → 511.9 | 635.0 → 326.0 / 636.8 → 326.9 | — → — |
| alpha16 | 800×800 / 1 / 2 | 672.0 → 357.0 / 681.0 → 359.7 | 525.0 → 172.0 / 533.1 → 174.7 | — → — |
| alpha16 | 800×800 / 1 / 4 | 615.0 → 279.0 / 635.7 → 288.0 | 477.0 → 97.0 / 477.9 → 98.8 | — → — |
| alpha16 | 800×800 / 1 / 10 | 591.0 → 239.0 / 614.4 → 239.9 | 453.0 → 54.0 / 453.9 → 57.6 | — → — |
| overdraw | 800×800 / 1 / 1 | 657.0 → 534.0 / 657.9 → 548.4 | 490.0 → 368.0 / 493.6 → 372.5 | — → — |
| overdraw | 800×800 / 1 / 2 | 437.0 → 366.0 / 471.2 → 386.7 | 267.0 → 195.0 / 272.4 → 195.9 | — → — |
| overdraw | 800×800 / 1 / 4 | 326.0 → 280.0 / 336.8 → 298.9 | 157.0 → 113.0 / 160.6 → 115.7 | — → — |
| overdraw | 800×800 / 1 / 10 | 268.0 → 250.0 / 270.7 → 256.3 | 102.0 → 68.0 / 104.7 → 77.0 | — → — |
| shadow | 800×800 / 1 / 1 | 235.0 → 207.0 / 235.0 → 226.8 | 67.0 → 20.0 / 68.8 → 23.6 | — → — |
| shadow | 800×800 / 1 / 2 | 206.0 → 198.0 / 219.5 → 221.4 | 47.0 → 14.0 / 47.9 → 14.0 | — → — |
| shadow | 800×800 / 1 / 4 | 198.0 → 195.0 / 199.8 → 222.0 | 33.0 → 10.0 / 35.7 → 10.0 | — → — |
| shadow | 800×800 / 1 / 10 | 196.0 → 194.0 / 205.0 → 225.5 | 31.0 → 8.0 / 32.8 → 9.8 | — → — |
| ssao | 800×800 / 1 / 1 | 702.0 → 275.0 / 713.7 → 278.6 | 536.0 → 108.0 / 538.7 → 108.9 | — → — |
| ssao | 800×800 / 1 / 2 | 331.0 → 224.0 / 332.8 → 224.0 | 176.0 → 60.0 / 179.6 → 60.0 | — → — |
| ssao | 800×800 / 1 / 4 | 261.0 → 204.0 / 264.6 → 206.7 | 97.0 → 34.0 / 102.4 → 34.9 | — → — |
| ssao | 800×800 / 1 / 10 | 223.0 → 189.0 / 224.8 → 198.0 | 57.0 → 21.0 / 61.5 → 28.2 | — → — |

Exact image/buffer comparisons: 272 / 272; maximum absolute difference 0. Differences include metadata, nonfinite patterns and changed-sample counts.

Fresh RSS was measured separately for selected high-sample-count and deep-layer cases only. A dash means unmeasured, not zero. Whole diagnostic-process RSS includes retained outputs and profiling; it is not a fresh-render memory result. No plotting or file encoding is included.


Layered transparency scales much better after disjoint tile resolve and compact records: final alpha16 native medians are 326/172/97/54 ms for 1/2/4/10 workers, compared with 635/525/477/453 ms at the clipped checkpoint. Public times retain R preparation/output overhead. The shadow case reaches 8 ms native at ten workers but 194 ms publicly, so native scaling does not translate into the same application-level multiplier. The machine has eight performance and two efficiency cores; ten workers is the measured machine-specific endpoint, not a portable recommendation.

Visibility is disabled throughout this comparison. Its explicit on/off sweep is separate. The current default batch remains 64 existing coverage blocks; no public core limit, block size, FSAA, sample count or map dimension is changed.
