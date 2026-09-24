# bins sweep: `08a3fd1-off` → `08a3fd1-on`

Three warm public calls and three isolated native samples per setting, with GC outside their timers. One separate diagnostic sample per setting supplies phase timings/counters; its p95 is not a distribution. Variant order alternates between settings. Quality is identical within each pair. Cold time, R allocations and diagnostic-process RSS remain in the CSV summaries. These short samples do not establish stable tail latency.

| Scene | Output / FSAA / workers | Public median / p95 ms | Native median / p95 ms | Fresh RSS MiB |
|---|---|---:|---:|---:|
| grid100k | 800×800 / 1 / 4 | 48.0 → 49.0 / 50.7 → 64.3 | 23.0 → 32.0 / 23.9 → 59.0 | — → — |
| grid100k | 1920×1080 / 2 / 4 | 1337.0 → 1236.0 / 1387.4 → 1319.7 | 107.0 → 104.0 / 109.7 → 104.9 | 1774.5 → 1775.6 |
| grid1m | 800×800 / 1 / 1 | 295.0 → 294.0 / 316.6 → 296.7 | 230.0 → 229.0 / 230.9 → 248.8 | — → — |
| grid1m | 800×800 / 1 / 2 | 271.0 → 247.0 / 280.9 → 276.7 | 191.0 → 188.0 / 191.0 → 192.5 | — → — |
| grid1m | 800×800 / 1 / 4 | 233.0 → 234.0 / 259.1 → 238.5 | 171.0 → 165.0 / 190.8 → 170.4 | 948.0 → 919.8 |
| grid1m | 800×800 / 1 / 10 | 251.0 → 226.0 / 253.7 → 232.3 | 163.0 → 164.0 / 164.8 → 164.0 | — → — |
| grid500k | 800×800 / 1 / 4 | 133.0 → 138.0 / 153.7 → 152.4 | 94.0 → 89.0 / 100.3 → 90.8 | — → — |
| overdraw | 800×800 / 1 / 4 | 142.0 → 136.0 / 152.8 → 137.8 | 121.0 → 110.0 / 121.9 → 110.0 | — → — |
| small | 800×800 / 1 / 4 | 21.0 → 21.0 / 21.9 → 21.9 | 6.0 → 5.0 / 6.0 → 5.9 | — → — |

Exact image/buffer comparisons: 153 / 153; maximum absolute difference 0. Differences include metadata, nonfinite patterns and changed-sample counts.

Fresh RSS was measured in separate one-render processes for selected settings. A dash means unmeasured, not zero. Whole diagnostic-process RSS includes retained outputs and profiling; it is not a fresh-render memory result. No plotting or file encoding is included.

## Bin-stage diagnostic

Both variants use `08a3fd1`; after sets `RAYVERTEX_PARALLEL_BINS=1`. This remains experimental and off by default. Stable contiguous primitive chunks count references independently, then a checked serial prefix reserves each tile/chunk segment before parallel fill. The histogram is bounded to 32 MiB, with a serial fallback for fewer than two eligible chunks. A chunk has at least 16,384 primitives. No worker calls R or changes primitive order.

| Setting | Bin build ms, OFF → ON | Histogram/cursor bytes, OFF → ON | Actual bin workers, ON |
|---|---:|---:|---:|
| grid100k-1920-1080-2-4 | 4.093 → 2.561 | 4147208 → 16588800 | 4 |
| grid100k-800-800-1-4 | 2.129 → 0.984 | 320008 → 1280000 | 4 |
| grid1m-800-800-1-1 | 18.542 → 9.998 | 320008 → 320008 | 1 |
| grid1m-800-800-1-10 | 10.334 → 12.860 | 320008 → 3200000 | 10 |
| grid1m-800-800-1-2 | 10.115 → 7.822 | 320008 → 640000 | 2 |
| grid1m-800-800-1-4 | 9.930 → 7.511 | 320008 → 1280000 | 4 |
| grid500k-800-800-1-4 | 6.238 → 3.397 | 320008 → 1280000 | 4 |
| overdraw-800-800-1-4 | 3.541 → 1.809 | 320008 → 1280000 | 4 |
| small-800-800-1-4 | 0.068 → 0.068 | 320008 → 320008 | 1 |

These stage clocks are one diagnostic sample per setting, not a timing distribution. In the million-triangle fixture, two/four bin workers reduce the observed bin stage, while ten workers increase it (10.33→12.86 ms). Four-worker isolated native median improves 171→165 ms but public median changes 233→234 ms; ten-worker native changes 163→164 ms despite a lower public median. Single-worker settings take the same serial path and their differences show run variation. The evidence does not support a universal default change.

All 153 image/buffer comparisons and the 245 scalar corpus results are exact. The focused package regression passes twelve assertions for main/shadow order, worker limits, memory bounds and prepared parity. The standalone randomized clipped-fan test compares all offsets and primitive identities with serial construction at 1/2/4/10 workers and constrained histogram budgets, and passes ASan/UBSan and TSan. Raw logs and the test code are retained.
