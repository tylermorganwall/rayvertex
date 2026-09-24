# Combined-default regression repeats

`05cec195` → `5bdfaaa5`, with normal/tangent experiments off. Three separate processes per variant/case, five warm and five isolated native samples per process. 800²/FSAA 1/one worker; alternating order; GC outside timers. No compilation, tests or competing measured renders during collection.

| Case | Round | Warm median / p95 ms (before → after) | Native median / p95 ms (before → after) |
|---|---:|---:|---:|
| grid1m | 1 | 237.0 → 239.0 / 238.0 → 239.8 | 177.0 → 179.0 / 181.2 → 179.8 |
| grid1m | 2 | 240.0 → 233.0 / 258.6 → 235.6 | 180.0 → 178.0 / 180.8 → 178.0 |
| grid1m | 3 | 237.0 → 235.0 / 241.4 → 241.0 | 174.0 → 178.0 / 175.8 → 183.8 |
| normal_shared | 1 | 258.0 → 253.0 / 260.8 → 256.2 | 198.0 → 201.0 / 205.0 → 204.2 |
| normal_shared | 2 | 265.0 → 253.0 / 266.0 → 256.8 | 198.0 → 196.0 / 198.8 → 199.6 |
| normal_shared | 3 | 261.0 → 256.0 / 271.8 → 260.0 | 201.0 → 203.0 / 208.6 → 212.2 |
| small | 1 | 21.0 → 17.0 / 40.8 → 35.8 | 7.0 → 8.0 / 7.8 → 8.0 |
| small | 2 | 22.0 → 17.0 / 41.8 → 36.8 | 7.0 → 7.0 / 7.0 → 7.8 |
| small | 3 | 23.0 → 17.0 / 43.8 → 37.8 | 8.0 → 8.0 / 8.8 → 8.0 |

The earlier 24 ms grid public regression and 11 ms native regression do not recur at that magnitude. Warm medians change +2/−7/−2 ms and native medians +2/−2/+4 ms across these independent rounds. The smaller native changes remain mixed; no cause is established for the original outlier. The original phase samples and p95 are retained. Shared-normal and small-scene public medians improve in all three rounds, without a corresponding consistent native gain.
