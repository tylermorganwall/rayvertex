# Historical candidate defaults — measured phase: 05cec195 → 5bdfaaa5

800×800; FSAA 1; one worker; unchanged fixture/lighting/quality. Three warm samples; three separate uninstrumented native samples; three diagnostic profiles. Cold is the first render in a fresh R process including preparation/decode, with OS file caches uncontrolled. Fresh-render RSS is a separate one-render process. Sustained mean covers five ordinary calls plus final release/GC; warm/native samples force GC outside each timer.

| Scene | Cold ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| alpha16 | 408.0 → 415.0 | 331.0 → 329.0 / 333.7 → 329.9 | 312.0 → 319.0 / 312.0 → 320.8 | 360.0 → 355.2 | 370.9 → 372.9 |
| grid1m | 296.0 → 285.0 | 245.0 → 269.0 / 270.2 → 269.0 | 180.0 → 191.0 / 181.8 → 267.5 | 264.2 → 255.8 | 710.0 → 711.0 |
| normal_shared | 331.0 → 334.0 | 263.0 → 252.0 / 265.7 → 258.3 | 203.0 → 200.0 / 203.9 → 202.7 | 276.0 → 273.6 | 682.6 → 671.9 |
| shadow | 113.0 → 110.0 | 37.0 → 34.0 / 37.9 → 34.0 | 20.0 → 20.0 / 20.0 → 20.0 | 62.2 → 55.6 | 205.7 → 183.7 |
| small | 110.0 → 99.0 | 24.0 → 17.0 / 24.0 → 17.9 | 8.0 → 7.0 / 8.0 → 7.9 | 47.4 → 40.0 | 204.8 → 191.1 |
| ssao | 227.0 → 223.0 | 159.0 → 147.0 / 161.7 → 148.8 | 108.0 → 110.0 / 108.0 → 110.0 | 158.6 → 156.6 | 249.5 → 230.8 |
| tangent_overdraw | 680.0 → 668.0 | 608.0 → 594.0 / 614.3 → 594.0 | 584.0 → 579.0 / 599.3 → 580.8 | 626.2 → 618.4 | 200.4 → 201.0 |
| toon | 260.0 → 257.0 | 187.0 → 185.0 / 206.8 → 204.8 | 144.0 → 142.0 / 146.7 → 143.8 | 198.8 → 190.0 | 290.0 → 281.5 |

Image/buffer comparisons: 136; exact: 136. Maximum absolute difference: 0. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.

All optional normal/tangent/visibility/indexed/bins/macrotile/coverage switches are unset in this combined comparison. The large grid regression prompted [three independent rounds of five samples](repeats/results.md): public medians change +2/−7/−2 ms and native medians +2/−2/+4 ms. The large original loss does not recur, but its cause remains unresolved and these original samples are not removed. No general native speedup is claimed.

These results measure the now-retired tangent candidate build with its switch off. Accepted final defaults are measured separately in `../accepted-delivery/results.md`; these records are retained for audit, not substituted for final measurements.
