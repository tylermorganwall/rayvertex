# Measured phase: 175c43a → e512c02

800×800; FSAA 1; one worker; unchanged fixture/lighting/quality. Three warm samples; three separate uninstrumented native samples; three diagnostic profiles. Cold is the first render in a fresh R process including preparation/decode, with OS file caches uncontrolled. Fresh-render RSS is a separate one-render process. Sustained mean covers five ordinary calls plus final release/GC; warm/native samples force GC outside each timer.

| Scene | Cold ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| alpha16 | 1110.0 → 887.0 | 1001.0 → 771.0 / 1018.1 → 789.0 | 868.0 → 633.0 / 872.5 → 633.9 | 785.4 → 789.0 | 757.0 → 759.3 |
| alpha4 | 521.0 → 460.0 | 387.0 → 320.0 / 403.2 → 338.9 | 241.0 → 183.0 / 241.9 → 183.9 | 334.0 → 338.2 | 436.1 → 431.2 |
| alpha64 | 3119.0 → 2360.0 | 3018.0 → 2265.0 / 3039.6 → 2265.9 | 2836.0 → 2099.0 / 2845.0 → 2126.9 | 2257.0 → 2255.6 | 2043.2 → 2040.8 |
| grid1m | 603.0 → 633.0 | 510.0 → 507.0 / 521.7 → 516.0 | 341.0 → 346.0 / 372.5 → 355.9 | 530.0 → 549.2 | 1275.7 → 1268.0 |
| near_crossing | 322.0 → 317.0 | 216.0 → 239.0 / 224.1 → 240.8 | 56.0 → 57.0 / 56.0 → 57.9 | 244.4 → 241.6 | 451.5 → 473.4 |
| orthographic | 332.0 → 329.0 | 204.0 → 198.0 / 210.3 → 214.2 | 48.0 → 49.0 / 48.9 → 49.0 | 222.6 → 218.6 | 458.5 → 481.3 |
| shadow | 348.0 → 349.0 | 239.0 → 225.0 / 248.0 → 232.2 | 78.0 → 69.0 / 81.6 → 69.9 | 237.2 → 237.2 | 473.8 → 447.0 |
| small | 289.0 → 275.0 | 180.0 → 183.0 / 195.3 → 194.7 | 31.0 → 26.0 / 31.9 → 27.8 | 180.4 → 181.2 | 490.9 → 477.5 |

Image/buffer comparisons: 136; exact: 107. Maximum absolute difference: 5.91430297521. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.

This is a correctness phase, not an equivalent-behavior speedup claim. At 800×800 the intended near-crossing changes remain visible; orthographic depth changes also alter a few equal-depth seam winners (maximum UV change 0.02514) and produce RGB differences at roundoff scale (maximum 3.33e-16). Other measured scenes remain exact. The earlier 47×33 corpus only exposed orthographic depth differences, so both resolutions are retained.

The 1M sustained mean rises 530.0→549.2 ms, an observed cost of clipping checks. Large forced-GC alpha timing differences do not reproduce in sustained loops: alpha64 is 2257.0→2255.6 ms. They are not attributed to a clipping speedup. Raw measurements and RSS regressions are retained.
