# Measured phase: 05cec195 → f1d7e122

800×800; FSAA 1; one worker; unchanged fixture/lighting/quality. Three warm samples; three separate uninstrumented native samples; three diagnostic profiles. Cold is the first render in a fresh R process including preparation/decode, with OS file caches uncontrolled. Fresh-render RSS is a separate one-render process. Sustained mean covers five ordinary calls plus final release/GC; warm/native samples force GC outside each timer.

| Scene | Cold ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| alpha16 | 404.0 → 412.0 | 329.0 → 327.0 / 329.0 → 334.2 | 318.0 → 318.0 / 322.5 → 328.8 | 348.8 → 352.0 | 371.8 → 378.4 |
| grid1m | 275.0 → 266.0 | 238.0 → 235.0 / 259.6 → 240.4 | 180.0 → 177.0 / 181.8 → 180.6 | 258.4 → 248.8 | 699.7 → 679.5 |
| normal_shared | 307.0 → 309.0 | 257.0 → 251.0 / 257.9 → 252.8 | 198.0 → 199.0 / 198.9 → 199.9 | 273.4 → 268.2 | 697.6 → 684.0 |
| shadow | 114.0 → 115.0 | 37.0 → 32.0 / 38.8 → 32.9 | 20.0 → 20.0 / 20.9 → 20.0 | 58.2 → 53.4 | 208.2 → 175.7 |
| small | 99.0 → 93.0 | 23.0 → 18.0 / 28.4 → 18.9 | 8.0 → 8.0 / 8.0 → 8.0 | 45.2 → 37.8 | 209.3 → 184.5 |
| ssao | 221.0 → 215.0 | 151.0 → 147.0 / 155.5 → 147.0 | 109.0 → 108.0 / 109.0 → 109.8 | 159.4 → 155.2 | 258.9 → 223.1 |
| tangent_overdraw | 672.0 → 662.0 | 599.0 → 589.0 / 600.8 → 594.4 | 580.0 → 573.0 / 584.5 → 578.4 | 620.0 → 614.4 | 199.3 → 200.6 |
| toon | 264.0 → 255.0 | 183.0 → 177.0 / 204.6 → 200.4 | 143.0 → 139.0 / 143.9 → 143.5 | 193.4 → 186.4 | 318.6 → 293.2 |

Image/buffer comparisons: 136; exact: 136. Maximum absolute difference: 0. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.

The accepted renderer `f1d7e122` restores the original tangent bodies and is source-equivalent to `5eb61c63` ignoring whitespace (`git diff -w 5eb61c63 f1d7e122 -- src R` is empty). The immutable `lib-composition` install therefore supplies the after measurements. All optional experiments are disabled. The retired tangent candidate's earlier default run remains separately recorded in `../delivery/`; it is not the final renderer.

The grid changes modestly (238 → 235 ms public, 180 → 177 ms native), while the small scene improves 23 → 18 ms public with native unchanged at 8 ms. Alpha16 sustained time regresses 348.8 → 352.0 ms and RSS rises 371.8 → 378.4 MiB. Shared-normal native time increases 198 → 199 ms. These short measurements do not establish a general native speedup.
