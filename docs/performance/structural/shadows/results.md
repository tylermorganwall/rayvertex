# Measured phase: f44b9b6 → 5f0853f

800×800; FSAA 1; one worker; unchanged fixture/lighting/quality. Three warm samples; three separate uninstrumented native samples; three diagnostic profiles. Cold is the first render in a fresh R process including preparation/decode, with OS file caches uncontrolled. Fresh-render RSS is a separate one-render process. Sustained mean covers five ordinary calls plus final release/GC; warm/native samples force GC outside each timer.

| Scene | Cold ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| alpha16 | 572.0 → 584.0 | 516.0 → 531.0 / 517.8 → 531.0 | 342.0 → 348.0 / 351.0 → 348.9 | 516.8 → 530.2 | 383.4 → 398.2 |
| overdraw | 637.0 → 638.0 | 550.0 → 549.0 / 568.0 → 570.6 | 377.0 → 383.0 / 377.9 → 388.4 | 553.2 → 560.6 | 455.4 → 412.7 |
| shadow | 289.0 → 265.0 | 233.0 → 207.0 / 246.5 → 216.9 | 44.0 → 20.0 / 49.4 → 20.0 | 217.8 → 189.6 | 395.5 → 348.9 |
| shared_textures | 271.0 → 270.0 | 204.0 → 206.0 / 209.4 → 206.9 | 21.0 → 22.0 / 21.9 → 23.8 | 201.4 → 197.0 | 369.0 → 393.6 |
| small | 253.0 → 258.0 | 200.0 → 187.0 / 204.5 → 192.4 | 8.0 → 8.0 / 8.0 → 8.0 | 183.0 → 181.2 | 363.2 → 374.6 |

Image/buffer comparisons: 85; exact: 85. Maximum absolute difference: 0. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.


The opaque sphere shadow case removes empty transparent-filter reads and uses scalar depth taps: main coverage/shading falls 39.43→15.04 ms, while the shadow raster itself falls 0.792→0.667 ms. The 5×5 tap pattern, per-light bias, strict comparison and border clamping are unchanged. A per-light flag is published after the shadow resolve barrier; any stored transparent fragment conservatively prevents the skip, even if later occluded. Direction vectors are transformed once without changing normalization conventions. Opaque depth shading retains the original barycentric z arithmetic; alpha-dependent casters keep the full shader.

All 245 scalar-reference outputs and source tests pass. The standalone ASan/UBSan sampler checks 523,260 exact comparisons across all dimensions 1×1 through 17×19, edges, infinity, thresholds, and shadow intensities. The alpha16 sustained mean regresses 516.8→530.2 ms and native median 342→348 ms despite no shadow pass. This is retained for investigation in the final repeat runs; no universal shader speedup is claimed. Shared-texture fresh RSS increases despite no new full-frame buffer.
