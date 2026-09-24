# Rejected candidate — measured phase: 5eb61c63 → 5bdfaaa5

800×800; FSAA 1; one worker; unchanged fixture/lighting/quality. Three warm samples; three separate uninstrumented native samples; three diagnostic profiles. Cold is the first render in a fresh R process including preparation/decode, with OS file caches uncontrolled. Fresh-render RSS is a separate one-render process. Sustained mean covers five ordinary calls plus final release/GC; warm/native samples force GC outside each timer.

| Scene | Cold ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| alpha16 | 413.0 → 411.0 | 326.0 → 324.0 / 329.6 → 329.4 | 318.0 → 326.0 / 318.9 → 327.8 | 354.2 → 359.4 | 369.4 → 369.1 |
| normal_shared | 346.0 → 326.0 | 292.0 → 255.0 / 294.7 → 256.8 | 233.0 → 213.0 / 235.7 → 231.9 | 283.4 → 281.4 | 692.7 → 699.8 |
| shadow | 113.0 → 121.0 | 33.0 → 33.0 / 34.8 → 33.9 | 20.0 → 21.0 / 20.9 → 21.0 | 61.2 → 59.4 | 192.0 → 187.4 |
| small | 100.0 → 102.0 | 18.0 → 20.0 / 18.9 → 20.0 | 7.0 → 8.0 / 7.0 → 8.0 | 41.0 → 46.4 | 184.2 → 183.7 |
| tangent | 147.0 → 134.0 | 61.0 → 60.0 / 61.9 → 60.9 | 43.0 → 45.0 / 43.0 → 47.7 | 88.8 → 85.8 | 203.3 → 198.6 |
| tangent_overdraw | 698.0 → 680.0 | 608.0 → 592.0 / 609.8 → 600.1 | 581.0 → 580.0 / 587.3 → 590.8 | 630.8 → 620.8 | 208.2 → 201.6 |

Image/buffer comparisons: 102; exact: 102. Maximum absolute difference: 0. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.

`RAYVERTEX_TANGENT_ALGEBRA=1` is set for both processes; the predecessor has no such switch and uses the retained inverse-transpose expression. Normal caching is off. The candidate specializes the float cofactor/projection expressions while preserving double normalization, determinant arithmetic, mirrored UV behavior and zero-column products. It does not cancel the determinant or redefine tangent handedness. The measured candidate was disabled by default. It has since been removed from the renderer after the broader sanitizer comparison failed the equivalence gate.

The three-sample comparison shows no convincing isolated-native win on tangent workloads: 43 → 45 ms on the ordinary case and 581 → 580 ms with overdraw. The unrelated shared-normal improvement is not evidence of a tangent-kernel gain. The [same-binary off/on repeats](repeats/results.md) show overdraw native medians improving 3–8 ms in all three rounds, while warm medians regress 2–20 ms. Ordinary tangent results remain mixed; the experiment was not enabled and has since been removed.

Focused validation: 24 assertions; 245 scalar-reference outputs and 44 extended shader/debug outputs agree exactly with the switch off and on (the enabled corpus also uses normal reuse and prepared scenes). The standalone arithmetic test passes 1,000,012 comparisons under ASan/UBSan: finite values, infinities and signed zeros compare by bytes; NaNs compare by class.

| Tangent case | Main coverage/depth/shading diagnostic median ms |
|---|---:|
| tangent | 39.094 → 40.022 |
| tangent_overdraw | 606.443 → 572.195 |

## Rejection and retained evidence

The broad ASan O1 corpus found tangent-Phong normal differences versus the release reference above the existing 1e-9 comparison threshold. Comparing the candidate switch on/off within that same ASan build isolates differences to tangent normal debug buffers, with maximum absolute difference 3.7252902984619141e-9; ordinary shaded images, depth, coverage and nonfinite patterns remain unchanged. The scalar route matches the previous ASan build. The standalone arithmetic checks still pass at both O1 and O2, so those checks alone were insufficient to prove whole-renderer equivalence.

A trial with an explicit float projection temporary did not resolve the renderer mismatch. Its patch, logs and numerical comparisons are in [rejection](rejection/). Acceptance tolerances were not loosened. Combined with the public-call regressions in all three overdraw repeat rounds, this is reason to remove the experiment rather than ship it behind a switch. The pre-experiment shader bodies are restored; the candidate arithmetic and focused tests are retained under `tools/experiments/` for future investigation. The timings above remain measurements of `5bdfaaa5`, not the accepted final renderer.
