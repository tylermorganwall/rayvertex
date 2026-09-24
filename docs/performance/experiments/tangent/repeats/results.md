# Same-binary tangent off/on repeats

Both variants use `5bdfaaa5`; reference has the switch unset and final has `RAYVERTEX_TANGENT_ALGEBRA=1`. Three separate processes per variant/case, five warm and five isolated native samples per process. 800²/FSAA 1/one worker; alternating order; GC outside timers. No compilation, tests or competing measured renders during collection.

| Case | Round | Warm median / p95 ms (off → on) | Native median / p95 ms (off → on) |
|---|---:|---:|---:|
| tangent | 1 | 61.0 → 61.0 / 81.8 → 86.0 | 44.0 → 45.0 / 45.0 → 45.0 |
| tangent | 2 | 65.0 → 64.0 / 85.8 → 82.8 | 44.0 → 44.0 / 44.8 → 44.8 |
| tangent | 3 | 60.0 → 59.0 / 80.8 → 79.8 | 45.0 → 44.0 / 45.8 → 44.0 |
| tangent_overdraw | 1 | 607.0 → 611.0 / 618.6 → 632.4 | 598.0 → 590.0 / 606.0 → 595.0 |
| tangent_overdraw | 2 | 615.0 → 635.0 / 634.8 → 667.8 | 590.0 → 587.0 / 592.0 → 606.4 |
| tangent_overdraw | 3 | 601.0 → 603.0 / 623.8 → 621.8 | 591.0 → 586.0 / 601.2 → 606.0 |

The same-binary repeats show no public-call improvement on the overdraw workload: all three warm medians regress by 2–20 ms. Its isolated native medians improve by 3–8 ms (about 0.5–1.3%), while ordinary tangent results are mixed. This was insufficient evidence to enable the experiment; it was subsequently removed after the sanitizer equivalence failure. No assembly or hardware-counter explanation is claimed.
