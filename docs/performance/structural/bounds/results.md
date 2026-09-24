# Measured phase: b448d6b → 811143b

800×800; FSAA 1; one worker; unchanged fixture/lighting/quality. Three warm samples; three separate uninstrumented native samples; three diagnostic profiles. Cold is the first render in a fresh R process including preparation/decode, with OS file caches uncontrolled. Fresh-render RSS is a separate one-render process. Sustained mean covers five ordinary calls plus final release/GC; warm/native samples force GC outside each timer.

| Scene | Cold ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| near_crossing | 286.0 → 280.0 | 230.0 → 218.0 / 235.4 → 237.8 | 24.0 → 24.0 / 24.9 → 24.9 | 210.2 → 213.2 | 364.0 → 385.9 |
| shadow | 260.0 → 263.0 | 210.0 → 206.0 / 227.1 → 231.2 | 20.0 → 20.0 / 20.9 → 20.0 | 191.4 → 193.8 | 405.7 → 374.0 |
| small | 251.0 → 257.0 | 194.0 → 184.0 / 194.9 → 187.6 | 8.0 → 7.0 / 8.0 → 7.0 | 180.6 → 184.0 | 379.6 → 340.0 |

Image/buffer comparisons: 51; exact: 51. Maximum absolute difference: 0. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.


This is a correctness follow-up, not a speedup phase. UBSan reproduces signed overflow in the previous tile-end expression at width `INT_MAX` and block size `INT_MAX/2+1` without allocating a huge framebuffer. Tile/reference-arena ends now clamp the span before addition. PCF border offsets widen before addition. A finite but subnormal area whose reciprocal overflows is rejected before invalid barycentrics can enter interpolation. The focused ASan/UBSan test covers both the extreme tile and the subnormal-area rejection. All 51 measured buffers and the 245-result ordinary corpus remain exact; the historical undefined state is not an image oracle. The slightly higher sustained means in these three short controls are retained, with no performance claim.

`codex/raster-final-scalar-reference` pins `811143b`, including this follow-up. Its golden corpus is generated with one worker, tree transparency, full auxiliary buffers, serial screen work, and uncached ordinary material decodes; it matches the earlier clipped scalar corpus exactly on all 245 cases. Earlier scalar branches and correction evidence remain retained.
