# Measured phase: fbe4359 → b448d6b

800×800; FSAA 1; one worker; unchanged fixture/lighting/quality. Three warm samples; three separate uninstrumented native samples; three diagnostic profiles. Cold is the first render in a fresh R process including preparation/decode, with OS file caches uncontrolled. Fresh-render RSS is a separate one-render process. Sustained mean covers five ordinary calls plus final release/GC; warm/native samples force GC outside each timer.

| Scene | Cold ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| alpha16 | 572.0 → 573.0 | 507.0 → 510.0 / 513.3 → 516.3 | 326.0 → 326.0 / 326.0 → 326.0 | 514.0 → 511.8 | 401.5 → 389.2 |
| grid100k | 302.0 → 285.0 | 211.0 → 214.0 / 232.6 → 227.5 | 48.0 → 37.0 / 48.0 → 37.9 | 212.6 → 206.4 | 407.4 → 439.1 |
| grid1m | 550.0 → 462.0 | 494.0 → 400.0 / 497.6 → 406.3 | 303.0 → 216.0 / 307.5 → 222.3 | 498.0 → 412.2 | 1081.1 → 1013.6 |
| grid500k | 431.0 → 387.0 | 315.0 → 277.0 / 337.5 → 279.7 | 166.0 → 126.0 / 171.4 → 131.4 | 351.6 → 308.2 | 673.8 → 677.0 |
| shadow | 267.0 → 259.0 | 204.0 → 221.0 / 221.1 → 230.0 | 20.0 → 20.0 / 20.9 → 20.0 | 192.2 → 191.2 | 391.4 → 359.3 |
| small | 250.0 → 252.0 | 193.0 → 190.0 / 198.4 → 191.8 | 8.0 → 8.0 / 8.0 → 8.0 | 181.2 → 179.0 | 371.6 → 340.1 |

Image/buffer comparisons: 102; exact: 102. Maximum absolute difference: 0. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.


A five-second native sample at `fbe4359` recorded 1,164 top-of-stack samples in `ModelInfo::vertex`, 957 in scalar coverage and 338 in diffuse vertex shading (thread samples are not elapsed percentages). Model construction now acquires checked read-only pointers/strides on the R thread. Rcpp owners keep arrays rooted; no worker creates R containers, and no additional packed geometry copy is allocated. Position/normal/UV indices and per-face/global normal flags remain independent. Material lookup uses the same stable view. Optional indexed transforms retain their existing arithmetic and are still off by default.

The one-million-triangle native median falls 303→216 ms; sustained public mean 498→412.2 ms. The 100k warm public median rises 211→214 ms despite a native gain, and the shadow warm median rises 204→221 ms despite unchanged native timing; sustained means do not show corresponding regressions. Fresh RSS is mixed. Both effects are retained rather than claiming uniform gains. All 245 clipped-scalar corpus outputs pass. The source suite passes including prepared snapshots, independent-index coverage and callback semantics; final sanitizer/worker validation is recorded separately.
