# Phase decisions and work counters

Read these observations alongside all rows in [phase-results.md](phase-results.md),
including regressions, p95 and memory. They describe the focused phase runs,
not a promise that every setting in the later matrix improves.

* **Correctness:** separate presentation/linear depth and immutable SSAO blur
  require independent storage. R allocations therefore increase: for the small
  fixture 527.4→537.2 MiB allocated per call, and SSAO 545.3→560.0 MiB. These are
  correctness costs, not optimizations. Gray sampling and steep-line undefined
  behavior cannot be judged by matching invalid prior output. Refraction-only
  high-water history is in the two `lifetime-*.csv` files.
* **Work removal:** the 1M fixture's shader setup/allocation diagnostic drops
  476.23→41.89 ms median and native wall time drops 1464→383 ms. Fixed-size corner
  storage removes per-triangle heap-backed inner vectors. Disabled outline and
  shadow payload counters are zero. Warm small-scene time drops 246→218 ms,
  while its full benchmark RSS rises 866.7→959.7 MiB. Toon warm time slightly
  increases 357→362 ms. Reduced buffers do not guarantee reduced process RSS.
* **Scheduling:** the same 1M fixture still has 1,002,528 triangles, one merged
  model, 19,126 active blocks and 1,434,093 bin references. Serial queued tasks
  drop 40,000→0; serial rendering executes active blocks directly. The small
  fixture has 6,400 active blocks instead of scheduling all 40,000 blocks.
  Coverage ownership and primitive order are unchanged. The change does not
  infer a tile-times-original-object loop in the merged ordinary API.
* **Early depth:** the 15-sample occlusion comparison changes native median
  173→169 ms and public median 356→352 ms; public p95 rises 365.8→372.2 ms.
  Full-process RSS rises 1231.6→1285.4 MiB with identical R allocation totals.
  This is a small reciprocal-work saving, not an opaque visibility pass. Shader
  calls and accepted fragments remain unchanged, and the measured change is
  small enough that noise matters.
* **Texture sharing:** a same-build cache-disabled diagnostic decodes 32 images
  (100,663,296 bytes); the cache decodes one (3,145,728 bytes). Both main and
  shadow shaders share immutable storage, with the same original channel count
  and decode options. Focused warm time drops 300.5→228.5 ms. Separate one-render
  RSS drops 509.6→487.5 MiB; full benchmark RSS 1289.9→1041.3 MiB. This is a
  frame-local cache, so each public render still decodes its assets.
* **Screen passes:** at four workers in the focused suite, SSAO warm/native
  medians change 443/261.5→280.5/106 ms, and toon 331/153.5→267/88 ms. R allocation
  totals are unchanged. Full-process RSS falls for those runs. SSAO still uses
  64 deterministic samples and its exact corrected blur footprint. JFA still
  uses the same iteration schedule, neighborhood and seed ties, with a barrier
  before swapping each pair of buffers. The final matrix and follow-up runs
  must also be consulted for single-worker regressions and variance.
* **Indexed transforms:** 1M mesh main clip evaluations change from 3,007,584
  corner evaluations to 502,681 indexed evaluations; indexed payload is
  28,150,136 bytes. The 500k case regresses 405→435 ms end to end despite native
  218→208 ms. The 100k native time is 93→94 ms. This remains **experimental and
  disabled by default**. It is not the brief's complete clipping/setup phase.

Native payload counts cover selected storage, not every allocation. At measured
checkpoint `3bb9586`, the scalar clip diagnostic counts three corners per face,
which is correct for the timed shader families. Delivery correction `18751b4`
counts each face's actual shader requirements, including both clip forms used
by diffuse tangent shaders. Its new test fails before the correction (2880 vs
5760 evaluations) and passes afterward. The additional traversal only runs with
profiling enabled; historical profiling-overhead values therefore describe the
recorded checkpoint. A separate delivery diagnostic measurement is retained.
