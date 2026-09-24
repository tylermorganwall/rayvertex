# Sustained ordinary API rendering, including cleanup

Corrected scalar reference `7f72368` versus delivery `18751b4`, 800×800, FSAA 1, one worker, identical fixture/camera/light settings. Variant order alternates by case. Each fresh process performs an untimed warm-up and initial GC, then ten public renders without forced collection between frames (five for alpha64). Automatic collections occur inside frame timings. The final image is released and a final GC runs before stopping the total-loop timer. Fixture creation and package loading are outside the timer; API preparation, decode, output processing and final cleanup are inside. There is no native-argument capture, tracing or profiling in this regime.

Throughput below is frames divided by the **total loop time including final cleanup**, not the reciprocal of the fastest frame. Frame medians/p95 include automatic collections; the separate loop mean also amortizes the final cleanup. Small sample p95 remains descriptive. These runs complement rather than replace the per-call matrix and regression repeats. All samples are retained in `raw/sustained/`; exact values in [sustained-summary.csv](sustained-summary.csv).

| Case | N per variant | Frame median / p95, reference → final (ms) | Mean including final cleanup, reference → final (ms) | FPS including cleanup, reference → final |
|---|---:|---|---|---|
| small | 10 | 207.5/246.9 → 179.0/210.2 | 221.1 → 189.5 | 4.52 → 5.28 |
| grid100k | 10 | 361.0/377.6 → 240.0/256.2 | 367.7 → 245.5 | 2.72 → 4.07 |
| grid500k | 10 | 884.5/910.6 → 382.0/393.6 | 888.7 → 384.6 | 1.13 → 2.60 |
| grid1m | 10 | 1533.5/1546.0 → 557.0/578.0 | 1524.7 → 564.5 | 0.66 → 1.77 |
| occluded | 10 | 442.5/461.6 → 315.0/347.5 | 447.3 → 323.7 | 2.24 → 3.09 |
| alpha4 | 10 | 410.5/439.8 → 378.0/397.9 | 419.1 → 384.2 | 2.39 → 2.60 |
| alpha16 | 10 | 1017.5/1053.2 → 976.0/1001.6 | 1024.5 → 983.6 | 0.98 → 1.02 |
| alpha64 | 5 | 3292.0/3357.6 → 3290.0/3315.6 | 3314.0 → 3290.6 | 0.30 → 0.30 |
| ssao | 10 | 463.5/480.6 → 467.5/492.6 | 468.5 → 471.0 | 2.13 → 2.12 |
| shadow | 10 | 272.0/285.3 → 240.0/256.0 | 275.3 → 248.9 | 3.63 → 4.02 |
| toon | 10 | 345.0/360.8 → 322.0/358.3 | 346.4 → 333.5 | 2.89 → 3.00 |
| shared_textures | 10 | 303.0/342.1 → 203.0/235.8 | 314.5 → 211.5 | 3.18 → 4.73 |
