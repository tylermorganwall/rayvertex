# Visibility OFF → ON at `d48a259`

Both libraries are the same build; only the after process sets `RAYVERTEX_VISIBILITY=1`. Three warm public calls and three isolated native samples per setting, with GC outside their timers. One separate diagnostic sample per setting supplies phase timings/counters; its p95 is not a distribution. Variant order alternates between settings. Quality is identical within each pair. Cold time, R allocations and diagnostic-process RSS remain in the CSV summaries. These short samples do not establish stable tail latency.

| Scene | Output / FSAA / workers | Public median / p95 ms | Native median / p95 ms | Fresh RSS MiB |
|---|---|---:|---:|---:|
| overdraw | 800×800 / 1 / 1 | 536.0 → 264.0 / 553.1 → 273.9 | 368.0 → 85.0 / 370.7 → 88.6 | — → — |
| overdraw | 800×800 / 1 / 2 | 360.0 → 224.0 / 378.9 → 247.4 | 197.0 → 57.0 / 198.8 → 57.0 | — → — |
| overdraw | 800×800 / 1 / 4 | 291.0 → 209.0 / 296.4 → 231.5 | 112.0 → 40.0 / 114.7 → 40.9 | — → — |
| overdraw | 800×800 / 1 / 10 | 250.0 → 206.0 / 253.6 → 221.3 | 67.0 → 32.0 / 67.9 → 32.0 | — → — |
| overdraw | 1920×1080 / 2 / 4 | 2643.0 → 2157.0 / 2694.3 → 2183.1 | 684.0 → 183.0 / 684.9 → 187.5 | — → — |
| small | 800×800 / 1 / 4 | 188.0 → 185.0 / 195.2 → 188.6 | 5.0 → 6.0 / 5.0 → 6.9 | — → — |

Exact image/buffer comparisons: 102 / 102; maximum absolute difference 0. Differences include metadata, nonfinite patterns and changed-sample counts.

Fresh RSS was measured separately for selected high-sample-count and deep-layer cases only. A dash means unmeasured, not zero. Whole diagnostic-process RSS includes retained outputs and profiling; it is not a fresh-render memory result. No plotting or file encoding is included.


The experiment remains opt-in. Its eligible untextured opaque overdraw case benefits across these worker/quality settings, while this is not a representative proof for every material or tiny/low-overdraw scene. The earlier low-overdraw million-triangle comparison was effectively unchanged; unknown/alpha-dependent/mixed tiles retain forward shading. No depth-prepass alternative, near-first order, or texture-opacity extension is claimed. The same original sequence resolves equality, and exceptional shader results trigger replay before committing colors/auxiliaries.
