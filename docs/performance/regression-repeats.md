# Regression repeats and interpretation

These are additional runs, not replacements for the original matrix. Three fresh processes per variant/case, five warm and five native samples each, alternating variant order by round and case. Settings: 800×800, FSAA 1, one worker. The short repeat driver omits debug serialization, Rprofmem and instrumented phases; it captures native arguments during one untimed warm-up. It retains those input arguments during public timings. Raw samples are in `raw/repeats/`; aggregates in [repeat-summary.csv](repeat-summary.csv).

| Case | Warm median / p95, reference → final | Native median / p95, reference → final |
|---|---|---|
| small | 472.0/576.6 → 270.0/430.3 | 96.0/152.6 → 43.0/103.2 |
| ssao | 954.0/1080.3 → 935.0/1079.5 | 601.0/705.3 → 622.0/718.3 |
| shadow | 419.0/548.3 → 472.0/1711.0 | 144.0/203.5 → 179.0/199.3 |
| toon | 659.0/764.2 → 627.0/739.8 | 213.0/366.7 → 223.0/379.0 |

## Serial screen-pass isolation

Phase 6 versus phase 8, same one-worker SSAO fixture, ten samples per process. The order is reversed in round 2.

| Round | Phase 6 warm/native medians | Phase 8 warm/native medians |
|---|---|---|
| 1 | 894.5/567.5 | 941.0/572.0 |
| 2 | 876.0/581.5 | 945.0/613.5 |

The original one-worker matrix recorded SSAO 595→808 ms and toon 417→564 ms. These larger regressions do not repeat consistently in the balanced runs. The original shadow regression remains in the aggregate repeat results, but per-process native medians change direction: round 1 is 133→180 ms, round 3 is 192→95 ms. One final shadow public sample takes 4343 ms; it remains in the data and p95 rather than being discarded. For the identical final small-scene binary, per-process public medians range from 213 to 414 ms while native medians are 43 ms in both those rounds.

The cause of this variability has not been isolated. These are sequential renderer jobs on a general-purpose machine, not CPU-affinity-controlled or OS-load-isolated trials. Thermal/performance status queries were unavailable in the sandbox. Do not interpret small differences, a single p95, or the best repeat as a reliable speedup. The report retains both the matrix and these follow-ups.

Default decisions: retain work removal and batching because the large geometry allocation reductions and native gains are substantial, with exact output comparisons. Retain shared decoding based on measured decode counts, memory, and focused timing. Retain parallel screen passes based on the focused four-worker phase comparison and the resolution/worker sweep; the one-worker follow-up does not establish a stable end-to-end win. Keep indexed transforms off because their modest mixed gains do not justify added cache memory. Public core defaults stay unchanged; the sweep does not establish a universal optimal worker count. Batch size 64 is retained as an initial bounded choice, not a claim that it wins every row.
