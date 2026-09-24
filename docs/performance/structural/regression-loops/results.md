# Repeated public-call regression checks

Three process rounds per scene/variant, alternating variant order between rounds. Each process warms once, then times ten ordinary public calls with automatic GC and final output cleanup inside the loop timer. 800×800, FSAA 1, one worker. Before is `e512c02`; after and full-buffers are `d48a259`. The latter sets only `RAYVERTEX_REFERENCE_BUFFERS=1` and is a diagnostic control, not a recommended preset.

| Scene | Variant | Process means (ms/frame) | Median process mean (ms) |
|---|---|---|---:|
| small | before | 184.0, 185.4, 182.6 | 184.0 |
| small | after | 176.2, 178.5, 176.7 | 176.7 |
| small | full_buffers | 179.0, 180.0, 181.2 | 180.0 |
| toon | before | 312.6, 318.3, 317.4 | 317.4 |
| toon | after | 324.0, 324.5, 326.2 | 324.5 |
| toon | full_buffers | 318.8, 323.9, 324.0 | 323.9 |
| environment | before | 232.6, 232.8, 235.2 | 232.8 |
| environment | after | 235.8, 232.6, 234.1 | 234.1 |
| environment | full_buffers | 236.1, 236.6, 238.6 | 236.6 |

Toon retains a 7.1 ms (2.2%) regression between median process means, despite the native-stage improvement in the broad table. Full buffers do not recover the earlier public time, so buffer allocation alone is not an established cause. The environment difference is only 1.3 ms (0.6%) here, smaller than the broad five-frame result; its intervals overlap. The small control improves here despite the broad warm-median regression. All original results remain reported. This follow-up measures timings only; the corresponding exact differences and memory/phase measurements are in [delivery](../delivery/results.md).

There are three independent process means, not thirty independent process measurements. Individual frame CSVs retain automatic-GC/outlier behavior; no stable tail-latency or general speedup claim follows from these samples.
