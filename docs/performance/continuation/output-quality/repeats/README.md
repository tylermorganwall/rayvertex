# High-sample-count SSAO repeat

Three alternating process rounds, five warm public and five native calls each, with GC outside timers. Same 1920×1080 / FSAA 2 / four-worker SSAO fixture. Before library is `lib-screen-final` (`d48a259`); after is `lib-output` (`69ae102`). This shorter harness does not serialize debug buffers or profile allocations, so its process-allocation history differs from the full sweep.

| Variant | Round | Public median ms | Native median ms |
|---|---:|---:|---:|
| before | 1 | 2540.0 | 410.0 |
| before | 2 | 2634.0 | 389.0 |
| before | 3 | 2382.0 | 391.0 |
| after | 1 | 1645.0 | 423.0 |
| after | 2 | 1635.0 | 442.0 |
| after | 3 | 1567.0 | 397.0 |

The public-call improvement repeats in all three rounds. The native after medians remain higher in each pair (410→423, 389→442, 391→397 ms), but the 403→549 ms gap from the full sweep does not repeat at that magnitude. Native renderer source is unchanged in the output phase; this does not prove identical native timing after different allocation histories or binary layout. The cause remains unresolved, and neither the original outlier nor these smaller native regressions is removed from the report.
