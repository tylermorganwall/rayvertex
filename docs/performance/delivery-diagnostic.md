# Delivery diagnostic follow-up

Commit `18751b4` corrects the scalar diffuse-tangent clip counter. Its new test
fails on checkpoint `3bb9586` (2880 versus 5760) and passes on delivery. The extra
face traversal runs only when profiling is enabled. Historical phase and
profiling-overhead tables describe their recorded checkpoint, not this traversal.

A separate 1M-triangle, 800×800, FSAA 1, one-worker check (five samples) records
public median/p95 625.0/649.8 ms,
native median/p95 447.0/471.4 ms,
and profiler-on native median 419.0 ms.
Benchmark-process RSS is 2028.2 MiB and R allocations
are 674.4 MiB. Profiler-on/off timing variation
is larger than the expected diagnostic cost in this run; this is not evidence
of negative profiling overhead or a new rendering speedup.

All 17 image/debug comparisons with checkpoint `3bb9586` are exact; see
[delivery-counter-differences.csv](delivery-counter-differences.csv).
Full rows and phase counters: [summary](delivery-diagnostic/summary.csv),
[phases](delivery-diagnostic/phases.csv). Raw samples are in
`raw/delivery-diagnostic/`.
