# Rejected tangent renderer experiment

The scalar and algebraic kernels and focused tests here preserve the experiment introduced at `5bdfaaa5`. The renderer no longer dispatches it, and `RAYVERTEX_TANGENT_ALGEBRA` has no effect on the accepted build. Run the archived R tests against that candidate revision, not the current package.

Release comparisons were exact, and standalone arithmetic checks passed at O1/O2 under ASan/UBSan. Nevertheless, the integrated ASan O1 renderer differed from its scalar path in normal debug buffers (maximum 3.7252902984619141e-9). An explicit float projection temporary did not resolve that difference. Same-binary timing repeats showed small isolated-native savings on overdraw but worse public-call medians. The candidate was removed rather than weakening acceptance tolerances.

The [phase report](../../docs/performance/experiments/tangent/results.md) retains code history, timings, memory, exact release comparisons, and sanitizer failure evidence. `tools/test-tangent-basis.cpp` exercises the archived arithmetic helper; passing it is not evidence that the whole renderer is equivalent.
