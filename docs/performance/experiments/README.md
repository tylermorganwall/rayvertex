# Normal, composition and tangent experiments

This continuation starts at clean `05cec195fb4dfe008bb1bf6198749b5a4e4714a4`, with the original project pin `b09385978f40f976e7e97286191d64d16184b7de` and corrected scalar branches/golden outputs retained. The [fresh baseline](baseline/README.md) preceded renderer changes. Hardware and toolchain remain Apple M1 Max, macOS arm64, R 4.6 and clang 16 at O2, as recorded in the [preceding baseline](../continuation/baseline/README.md). There are no new correctness conventions or reductions in rendering quality.

| Phase / implementation commit | Default | Evidence and decision |
|---|---|---|
| Bounded normal reuse, `511577a3` | Off | [Seven workloads](normals/results.md). Shared-normal cache: one transform and 3,007,583 hits in 25 bytes, but public median 256 → 254 ms and sustained 269.4 → 272.0 ms. Mixed results justify keeping it experimental. |
| SSAO/background composition, `5eb61c63` | On | [Six workloads](composition/results.md), [HD/FSAA-2 sweep](composition-quality/results.md). Small 800² public median 24 → 18 ms; HD SSAO 1655 → 1539 ms and fresh RSS 2471.9 → 2308.4 MiB. HD grid public time is unchanged. |
| Tangent candidate, `5bdfaaa5`; retired by `f1d7e122` | Removed | [Six workloads](tangent/results.md). Ordinary tangent native median 43 → 45 ms; overdraw 581 → 580 ms. [Same-binary repeats](tangent/repeats/results.md) show 3–8 ms native savings on overdraw but 2–20 ms public regressions; it was removed after an ASan normal-buffer mismatch. |

Normal caching is frame-local, main-thread-only and limited to 64 MiB of aggregate value/validity payload. Cache identity includes the exact transformation matrix and pre-transform normalization convention. An insufficient budget keeps the scalar route. Geometric fallback normals reuse the original first-vertex expression, preserving each shader's normalization order. No global mutable geometry cache is introduced.

Composition keeps R's exponentiation of the ambient matrix, then fuses the existing RGB multiplication and background replacement into assembly/decode/orientation. Debug paths retain their old buffers and effects. Tone mapping, bloom, FSAA filtering, alpha and rayimage attributes are unchanged. The reference composition and output paths remain independently available.

The tangent candidate retained float matrix/projection arithmetic, double normalization, determinant signs and zero-column products. Release comparisons and standalone O1/O2 arithmetic tests were exact, but the integrated ASan O1 renderer differed from its own scalar path in tangent normal debug buffers (maximum 3.7252902984619141e-9). An explicit float-temporary trial did not resolve the mismatch. Together with the public-call regressions, this failed acceptance. Commit `f1d7e122` restores the pre-experiment tangent shader bodies; candidate code/tests remain under `tools/experiments/`, and all failure evidence is retained in the [phase report](tangent/results.md). Tolerances were not loosened.

## Accepted final renderer

The final renderer at `f1d7e122` is source-equivalent to `5eb61c63` after ignoring whitespace: composition is enabled, normal caching is opt-in, and the original scalar tangent shader bodies are restored. The immutable `lib-composition` install is therefore used for the accepted final timing run; the complete sanitizer/no-thread/package validation rebuilds the accepted source. The prior candidate-default and prepared measurements remain separately labelled as historical evidence.

The [accepted eight-workload comparison](accepted-delivery/results.md) has 136/136 exact image/buffer comparisons. At 800²/FSAA 1/one worker, small-scene warm public time is 23 → 18 ms, the 1M grid is 238 → 235 ms, and SSAO fresh RSS is 258.9 → 223.1 MiB. Alpha16 sustained time regresses 348.8 → 352.0 ms and RSS increases 371.8 → 378.4 MiB. Native changes are small and mixed; no general native speedup is claimed.

The [accepted prepared comparison](accepted-prepared/results.md) has 51/51 exact comparisons. Shared-normal warm calls change 247 → 239 ms, with preparation 24 → 26 ms and preparation-plus-first 365 → 350 ms. The small prepared first call regresses 95 → 98 ms while warm calls improve 23 → 16 ms. Costs of constructing and retaining the prepared snapshot stay included where stated.

## Measurement contract

Each phase compares immutable installed builds, with variants alternating order and no concurrent build, test or renderer process launched by the driver. Public cold/warm, isolated native, instrumented phases, sustained calls including cleanup, R allocations and fresh one-render RSS are separate measurements. Phase CSVs include diagnostic counters; R boundary traces include composition/output work. Every phase retains exact image/buffer comparisons, including attributes and nonfinite patterns. Regressions and outliers remain in the reports.

Cold means a fresh R process, not a flushed operating-system file cache. Three-sample median/p95 results are descriptive, not stable tail estimates. Diagnostic timings have instrumentation overhead. Rprofmem excludes C++ allocations; cache payload counters are not process RSS. Public-call changes on a shader that does not use a new kernel cannot be attributed to that kernel.

Active developer switches are `RAYVERTEX_NORMAL_CACHE=1` and `RAYVERTEX_REFERENCE_COMPOSITION=1`. The retired `RAYVERTEX_TANGENT_ALGEBRA` switch has no effect on the accepted renderer. `RAYVERTEX_REFERENCE_OUTPUT=1` also restores the old R composition/output route. These add no public quality preset or mandatory compiler/ISA flag. The final default retains normal caching disabled.

## Validation

The [accepted validation report](validation/README.md) records 1,412 release assertions, 1,392 no-thread assertions, 245 exact scalar-reference outputs and 44 exact extended shader/debug outputs. Both sanitizer builds pass 464 focused assertions. The broad ASan/UBSan run still reports the pre-existing upstream resize alignment failure even though its numerical comparison passes. `R CMD check` has no errors/warnings and the existing `tail()` NOTE. The tangent candidate's separate failure remains archived rather than counted as an accepted pass.

## Reproduction and remaining scope

Build each pinned revision into a separate library with the recorded Makevars and `R CMD INSTALL --preclean`. `tools/bench-rasterizer-structural.py BEFORE_LIB AFTER_LIB OUTPUT` records ordinary/native/diagnostic/sustained/RSS results; `RAYVERTEX_STRUCTURAL_CASES` selects the cases listed in each archived `settings.json`. Enable the normal switch for its phase. To reproduce the rejected tangent phase, build `5bdfaaa5` and enable its historical tangent switch; the preceding and accepted renderers ignore it. Keep them unset for composition and combined default comparisons. Use `RAYVERTEX_PREPARED=1` for both variants of a prepared comparison.

`tools/bench-rasterizer-structural-matrix.py BEFORE_LIB AFTER_LIB OUTPUT composition` runs the HD comparison. `tools/bench-rasterizer-repeats.py` supports `RAYVERTEX_REPEAT_CASES=tangent,tangent_overdraw` and `RAYVERTEX_TANGENT_AFTER=1` for three independent rounds of five warm/native samples, switching only the final variant in the same binary. Validation commands are retained in `tools/validate-rasterizer-experiments.sh` and the phase logs.

The [remaining-work ledger](../structural/remaining.md) still lists submesh bounds, a separate large-triangle list, a universal frame layout, packed per-material attributes, broader shader dispatch, hierarchical depth, prepared native geometry, SIMD and further effects/resize fusion. These experiments do not complete those designs or establish their performance. Windows/Linux/actual Emscripten, hardware counters, supported leak detection, forced allocation failure/interrupt injection, OS-cache-cold, and plotting/encoding performance remain unmeasured.
