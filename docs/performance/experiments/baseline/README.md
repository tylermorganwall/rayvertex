# Remaining-experiments starting point

Clean source pin: `05cec195fb4dfe008bb1bf6198749b5a4e4714a4` (renderer `eec25f71`). Original project pin and corrected scalar branches remain unchanged. Isolated release baseline: `/tmp/rayvertex-optimization/lib-varying-demand`, rebuilt at the end of the preceding continuation with the recorded O2 Makevars. Baseline source tests: 1,310 passing; validation details remain in `../../continuation/validation/README.md`.

Before renderer edits, fresh 800²/FSAA 1/one-worker runs collected seven small-scene and three shared-normal/tangent-overdraw samples, plus separate native and instrumented runs. New procedural fixtures add explicitly shared and indexed non-unit normals and tangent normal-map shading; existing fixtures keep their original defaults. Phase comparisons rerun both variants and include separate fresh RSS, allocations and image differences. Do not use these baseline-only records to claim a speedup.
