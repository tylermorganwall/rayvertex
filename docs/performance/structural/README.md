# Structural continuation

Continuation starts at `f15efde`, whose renderer is `18751b4`. The original starting commit and corrected scalar reference remain pinned in the parent report. Comparison libraries are isolated under `/tmp/rayvertex-optimization`; no quality settings changed.

## Sparse exact transparency

`FragmentArena` replaces per-sample trees with tile-owned contiguous records. Explicit tile-local submission sequence preserves equal-depth last-write-wins, triangles before serial lines, and the original back-to-front operation and auxiliary-buffer source. Both line algorithms, standalone line renders, main triangles, and transparent shadows use it. Capacity grows without a layer cap and dies with the frame. `RAYVERTEX_REFERENCE_TRANSPARENCY=1` selects trees for developer comparisons.

The 245 corrected scalar render/debug outputs match exactly. Source package tests pass. `tools/test-fragment-arena.cpp` compares 750,000 randomized inserts against trees under ASan/UBSan, including >129 layers, signed zero, infinity, rectangular dimensions, auxiliary attributes, and invalid block size.

`tools/bench-rasterizer-structural.py` runs sequential before/after ordinary API, isolated native, phase diagnostic, fresh-render RSS and sustained-loop measurements for six workloads at 800×800, FSAA 1, one worker. Measured results and raw evidence are in [sparse/results.md](sparse/results.md). Sustained alpha4/16/64 times improve, with exact image/buffer comparisons. Small-scene timing is effectively unchanged. Fresh RSS improves for alpha16/64, but alpha4, small, and shadow fresh-process RSS increases; these results are retained rather than claiming uniform memory savings.

## Once-per-primitive setup and contiguous bins

`TriangleSetup` resolves projected vertices, reciprocal w, culling, area, integer bounds, edge slopes and material once per primitive/pass. Tile-local robust edge origins and multiply-from-origin stepping retain the original numeric order. Contiguous bins count, prefix-sum and fill references in original model/face order with overflow checks. Construction is serial: it removes nested allocations without an unbounded parallel histogram. Vertex shader callbacks and independent attribute indices are unchanged. Repeated full-frame clip arrays are replaced by setup storage. Shadow setup/bin/coverage/resolve/clear and main setup/bin stages now have individual timers.

All 245 corrected-reference outputs and the source test suite pass. The standalone ASan/UBSan bin test checks setup counts, references for large triangles, ordering and partial edge blocks. Homogeneous clipping and changed coverage rules are deliberately absent from this equivalent-behavior commit. Measurements follow in a separate evidence commit.
