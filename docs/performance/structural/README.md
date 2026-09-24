# Structural continuation

Continuation starts at `f15efde`, whose renderer is `18751b4`. The original starting commit and corrected scalar reference remain pinned in the parent report. Comparison libraries are isolated under `/tmp/rayvertex-optimization`; no quality settings changed.

## Sparse exact transparency

`FragmentArena` replaces per-sample trees with tile-owned contiguous records. Explicit tile-local submission sequence preserves equal-depth last-write-wins, triangles before serial lines, and the original back-to-front operation and auxiliary-buffer source. Both line algorithms, standalone line renders, main triangles, and transparent shadows use it. Capacity grows without a layer cap and dies with the frame. `RAYVERTEX_REFERENCE_TRANSPARENCY=1` selects trees for developer comparisons.

The 245 corrected scalar render/debug outputs match exactly. Source package tests pass. `tools/test-fragment-arena.cpp` compares 750,000 randomized inserts against trees under ASan/UBSan, including >129 layers, signed zero, infinity, rectangular dimensions, auxiliary attributes, and invalid block size.

`tools/bench-rasterizer-structural.py` runs sequential before/after ordinary API, isolated native, phase diagnostic, fresh-render RSS and sustained-loop measurements for six workloads at 800×800, FSAA 1, one worker. Timings and image comparisons will be recorded after the run. This is not yet a speedup claim. Sorting and retained vector capacity can lose against trees on some layer distributions; measurements decide the default.
