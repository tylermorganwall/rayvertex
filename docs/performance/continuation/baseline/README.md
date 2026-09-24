# Continuation baseline

Starting clean commit: `b755958b33ef3250f057c5c8bb34c94f03a0ae71` (renderer `d48a259`). Installed baseline: `/tmp/rayvertex-optimization/lib-screen-final`; rendering sources match that checkpoint. The existing corrected scalar references remain retained. A fresh seven-sample 800×800/FSAA 1/one-worker small-scene benchmark and 20-render, 1 ms R sampling profile were collected before implementation. The latter includes memory profiling overhead and is a work-attribution diagnostic, not a speed measurement. Counts/time percentages overlap for nested calls.

R profile self time: `render_clamp` 28.22%, `to_linear` 17.82%, decode helper 9.35%, native `.Call` 10.88%. This motivates output traversal fusion before speculative ISA work. Raw samples and dependency versions are retained here. The prior full 14-workload baseline is in `../../structural/delivery`; new phases compare isolated before/after libraries again.
