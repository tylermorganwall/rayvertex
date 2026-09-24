# Rasterizer optimization ledger

Starting commit: `b09385978f40f976e7e97286191d64d16184b7de`.
The checkout was clean before this work. The original source and installed
package are retained in `/tmp/rayvertex-optimization/baseline-src` and
`/tmp/rayvertex-optimization/lib-baseline` for this session. Reconstruct the
source with `git archive` at the recorded commit.

Environment: Apple M1 Max, 10 physical/logical cores (heterogeneous performance
and efficiency cores), 64 GiB RAM, macOS 15.7.3, native arm64 R 4.6.0, Apple
clang 16.0.0. Effective compilation uses `-falign-functions=64 -Wall -g -O2`;
no native-ISA or fast-math flags. The user's compiler cache could not start in
the sandbox, so builds bypass it using a temporary Makevars with the same
compiler and optimization flags and four build jobs. This is a build-only
change, not a renderer optimization.

The attached brief is an engineering specification; its static observations
are not performance measurements. Changes and measurements below are grouped
by phase. Corrections are compared separately from equivalent-behavior work.

## Reproduction

Developer benchmark scripts live in `tools/`. They use deterministic local
meshes, ordinary public calls, no plotting and no encoding. Process-cold runs
do not flush the OS file cache. Raw samples are retained alongside summaries;
small sample counts do not establish a reliable tail-latency estimate.

## Status

Baseline build, existing tests and measurement harness are in progress.
