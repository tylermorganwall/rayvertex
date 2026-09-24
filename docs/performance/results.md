# Corrected scalar reference versus final default

Reference `7f72368`; final renderer `3bb9586`; indexed transforms OFF. For automatic-GC throughput, see [sustained.md](sustained.md). See [README.md](README.md) for machine, settings, timing boundaries and limitations. All timing values are milliseconds; memory values are MiB. Raw values and repetition counts: [reference](matrix-reference/summary.csv), [final](matrix-final/summary.csv).

## Representative workloads

800×800, FSAA 1, one worker; five warm samples except alpha64 (three). Columns show reference → final.

| Case | Cold | Warm median / p95 | Native median / p95 | FPS | Single-render RSS | Benchmark RSS | R allocations |
|---|---:|---|---|---:|---:|---:|---:|
| small | 371.0 → 342.0 | 244.0/249.2 → 215.0/220.4 | 67.0/87.4 → 34.0/56.2 | 4.1 → 4.7 | 546.3 → 477.7 | 967.9 → 1349.7 | 537.2 → 531.2 |
| grid100k | 569.0 → 409.0 | 412.0/427.0 → 278.0/286.0 | 232.0/238.8 → 88.0/88.8 | 2.4 → 3.6 | 627.5 → 565.6 | 1068.5 → 1294.1 | 549.5 → 543.5 |
| grid500k | 1127.0 → 540.0 | 966.0/1002.2 → 404.0/422.4 | 773.0/917.8 → 237.0/237.0 | 1.0 → 2.5 | 875.0 → 773.6 | 1640.8 → 1524.4 | 607.5 → 601.5 |
| grid1m | 2016.0 → 704.0 | 1665.0/1689.2 → 581.0/607.2 | 1463.0/1486.0 → 399.0/442.6 | 0.6 → 1.7 | 1505.8 → 1188.0 | 2095.7 → 2012.0 | 680.4 → 674.4 |
| occluded | 636.0 → 491.0 | 525.0/573.6 → 347.0/385.8 | 309.0/371.4 → 165.0/175.0 | 1.9 → 2.9 | 593.1 → 553.9 | 1212.1 → 1507.0 | 549.6 → 543.7 |
| alpha4 | 611.0 → 600.0 | 469.0/482.2 → 399.0/428.4 | 371.0/391.0 → 235.0/241.6 | 2.1 → 2.5 | 547.4 → 499.0 | 1217.0 → 1150.4 | 534.9 → 529.0 |
| alpha16 | 1738.0 → 1369.0 | 1572.0/1605.6 → 1159.0/1177.6 | 1289.0/1400.4 → 986.0/1025.8 | 0.6 → 0.9 | 915.1 → 841.4 | 1638.2 → 1772.9 | 534.9 → 529.0 |
| alpha64 | 5080.0 → 3561.0 | 4434.0/4611.3 → 3502.0/3594.7 | 4126.0/4349.2 → 3136.0/3385.3 | 0.2 → 0.3 | 2337.6 → 2263.4 | 3126.7 → 3067.8 | 535.0 → 529.0 |
| ssao | 759.0 → 945.0 | 595.0/622.6 → 808.0/838.2 | 376.0/448.6 → 567.0/575.8 | 1.7 → 1.2 | 543.8 → 497.5 | 1109.8 → 1277.7 | 560.0 → 554.0 |
| shadow | 591.0 → 540.0 | 367.0/396.8 → 398.0/421.2 | 144.0/160.2 → 129.0/178.6 | 2.7 → 2.5 | 529.4 → 477.6 | 1101.3 → 1279.0 | 535.6 → 532.6 |
| toon | 625.0 → 676.0 | 417.0/466.6 → 564.0/597.0 | 286.0/417.2 → 268.0/325.2 | 2.4 → 1.8 | 514.9 → 541.0 | 1065.9 → 1137.8 | 535.5 → 532.0 |
| shared_textures | 566.0 → 504.0 | 386.0/458.2 → 337.0/391.4 | 197.0/225.0 → 98.0/121.4 | 2.6 → 3.0 | 604.4 → 504.9 | 1582.4 → 1191.5 | 535.5 → 532.6 |

## Resolution and FSAA sweep

Four workers, five warm samples per setting; reference → final. Shadow settings and quality remain identical within each row.

| Case | Output | FSAA | Warm median / p95 | Native median / p95 | Benchmark RSS |
|---|---|---:|---|---|---:|
| grid100k | 1920×1080 | 1 | 1043.0/1071.4 → 653.0/672.6 | 867.0/941.4 → 281.0/384.0 | 2327.2 → 1967.3 |
| grid100k | 1920×1080 | 2 | 4270.0/5474.8 → 3505.0/4763.4 | 1735.0/1887.0 → 515.0/528.2 | 6540.8 → 6483.3 |
| grid100k | 800×800 | 1 | 610.0/634.2 → 268.0/274.8 | 331.0/344.2 → 86.0/94.0 | 1421.8 → 1205.7 |
| grid100k | 800×800 | 2 | 1227.0/1274.4 → 916.0/1156.6 | 567.0/859.6 → 329.0/341.0 | 2756.4 → 2493.3 |
| small | 1920×1080 | 1 | 835.0/900.0 → 954.0/1065.2 | 323.0/346.0 → 124.0/196.0 | 2174.3 → 2075.9 |
| small | 1920×1080 | 2 | 5165.0/6274.2 → 2813.0/3791.0 | 1456.0/1510.0 → 459.0/746.0 | 6482.6 → 6047.7 |
| small | 800×800 | 1 | 407.0/451.4 → 379.0/429.2 | 168.0/190.8 → 33.0/51.4 | 1191.5 → 1083.3 |
| small | 800×800 | 2 | 1020.0/1155.8 → 941.0/1663.6 | 427.0/458.0 → 172.0/290.6 | 2732.1 → 2448.9 |
| ssao | 1920×1080 | 1 | 2544.0/2628.6 → 1211.0/1435.0 | 1557.0/1715.6 → 377.0/423.8 | 2085.6 → 1901.2 |
| ssao | 1920×1080 | 2 | 9935.0/11182.8 → 5099.0/6211.2 | 6886.0/7113.6 → 2093.0/2150.4 | 7075.0 → 6837.5 |
| ssao | 800×800 | 1 | 773.0/796.0 → 357.0/546.4 | 490.0/504.4 → 147.0/242.0 | 1238.5 → 1041.3 |
| ssao | 800×800 | 2 | 3721.0/3827.6 → 1991.0/2308.0 | 1493.0/2623.8 → 1065.0/1134.0 | 2752.4 → 2313.4 |

## Worker sweep

800×800, FSAA 1. Entries are warm/native medians, reference → final; p95 and memory are in the linked CSVs. Ten physical cores include two efficiency cores.

| Case | 1 worker | 2 workers | 4 workers | 10 workers |
|---|---|---|---|---|
| small | 244.0/67.0 → 215.0/34.0 | 502.0/94.0 → 367.0/74.0 | 407.0/168.0 → 379.0/33.0 | 524.0/188.0 → 368.0/39.0 |
| grid100k | 412.0/232.0 → 278.0/88.0 | 768.0/376.0 → 327.0/106.0 | 610.0/331.0 → 268.0/86.0 | 844.0/250.0 → 422.0/89.0 |
| occluded | 525.0/309.0 → 347.0/165.0 | 687.0/267.0 → 542.0/139.0 | 792.0/425.0 → 501.0/163.0 | 795.0/454.0 → 302.0/123.0 |
| ssao | 595.0/376.0 → 808.0/567.0 | 886.0/664.0 → 486.0/174.0 | 773.0/490.0 → 357.0/147.0 | 1022.0/732.0 → 330.0/180.0 |
| shadow | 367.0/144.0 → 398.0/129.0 | 475.0/170.0 → 281.0/91.0 | 531.0/156.0 → 234.0/66.0 | 545.0/140.0 → 400.0/60.0 |
| toon | 417.0/286.0 → 564.0/268.0 | 664.0/339.0 → 462.0/202.0 | 439.0/219.0 → 556.0/241.0 | 748.0/227.0 → 510.0/261.0 |

## Batch sweep

Final default renderer, four workers, 800×800, FSAA 1, ten warm samples. Coverage blocks stay 4×4. Entries are warm/native median / p95 milliseconds.

| Case | 16 blocks per batch | 64 blocks per batch | 256 blocks per batch |
|---|---|---|---|
| small | 377.5/70.5 med; 429.0/95.4 p95 | 326.0/40.0 med; 448.7/45.7 p95 | 231.0/54.0 med; 400.1/87.6 p95 |
| grid100k | 432.0/80.5 med; 568.6/86.1 p95 | 456.0/139.0 med; 527.8/172.1 p95 | 500.0/83.0 med; 554.1/95.2 p95 |
| occluded | 484.5/168.5 med; 532.1/191.2 p95 | 555.5/173.0 med; 631.6/200.9 p95 | 510.5/107.0 med; 615.8/120.8 p95 |

## Instrumentation cost at checkpoint 3bb9586

Measured checkpoint `3bb9586`, representative one-worker cases. The later profiler-only correction is measured separately in [delivery-diagnostic.md](delivery-diagnostic.md). Uninstrumented and instrumented native medians are separate runs (five versus three samples, except alpha64 three each). This includes counter collection and CSV logging overhead; noise and run order can produce apparent negative overhead. Public speed comparisons above use instrumentation OFF.

| Case | Native, profiler off | Native, profiler on | Relative difference |
|---|---:|---:|---:|
| small | 34.0 | 40.0 | +17.6% |
| grid100k | 88.0 | 90.0 | +2.3% |
| grid500k | 237.0 | 233.0 | -1.7% |
| grid1m | 399.0 | 420.0 | +5.3% |
| occluded | 165.0 | 173.0 | +4.8% |
| alpha4 | 235.0 | 237.0 | +0.9% |
| alpha16 | 986.0 | 975.0 | -1.1% |
| alpha64 | 3136.0 | 3358.0 | +7.1% |
| ssao | 567.0 | 560.0 | -1.2% |
| shadow | 129.0 | 153.0 | +18.6% |
| toon | 268.0 | 265.0 | -1.1% |
| shared_textures | 98.0 | 59.0 | -39.8% |

Phase timings/counters for every final setting: [reference](matrix-reference/phases.csv), [final](matrix-final/phases.csv). Matched image and debug comparisons: [matrix-differences.csv](matrix-differences.csv).
