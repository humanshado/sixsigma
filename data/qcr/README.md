# Datasets from the qcr package

These example datasets come from the R package
[qcr](https://github.com/mflores72000/qcr) (Quality Control Review) by Miguel
Flores, Rubén Fernández-Casal, Salvador Naya and Javier Tarrío-Saavedra,
licensed under the GNU GPL (version 2 or later). Several originate from
D. C. Montgomery, *Introduction to Statistical Quality Control*. They were
converted from `.RData` to CSV; `archery1` was reshaped from a 24 x 2 x 3 array
to one row per arrow, and the `employment` column `occupantion` was renamed
`occupation`.

| File | Description | Suggested use |
|---|---|---|
| pistonrings.csv | Piston ring inside diameters, 40 samples of 5 | Xbar-R/S, EWMA, CUSUM, capability |
| orangejuice.csv | Nonconforming orange juice cans per sample of 50 | P and NP charts |
| circuit.csv | Nonconformities per circuit board inspection unit | C and U charts |
| pcmanufact.csv | Nonconformities in personal computers, samples of 5 | U chart |
| dowel1.csv | Dowel pin diameter and length | Multivariate (T², MEWMA, MCUSUM), depth charts |
| archery1.csv | Arrow positions (x, y), 24 ends of 3 arrows | Multivariate and depth charts with subgroups |
| plates.csv | Vickers hardness, 50 samples of 5 | Capability, non-normal capability |
| oxidation.csv | Oxidation onset temperature, 50 samples of 5 | Capability, non-normal capability |
| counters.csv | Counter error by brand | Hypothesis tests |
| employment.csv | Level of employment by hemisphere | Hypothesis tests |
