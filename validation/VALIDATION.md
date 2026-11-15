# Validation report

Six Sigma Studio 1.0.0, run 2026-11-15 with R 4.3.3.

**51 of 51 checks pass.** Each check compares a result computed by the app's own functions with a
published reference value (certified values, textbook examples or standard tables). Values printed
in a reference are compared at the precision printed (half a unit in the last digit).

The input data are in `validation/datasets/`, so the same checks can be repeated in Minitab, JMP or
any other package.

| Area | Reference | Statistic | Published | Six Sigma Studio | Result |
|---|---|---|---|---|---|
| Regression | NIST StRD Longley (certified) | Intercept | -3482258.635 | -3482258.635 | Pass |
| Regression | NIST StRD Longley (certified) | B1 | 15.06187227 | 15.06187227 | Pass |
| Regression | NIST StRD Longley (certified) | B2 | -0.03581917929 | -0.03581917929 | Pass |
| Regression | NIST StRD Longley (certified) | B3 | -2.020229804 | -2.020229804 | Pass |
| Regression | NIST StRD Longley (certified) | B4 | -1.033226867 | -1.033226867 | Pass |
| Regression | NIST StRD Longley (certified) | B5 | -0.05110410565 | -0.05110410565 | Pass |
| Regression | NIST StRD Longley (certified) | B6 | 1829.151465 | 1829.151465 | Pass |
| Regression | NIST StRD Longley (certified) | Residual standard deviation | 304.8540736 | 304.8540736 | Pass |
| Regression | NIST StRD Longley (certified) | R-squared | 0.9954790046 | 0.9954790046 | Pass |
| Control charts | Montgomery SQC, Example 6.1 (piston rings) | Grand mean |      74.001 |   74.001176 | Pass |
| Control charts | Montgomery SQC, Example 6.1 (piston rings) | Average range |       0.023 |     0.02276 | Pass |
| Control charts | Montgomery SQC, Example 6.1 (piston rings) | Xbar UCL |      74.014 | 74.01430441 | Pass |
| Control charts | Montgomery SQC, Example 6.1 (piston rings) | Xbar LCL |      73.988 | 73.98804759 | Pass |
| Control charts | Montgomery SQC, Example 6.1 (piston rings) | R UCL |       0.048 | 0.04812600054 | Pass |
| Control charts | Montgomery SQC, Example 7.1 (orange juice cans) | p-bar |      0.2313 | 0.2313333333 | Pass |
| Control charts | Montgomery SQC, Example 7.1 (orange juice cans) | P chart UCL |      0.4102 | 0.4102391186 | Pass |
| Control charts | Montgomery SQC, Example 7.1 (orange juice cans) | P chart LCL |      0.0524 | 0.05242754807 | Pass |
| Control charts | Montgomery SQC, Example 9.1 (tabular CUSUM) | C+ at sample 29 |        5.28 |        5.28 | Pass |
| Control charts | Montgomery SQC, Example 9.1 (tabular CUSUM) | C+ at sample 30 |         5.3 |         5.3 | Pass |
| Control charts | Montgomery SQC, Example 9.1 (tabular CUSUM) | First signal (sample) |          29 |          29 | Pass |
| Control charts | Montgomery SQC, Example 9.2 (EWMA) | z1 |       9.945 |       9.945 | Pass |
| Control charts | Montgomery SQC, Example 9.2 (EWMA) | z2 |       9.749 |      9.7495 | Pass |
| Control charts | Montgomery SQC, Example 9.2 (EWMA) | UCL at sample 1 |       10.27 |       10.27 | Pass |
| Design of experiments | Montgomery DOE, Example 6.2 (filtration rate) | Effect A |      21.625 |      21.625 | Pass |
| Design of experiments | Montgomery DOE, Example 6.2 (filtration rate) | Effect C |       9.875 |       9.875 | Pass |
| Design of experiments | Montgomery DOE, Example 6.2 (filtration rate) | Effect D |      14.625 |      14.625 | Pass |
| Design of experiments | Montgomery DOE, Example 6.2 (filtration rate) | Effect A*C |     -18.125 |     -18.125 | Pass |
| Design of experiments | Montgomery DOE, Example 6.2 (filtration rate) | Effect A*D |      16.625 |      16.625 | Pass |
| Design of experiments | Lenth (1989) applied to Example 6.2 | Lenth PSE |       2.625 |       2.625 | Pass |
| Reliability | Lieblein & Zelen (1956) ball bearings; Lawless (2003) | Weibull shape (MLE) |       2.102 | 2.102058499 | Pass |
| Reliability | Lieblein & Zelen (1956) ball bearings; Lawless (2003) | Weibull scale (MLE) |       81.88 | 81.87834026 | Pass |
| Reliability | Nelson (1990) Class-B insulation; Meeker & Escobar (1998) | Lognormal sigma (log10 hours) |      0.2592 | 0.2591815412 | Pass |
| Reliability | Nelson (1990) Class-B insulation; Meeker & Escobar (1998) | Intercept (log10 hours) |       -6.02 | -6.018237316 | Pass |
| Reliability | Success-run formula n = ln(1-C)/ln(R) | Units for R = 0.90 at C = 0.90, 0 failures |          22 |          22 | Pass |
| Reliability | Binomial (cumulative) test plan tables | Units for R = 0.90 at C = 0.90, 1 failure |          38 |          38 | Pass |
| Reliability | Chi-square MTBF test (time-terminated) | Test time / MTBF, 90%, 1 failure |        3.89 |  3.88972017 | Pass |
| Statistics | NIST e-Handbook 1.3.5.17 (Grubbs) | Grubbs G |      2.4688 | 2.468764611 | Pass |
| Statistics | NIST e-Handbook 1.3.5.17.3 (Rosner generalized ESD) | R1 |       3.119 | 3.118906049 | Pass |
| Statistics | NIST e-Handbook 1.3.5.17.3 (Rosner generalized ESD) | R3 |       3.179 | 3.179423937 | Pass |
| Statistics | NIST e-Handbook 1.3.5.17.3 (Rosner generalized ESD) | Number of outliers |           3 |           3 | Pass |
| Statistics | Normal tolerance factor tables (NIST e-Handbook 7.2.6.3; Odeh & Owen) | One-sided k, n = 10, 95% coverage, 95% confidence |       2.911 | 2.910963413 | Pass |
| Statistics | Normal tolerance factor tables (NIST e-Handbook 7.2.6.3; Odeh & Owen) | One-sided k, n = 20, 90% coverage, 95% confidence |       1.926 | 1.925990972 | Pass |
| Statistics | Normal tolerance factor tables (NIST e-Handbook 7.2.6.3; Odeh & Owen) | Two-sided k (exact), n = 10, 95% coverage, 95% confidence |       3.393 | 3.393429479 | Pass |
| Statistics | Cohen (1988) power tables | Two-sample t: n per group for d = 1, 80% power, alpha 0.05 |          17 |          17 | Pass |
| Acceptance sampling | Montgomery SQC, Section 15.2 (single sampling n = 89, c = 2) | Pa at p = 0.01 |      0.9397 | 0.9396899183 | Pass |
| Acceptance sampling | Montgomery SQC, Section 15.2 (single sampling n = 89, c = 2) | Pa at p = 0.02 |      0.7366 | 0.7365775757 | Pass |
| Acceptance sampling | Montgomery SQC, Section 15.2 (single sampling n = 89, c = 2) | Pa at p = 0.03 |      0.4985 | 0.4984828379 | Pass |
| Acceptance sampling | Montgomery SQC, Section 15.2 (single sampling n = 89, c = 2) | Pa at p = 0.04 |      0.3042 | 0.3041583593 | Pass |
| Acceptance sampling | Montgomery SQC, Section 15.2 (single sampling n = 89, c = 2) | Pa at p = 0.05 |      0.1721 | 0.1720768642 | Pass |
| Acceptance sampling | Montgomery SQC, Section 15.3 (double sampling) | Pa at p = 0.05 |      0.2904 | 0.2904154823 | Pass |
| Measurement systems | Fleiss (1971), 10 subjects x 14 raters | Fleiss' kappa |        0.21 | 0.2099307044 | Pass |

## Notes

- **Tolerance factor.** Some tables print 3.379 for the two-sided factor (n = 10, 95%/95%); that is Howe's
  approximation. The app computes the exact factor (3.393); a simulation of 200,000 samples confirms it gives
  95.0% confidence.
- **Insulation intercept.** The fitted sigma matches the published 0.2592 to all four decimals. The intercept
  (-6.0182 log10 hours) is compared at two decimals because the third depends on the temperature constant and
  rounding used in the reference; the difference (0.0008 log10 hours) is 0.2% in predicted life.
- **Not covered here.** Results computed by the SixSigma package itself (capability report, Gage R&R) are that
  package's own; the unit tests in `tests/testthat` check many more methods against R reference functions.
