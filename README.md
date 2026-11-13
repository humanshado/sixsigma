# Six Sigma Studio

A point-and-click desktop-style application for quality engineering, in the
spirit of Minitab's quality tools. It puts a graphical interface on the
open-source [SixSigma](https://cran.r-project.org/package=SixSigma) R package,
so you can run Six Sigma analyses **without writing any R code**.

![Capability analysis](docs/screenshots/capability.png)

## Features

| Menu | Analysis |
|---|---|
| **Data** | Load a sample dataset, upload CSV/Excel, or paste columns from a spreadsheet; edit cells, add columns, and export the worksheet |
| **Stat → Basic statistics** | Descriptive statistics (optionally by group), histogram with normal fit, normal probability plot, Anderson-Darling and Shapiro-Wilk tests, confidence interval for the mean |
| **Stat → Hypothesis tests** | 1-sample, 2-sample (Welch or pooled) and paired t-tests; one-way ANOVA with Tukey comparisons; equal variances (F/Bartlett and Levene); Mann-Whitney and Kruskal-Wallis; 1-proportion (exact) and 2-proportion tests; chi-square test of association. Each shows the hypotheses, a plain-language conclusion and a chart |
| **Stat → Regression** | Simple, quadratic/cubic and multiple linear regression (numeric and categorical predictors, optional interactions): equation, coefficients with VIF, R-sq/adjusted/predicted R-sq, ANOVA, fitted line plot with confidence and prediction bands, four-in-one residual plots, unusual observations, and predictions for new settings |
| **DOE → Create factorial design** | Two-level full and fractional factorial designs for 2 to 8 factors using standard minimum-aberration generators; replicates, center points, randomized run order; resolution, defining relation and alias structure. The design opens in the worksheet, ready for you to type in results |
| **DOE → Analyze factorial design** | Effects and coefficients, Pareto and normal plots of standardized effects, main effects and interaction plots, curvature test from center points, residual plots. Unreplicated designs use Lenth's method; aliased terms are detected and reported |
| **Quality Tools → Capability analysis** | Cp, Cpk (within-subgroup sigma from moving ranges or pooled subgroups) and Pp, Ppk (overall), Cpm (Boyles interval) and Cpmk, all with confidence intervals; nonparametric CNp, CNpk, CNpm, CNpmk; Vännman capability plot; expected and observed PPM; the SixSigma capability report |
| **Quality Tools → Non-normal capability** | Fits normal, lognormal, Weibull, gamma, exponential and logistic distributions or a Box-Cox transformation, ranks them by AIC and Anderson-Darling statistic, and computes percentile-based Pp/Ppk and PPM with histogram and probability plots |
| **Quality Tools → Control charts** | I-MR, Xbar-R, Xbar-S, P, NP, C, U, EWMA and CUSUM charts; subgroups by size or column; variable sample sizes; limits from the first N points (Phase I/II) or a known mean and sigma; run-rule signals |
| **Quality Tools → Multivariate control charts** | Hotelling T² (individuals or subgroups, Phase I/II limits, variable contributions to each signal), MEWMA and MCUSUM; nonparametric data-depth r, Q and S charts (Mahalanobis or spatial depth) that need no normality assumption |
| **Quality Tools → Gage R&R** | Crossed or nested studies with variance components, % study variation, number of distinct categories, ANOVA and the six-panel report |
| **Quality Tools → Loss function** | Taguchi loss function (nominal-, smaller- or larger-is-better) |
| **Quality Tools → Cause-and-effect diagram** | Ishikawa (fishbone) diagram from a simple text list |
| **Quality Tools → Pareto chart** | From typed counts, a category + count column, or one row per defect; cumulative percentage line, "vital few" highlighting and an optional "Other" group |
| **Quality Tools → Process yield** | Yield, first-time yield, rolled throughput yield, DPMO and sigma level |

Every chart can be downloaded as a PNG image.

| Control charts | Gage R&R |
|---|---|
| ![Control charts](docs/screenshots/control-charts.png) | ![Gage R&R](docs/screenshots/gage-rr.png) |
| **Regression** | **Create factorial design** |
| ![Regression](docs/screenshots/regression.png) | ![Create factorial design](docs/screenshots/doe-create.png) |
| **Multivariate control charts** | **Non-normal capability** |
| ![Multivariate control charts](docs/screenshots/multivariate.png) | ![Non-normal capability](docs/screenshots/nonnormal.png) |
| **Analyze factorial design** | **Hypothesis tests** |
| ![Analyze factorial design](docs/screenshots/doe-analyze.png) | ![One-way ANOVA](docs/screenshots/hypothesis-anova.png) |
| **Pareto chart** | |
| ![Pareto chart](docs/screenshots/pareto.png) | |

## Running it

1. Install [R](https://cran.r-project.org/) (version 4.1 or later).
2. Install the required packages (once, and again after updating the app). The
   script also upgrades packages that are too old, such as bslib versions
   before 0.5:

   ```bash
   Rscript install.R
   ```

3. Start the app from this folder:

   ```bash
   Rscript -e 'shiny::runApp(".", launch.browser = TRUE)'
   ```

   It opens in your web browser. In RStudio you can instead open `app.R` and
   click **Run App**.

   If the app reports that packages are outdated, close any running R or
   RStudio sessions, run `Rscript install.R` again, and restart.

## Project layout

```
app.R               Menus and page layout
R/mod_data.R        Worksheet: sample data, upload, paste, export
R/mod_basic.R       Basic statistics
R/mod_hypothesis.R  Hypothesis tests (user interface)
R/hyptests.R        Hypothesis test calculations
R/mod_capability.R  Capability analysis
R/mod_control.R     Control charts (user interface)
R/spc.R             Control chart calculations (Shewhart, EWMA, CUSUM) and run rules
R/mod_multivariate.R Multivariate control charts (user interface)
R/multivariate.R    T², MEWMA, MCUSUM and data-depth r/Q/S charts
R/capability.R      Capability indices, nonparametric indices, Vännman plot
R/mod_nonnormal.R   Non-normal capability (user interface)
R/capability_nonnormal.R Distribution fitting, Box-Cox, percentile indices
data/qcr/           Example datasets from the qcr package (see its README)
R/mod_gage.R        Gage R&R
R/mod_regression.R  Regression (user interface)
R/regression.R      Regression calculations and plots
R/mod_doe.R         DOE: create and analyze designs (user interface)
R/doe.R             Design generation, alias structure, effects analysis
R/mod_pareto.R      Pareto chart
R/mod_tools.R       Loss function, cause-and-effect diagram, yield
R/utils.R           Shared helpers
tests/testthat/     Unit tests for every calculation module
```

## How the analyses are computed

Hypothesis tests use R's standard `stats` functions (`t.test`, `aov`,
`TukeyHSD`, `var.test`, `bartlett.test`, `wilcox.test`, `kruskal.test`,
`binom.test`, `prop.test`, `fisher.test`, `chisq.test`); Levene's test is the
Brown-Forsythe form (ANOVA on absolute deviations from group medians).

Regression uses `lm`; predicted R-sq comes from the PRESS statistic. Factorial
designs use the standard generators from Box, Hunter & Hunter and Montgomery
(the tests check that every design is balanced and orthogonal), and the effects
analysis reproduces Montgomery's unreplicated 2^4 filtration-rate example,
including Lenth's pseudo standard error.

Capability, Gage R&R, loss function, cause-and-effect, yield and confidence
intervals call the SixSigma package directly, so the results are the
package's own.

SixSigma only provides a moving-range chart, so the other control charts are
computed in `R/spc.R` with the standard Shewhart formulas, using SixSigma's
exact d2, d3 and c4 constants. The tests check these limits against the
SixSigma moving-range chart and against the tabulated A2, A3, B4, D3 and D4
constants:

```bash
Rscript -e 'testthat::test_dir("tests/testthat")'
```

Run rules flagged on control charts: a point beyond the control limits; nine
points in a row on one side of the centre line; six points in a row steadily
increasing or decreasing.

### Methods adapted from qcr

EWMA, CUSUM, the multivariate and depth-based charts, the extended capability
indices and non-normal capability follow the
[qcr](https://github.com/mflores72000/qcr) package (Flores, Fernández-Casal,
Naya, Tarrío-Saavedra). They are reimplemented here, so the app needs neither
qcc nor fda.usc, and are checked against textbook examples (Montgomery's
CUSUM and EWMA examples), R's own functions and simulated data. Where this
app differs from qcr:

- MEWMA and MCUSUM on subgroup means use the covariance of the means (S/n).
- The depth S chart is standardized consistently before comparing with -z(α).
- The Cpm confidence interval uses Boyles' degrees of freedom n(1+ξ²)²/(1+2ξ²).
- Data depth offers Mahalanobis and spatial depth (qcr also has Tukey,
  simplicial and random-projection depth through fda.usc).
- qcr's control charts for functional data are not included.

## Credits and licence

Several methods and the example datasets in `data/qcr` come from the qcr
package by Miguel Flores, Rubén Fernández-Casal, Salvador Naya and Javier
Tarrío-Saavedra (GPL-2 or later).

The statistical engine is the SixSigma package by Emilio L. Cano, Javier M.
Moguerza, Mariano Prieto, Andrés Redchuk and contributors, from the books
*Six Sigma with R* and *Quality Control with R*.

This application is licensed under the GNU General Public License, version 2
or later, the same licence as the SixSigma package.
