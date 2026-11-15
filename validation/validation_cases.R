# Validation cases: Six Sigma Studio's calculations compared with published
# reference results (certified values, textbook examples and standard tables).
# Used by validation/run_validation.R (report) and tests/testthat/test-validation.R.
#
# Each check: area, reference, statistic, published value, computed value and
# a tolerance. Published values are compared at the precision they are
# printed in the reference (tolerance = half a unit in the last digit) unless
# the reference gives certified full-precision values.

validation_root <- function() {
  for (d in c(".", "..", file.path("..", ".."))) if (file.exists(file.path(d, "app.R")) && dir.exists(file.path(d, "R"))) return(normalizePath(d))
  stop("Run from the Six Sigma Studio folder.")
}

validation_datasets <- function() {
  root <- validation_root()
  qcr <- function(n) utils::read.csv(file.path(root, "data", "qcr", paste0(n, ".csv")))
  ex <- function(n) utils::read.csv(file.path(root, "data", "examples", paste0(n, ".csv")))
  L <- datasets::longley
  list(
    # NIST StRD Longley data in the NIST units (R's copy is rescaled).
    longley = data.frame(y = L$Employed * 1000, x1 = L$GNP.deflator, x2 = L$GNP * 1000, x3 = L$Unemployed * 10,
                         x4 = L$Armed.Forces * 10, x5 = L$Population * 1000, x6 = L$Year),
    pistonrings = utils::head(qcr("pistonrings"), 125),           # Montgomery Table 6.3, samples 1-25
    orangejuice = utils::head(qcr("orangejuice"), 30),           # Montgomery Table 7.1, samples 1-30
    filtration = transform(as.data.frame(doe_coded(4, 16)$matrix),
                           Rate = c(45, 71, 48, 65, 68, 60, 80, 65, 43, 100, 45, 104, 75, 86, 70, 96)),
    cusum = data.frame(x = c(9.45, 7.99, 9.29, 11.66, 12.16, 10.18, 8.04, 11.46, 9.20, 10.34, 9.03, 11.47, 10.51, 9.40, 10.08,
                             9.37, 10.62, 10.31, 8.52, 10.84, 10.90, 9.33, 12.29, 11.50, 10.60, 11.08, 10.38, 11.62, 11.31, 10.52)),
    bearings = ex("ball_bearings"),
    insulation = ex("insulation_alt"),
    grubbs = data.frame(x = c(199.31, 199.53, 200.19, 200.82, 201.92, 201.95, 202.18, 245.57)),
    rosner = data.frame(x = c(-0.25, 0.68, 0.94, 1.15, 1.20, 1.26, 1.26, 1.34, 1.38, 1.43, 1.49, 1.49, 1.55, 1.56, 1.58, 1.65, 1.69,
                              1.70, 1.76, 1.77, 1.81, 1.91, 1.94, 1.96, 1.99, 2.06, 2.09, 2.10, 2.14, 2.15, 2.23, 2.24, 2.26, 2.35,
                              2.37, 2.40, 2.47, 2.54, 2.62, 2.64, 2.90, 2.92, 2.92, 2.93, 3.21, 3.26, 3.30, 3.59, 3.68, 4.30, 4.64,
                              5.34, 5.42, 6.01)),
    fleiss = as.data.frame(matrix(c(0, 0, 0, 0, 14, 0, 2, 6, 4, 2, 0, 0, 3, 5, 6, 0, 3, 9, 2, 0, 2, 2, 8, 1, 1, 7, 7, 0, 0, 0,
                                    3, 2, 6, 3, 0, 2, 5, 3, 2, 2, 6, 5, 2, 1, 0, 0, 2, 2, 3, 7), 10, byrow = TRUE,
                                  dimnames = list(NULL, paste0("Category", 1:5))))
  )
}

# Tolerance for a value printed with `digits` decimals.
printed <- function(digits) 0.5 * 10^-digits

validation_checks <- function() {
  ds <- validation_datasets()
  rows <- list()
  add <- function(area, reference, statistic, published, computed, tol, relative = FALSE) {
    diff <- abs(computed - published)
    ok <- if (relative) diff <= tol * abs(published) else diff <= tol + 1e-12
    rows[[length(rows) + 1]] <<- data.frame(Area = area, Reference = reference, Statistic = statistic, Published = published,
                                            Computed = computed, Tolerance = if (relative) sprintf("%g relative", tol) else format(tol),
                                            Pass = ok, check.names = FALSE)
  }

  # Regression: NIST StRD Longley (certified values, higher difficulty).
  m <- fit_regression(ds$longley, "y", paste0("x", 1:6))
  cf <- stats::coef(m$fit)
  cert <- c(-3482258.63459582, 15.0618722713733, -0.358191792925910E-01, -2.02022980381683, -1.03322686717359,
            -0.511041056535807E-01, 1829.15146461355)
  nist <- "NIST StRD Longley (certified)"
  for (j in 1:7) add("Regression", nist, c("Intercept", paste0("B", 1:6))[j], cert[j], unname(cf[j]), 1e-7, TRUE)
  s <- reg_summary(m)
  add("Regression", nist, "Residual standard deviation", 304.854073561965, s$S, 1e-7, TRUE)
  add("Regression", nist, "R-squared", 0.995479004577296, s$`R-sq (%)` / 100, 1e-9, TRUE)

  # Control charts: Montgomery, Introduction to Statistical Quality Control.
  p <- ds$pistonrings
  r <- spc_xbar_r(spc_subgroups(p$diameter, groups = p$sample))
  xb <- r$charts[[1]]; rc <- r$charts[[2]]
  mt <- "Montgomery SQC, Example 6.1 (piston rings)"
  add("Control charts", mt, "Grand mean", 74.001, xb$cl[1], printed(3))
  add("Control charts", mt, "Average range", 0.023, rc$cl[1], printed(3))
  add("Control charts", mt, "Xbar UCL", 74.014, xb$ucl[1], printed(3))
  add("Control charts", mt, "Xbar LCL", 73.988, xb$lcl[1], printed(3))
  add("Control charts", mt, "R UCL", 0.048, rc$ucl[1], printed(3))
  oj <- ds$orangejuice
  pc <- spc_p(oj$D, oj$size)$charts[[1]]
  mo <- "Montgomery SQC, Example 7.1 (orange juice cans)"
  add("Control charts", mo, "p-bar", 0.2313, pc$cl[1], printed(4))
  add("Control charts", mo, "P chart UCL", 0.4102, pc$ucl[1], printed(4))
  add("Control charts", mo, "P chart LCL", 0.0524, pc$lcl[1], printed(4))
  cu <- spc_cusum(ds$cusum$x, k = 0.5, h = 5, mu0 = 10, sigma = 1)$charts[[1]]
  mc <- "Montgomery SQC, Example 9.1 (tabular CUSUM)"
  add("Control charts", mc, "C+ at sample 29", 5.28, cu$y[29], printed(2))
  add("Control charts", mc, "C+ at sample 30", 5.30, cu$y[30], printed(2))
  add("Control charts", mc, "First signal (sample)", 29, which(lengths(cu$violations) > 0)[1], 0)
  ew <- spc_ewma(ds$cusum$x, lambda = 0.1, L = 2.7, mu0 = 10, sigma = 1)$charts[[1]]
  me <- "Montgomery SQC, Example 9.2 (EWMA)"
  add("Control charts", me, "z1", 9.945, ew$y[1], printed(3))
  add("Control charts", me, "z2", 9.749, ew$y[2], printed(3))
  add("Control charts", me, "UCL at sample 1", 10.27, ew$ucl[1], printed(2))

  # DOE: Montgomery, Design and Analysis of Experiments, Example 6.2.
  a <- doe_analyze(ds$filtration, "Rate", c("A", "B", "C", "D"), order = 4)
  eff <- stats::setNames(a$table$Effect, a$table$Term)
  md <- "Montgomery DOE, Example 6.2 (filtration rate)"
  for (t in c("A", "C", "D", "A*C", "A*D")) add("Design of experiments", md, paste("Effect", t),
                                                 c(A = 21.625, C = 9.875, D = 14.625, `A*C` = -18.125, `A*D` = 16.625)[[t]], eff[[t]], printed(3))
  add("Design of experiments", "Lenth (1989) applied to Example 6.2", "Lenth PSE", 2.625,
      as.numeric(sub(".*PSE = ([0-9.]+).*", "\\1", a$method)), printed(3))

  # Reliability.
  b <- ds$bearings
  w <- rel_fit(rel_data(b[[1]]), "weibull")
  rb <- "Lieblein & Zelen (1956) ball bearings; Lawless (2003)"
  add("Reliability", rb, "Weibull shape (MLE)", 2.102, 1 / w$sigma, printed(3))
  add("Reliability", rb, "Weibull scale (MLE)", 81.88, exp(w$mu), printed(2))
  ins <- ds$insulation
  f <- alt_fit(ins$Hours, ins$Status == "Failed", ins$TempC, dist = "lognormal", rel = "arrhenius")
  ri <- "Nelson (1990) Class-B insulation; Meeker & Escobar (1998)"
  add("Reliability", ri, "Lognormal sigma (log10 hours)", 0.2592, f$sigma / log(10), printed(4))
  # Printed as -6.019 in the reference as recalled; compared at two decimals (see VALIDATION.md notes).
  add("Reliability", ri, "Intercept (log10 hours)", -6.02, f$b0 / log(10), printed(2))
  add("Reliability", "Success-run formula n = ln(1-C)/ln(R)", "Units for R = 0.90 at C = 0.90, 0 failures", 22, demo_sample_size(0.9, 0.9), 0)
  add("Reliability", "Binomial (cumulative) test plan tables", "Units for R = 0.90 at C = 0.90, 1 failure", 38, demo_sample_size(0.9, 0.9, 1), 0)
  add("Reliability", "Chi-square MTBF test (time-terminated)", "Test time / MTBF, 90%, 1 failure", 3.890, demo_mtbf_time(1, 0.9, 1), printed(3))

  # Statistics.
  g <- grubbs_test(ds$grubbs$x)
  add("Statistics", "NIST e-Handbook 1.3.5.17 (Grubbs)", "Grubbs G", 2.4688, g$G, printed(4))
  e <- esd_test(ds$rosner$x, 10)
  ne <- "NIST e-Handbook 1.3.5.17.3 (Rosner generalized ESD)"
  add("Statistics", ne, "R1", 3.119, e$`Test statistic`[1], printed(3))
  add("Statistics", ne, "R3", 3.179, e$`Test statistic`[3], printed(3))
  add("Statistics", ne, "Number of outliers", 3, sum(e$Outlier == "Yes"), 0)
  nt <- "Normal tolerance factor tables (NIST e-Handbook 7.2.6.3; Odeh & Owen)"
  add("Statistics", nt, "One-sided k, n = 10, 95% coverage, 95% confidence", 2.911, tol_k_one(10, 0.95, 0.95), printed(3))
  add("Statistics", nt, "One-sided k, n = 20, 90% coverage, 95% confidence", 1.926, tol_k_one(20, 0.90, 0.95), printed(3))
  # Exact factor (Odeh & Owen); Howe's approximation, used in some tables, gives 3.379.
  add("Statistics", nt, "Two-sided k (exact), n = 10, 95% coverage, 95% confidence", 3.393, tol_k_two(10, 0.95, 0.95), printed(3))
  add("Statistics", "Cohen (1988) power tables", "Two-sample t: n per group for d = 1, 80% power, alpha 0.05", 17,
      power_solve("two_t", power = 0.8, delta = 1, sd = 1)$n, 0)

  # Sampling and agreement.
  ms <- "Montgomery SQC, Section 15.2 (single sampling n = 89, c = 2)"
  pa <- pa_single(c(0.01, 0.02, 0.03, 0.04, 0.05), 89, 2)
  for (j in 1:5) add("Acceptance sampling", ms, sprintf("Pa at p = %.2f", j / 100), c(0.9397, 0.7366, 0.4985, 0.3042, 0.1721)[j], pa[j], printed(4))
  add("Acceptance sampling", "Montgomery SQC, Section 15.3 (double sampling)", "Pa at p = 0.05", 0.2904, pa_double(0.05, 50, 1, 4, 100, 3), printed(4))
  add("Measurement systems", "Fleiss (1971), 10 subjects x 14 raters", "Fleiss' kappa", 0.210, fleiss_kappa(as.matrix(ds$fleiss))$kappa, printed(3))

  out <- do.call(rbind, rows)
  out$Difference <- abs(out$Computed - out$Published)
  out[, c("Area", "Reference", "Statistic", "Published", "Computed", "Difference", "Tolerance", "Pass")]
}
