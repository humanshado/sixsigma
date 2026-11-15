library(ggplot2)
for (f in c("reliability.R", "sampling.R", "agreement.R", "power.R", "data_tools.R", "fmea.R", "capability_nonnormal.R"))
  source(file.path("..", "..", "R", f), local = TRUE)

bearings <- c(17.88, 28.92, 33.00, 41.52, 42.12, 45.60, 48.48, 51.84, 51.96, 54.12, 55.56, 67.80, 68.64, 68.64,
              68.88, 84.12, 93.12, 98.64, 105.12, 105.84, 127.92, 128.04, 173.40)

test_that("Weibull and lognormal fits reproduce the ball-bearing textbook values", {
  f <- rel_fit(rel_data(bearings), "weibull")
  expect_equal(round(1 / f$sigma, 3), 2.102)
  expect_equal(round(exp(f$mu), 2), 81.88)
  l <- rel_fit(rel_data(bearings), "lognormal")
  expect_equal(round(c(l$mu, l$sigma), 3), c(4.150, 0.522))
  b10 <- rel_percentiles(f, 0.10)
  expect_equal(b10$Life, exp(f$mu) * (-log(0.9))^(f$sigma))
  expect_true(b10$`CI lower` < b10$Life && b10$Life < b10$`CI upper`)
})

test_that("censored fits match direct likelihood maximisation and closed forms", {
  set.seed(1); t <- rweibull(30, 2, 1000); d <- rel_data(pmin(t, 1300), t <= 1300)
  f <- rel_fit(d, "weibull")
  nll <- function(p) -sum(ifelse(d$f, dweibull(d$t, exp(p[1]), exp(p[2]), log = TRUE),
                                 pweibull(d$t, exp(p[1]), exp(p[2]), lower.tail = FALSE, log.p = TRUE)))
  o <- optim(c(0, log(1000)), nll, control = list(reltol = 1e-12))
  expect_equal(c(1 / f$sigma, exp(f$mu)), exp(o$par), tolerance = 1e-4)
  e <- rel_fit(d, "exponential")
  expect_equal(exp(e$mu), sum(d$t) / sum(d$f), tolerance = 1e-6)
  r <- rel_reliability(f, 800)
  expect_equal(r$Reliability, exp(-(800 / exp(f$mu))^(1 / f$sigma)))
  expect_equal(rel_mttf(f)$Estimate, exp(f$mu) * gamma(1 + f$sigma))
  km <- rel_kaplan_meier(d)
  expect_equal(km$Reliability[1], 1 - 1 / 30)
  expect_error(rel_data(c(1, 2, 3), c(TRUE, FALSE, FALSE)), "two failures")
  for (ds in rel_dists) expect_no_error(ggplot2::ggplot_build(rel_probability_plot(rel_fit(d, ds))))
  expect_equal(rel_compare(d)$Code[1], "weibull")
})

test_that("frequencies count as repeated observations", {
  a <- rel_fit(rel_data(c(10, 20, 20, 30)), "weibull")
  b <- rel_fit(rel_data(c(10, 20, 30), freq = c(1, 2, 1)), "weibull")
  expect_equal(c(a$mu, a$sigma), c(b$mu, b$sigma), tolerance = 1e-6)
})

test_that("single and double sampling OC values match Montgomery", {
  expect_equal(round(pa_single(c(0.01, 0.02, 0.03, 0.04, 0.05), 89, 2), 4), c(0.9397, 0.7366, 0.4985, 0.3042, 0.1721))
  expect_equal(round(pa_double(0.05, 50, 1, 4, 100, 3), 4), 0.2904)
  expect_equal(pa_single(0.02, 50, 1, N = 500, model = "hypergeometric"), phyper(1, 10, 490, 50))
  expect_equal(pa_single(0.02, 50, 1, model = "poisson"), ppois(1, 1))
})

test_that("designed attribute plans meet both risks and are minimal", {
  p <- design_attribute(0.01, 0.05, 0.06, 0.10)
  expect_gte(p$pa_aql, 0.95); expect_lte(p$pa_ltpd, 0.10)
  expect_gt(pa_single(0.06, p$n - 1, p$c), 0.10)  # one fewer sample would not protect the consumer
  expect_error(design_attribute(0.05, 0.05, 0.02, 0.1), "AQL < LTPD")
  v <- design_variables(0.01, 0.05, 0.06, 0.10)
  expect_equal(round(v$k, 2), 1.89)
  pa <- pa_variables(c(0.01, 0.06), v$n, v$k)
  expect_gte(pa[1], 0.94); expect_lte(pa[2], 0.11)
  dec <- variables_decision(c(9.9, 10.1, 10, 10.05, 9.95), 5, 1.5, usl = 11)
  expect_true(dec$accept)
})

test_that("Fleiss' kappa reproduces the standard example (0.210)", {
  w <- matrix(c(0, 0, 0, 0, 14, 0, 2, 6, 4, 2, 0, 0, 3, 5, 6, 0, 3, 9, 2, 0, 2, 2, 8, 1, 1, 7, 7, 0, 0, 0,
                3, 2, 6, 3, 0, 2, 5, 3, 2, 2, 6, 5, 2, 1, 0, 0, 2, 2, 3, 7), 10, byrow = TRUE)
  colnames(w) <- 1:5
  expect_equal(round(fleiss_kappa(w)$kappa, 3), 0.210)
  expect_equal(fleiss_kappa(cbind(a = c(3, 0, 3), b = c(0, 3, 0)))$kappa, 1)
})

test_that("attribute agreement counts matches and handles ordinal data", {
  d <- read.csv(file.path("..", "..", "data", "examples", "attribute_agreement.csv"))
  a <- agreement_analysis(d, "Part", "Appraiser", "Result", "Standard")
  amy <- d[d$Appraiser == "Amy", ]
  same <- sum(tapply(amy$Result, amy$Part, function(v) length(unique(v)) == 1))
  expect_equal(a$within$Matched[a$within$Appraiser == "Amy"], same)
  right <- sum(tapply(seq_len(nrow(amy)), amy$Part, function(i) all(amy$Result[i] == amy$Standard[i])))
  expect_equal(a$vs_std$Matched[a$vs_std$Appraiser == "Amy"], right)
  expect_lte(a$all_std$Matched, min(a$vs_std$Matched))
  o <- expand.grid(Sample = 1:8, App = c("A", "B"), Trial = 1:2); o$Score <- ((o$Sample - 1) %% 5) + 1
  k <- agreement_analysis(o, "Sample", "App", "Score", ordinal = TRUE)
  expect_equal(k$kendall$W, rep(1, 3))
  expect_error(agreement_analysis(d[-1, ], "Part", "Appraiser", "Result"), "balanced")
  expect_no_error(ggplot2::ggplot_build(agreement_plot(a)))
})

test_that("sample sizes agree with R's power functions", {
  expect_equal(power_solve("two_t", power = 0.8, delta = 1, sd = 1)$n, ceiling(power.t.test(delta = 1, power = 0.8)$n))
  expect_equal(power_solve("one_t", power = 0.9, delta = 0.5, sd = 1)$n,
               ceiling(power.t.test(delta = 0.5, power = 0.9, type = "one.sample")$n))
  expect_equal(power_solve("two_prop", power = 0.8, delta = 0.1, p0 = 0.5)$n, ceiling(power.prop.test(p1 = 0.5, p2 = 0.6, power = 0.8)$n))
  expect_equal(power_solve("two_t", n = 20, power = 0.8, sd = 1)$delta, power.t.test(n = 20, power = 0.8)$delta, tolerance = 1e-4)
  expect_equal(power_solve("anova", power = 0.8, delta = 1, sd = 1, groups = 3)$n, 21)
  r <- power_solve("factorial", power = 0.8, delta = 2, sd = 1, k = 3, runs = 8)
  expect_gte(r$power, 0.8)
  expect_error(power_solve("two_t", n = 10, power = 0.8, delta = 1), "exactly one")
})

test_that("data tools transform correctly and formulas are restricted", {
  d <- data.frame(a = 1:4, b = c(2, 4, 6, NA), g = c("x", "y", "x", "y"))
  expect_equal(nrow(dt_stack(d, c("a", "b"), keep = "g")), 8)
  expect_equal(names(dt_unstack(d, "a", "g")), c("a_x", "a_y"))
  expect_equal(dt_subset(d, "a", ">", "2")$a, 3:4)
  expect_equal(dt_sort(d, "a", TRUE)$a, 4:1)
  expect_equal(dt_calc(d, "z", "(a - mean(a)) / sd(a)")$z, as.numeric(scale(1:4)))
  for (bad in c("system('ls')", "get('a')", "a[1]", "eval(a)", "Sys.getenv()", "unknown_col"))
    expect_error(dt_calc(d, "x", bad))
  expect_equal(dt_recode(d, "g", "x = 1\ny = 2")$g, c(1, 2, 1, 2))
  expect_error(dt_convert(d, "g", "numeric"), "not numbers")
  expect_equal(nrow(dt_drop_missing(d)), 3)
  expect_error(dt_rename(d, "a", "b"), "already exists")
})

test_that("FMEA computes RPN, criticality and priorities", {
  r <- fmea_compute(fmea_example(), threshold = 100, sev_flag = 9)
  expect_equal(r$RPN, r$S * r$O * r$D)
  expect_equal(r$Priority[r$Failure == "Oversize hole missed"], "High (severity)")
  expect_true(all(r$Priority[!is.na(r$RPN) & r$RPN >= 100 & r$S < 9] == "High (RPN)"))
  bad <- fmea_example(); bad$S[1] <- 12
  expect_true(is.na(fmea_compute(bad)$RPN[1]))
  expect_no_error(ggplot2::ggplot_build(fmea_pareto(r)))
  expect_no_error(ggplot2::ggplot_build(fmea_matrix(r)))
})

test_that("non-normal capability fits large-scale data (MASS optimizer workaround)", {
  set.seed(1); x <- rweibull(300, 2, 1000)
  f <- nn_fit(x, "weibull")
  expect_equal(unname(f$par["scale"]), 1000, tolerance = 0.05)
  expect_true(is.finite(nn_fit(rgamma(200, 3, 0.002), "gamma")$aic))
})

test_that("every distribution fits heavily censored field data", {
  d <- read.csv(file.path("..", "..", "data", "examples", "pump_field_failures.csv"))
  dd <- rel_data(d$Hours, d$Status == "Failed")
  expect_gt(mean(!dd$f), 0.5)
  for (ds in rel_dists) expect_no_error(rel_fit(dd, ds))
  f <- rel_fit(dd, "normal")
  nll <- function(p) -sum(ifelse(dd$f, dnorm(dd$t, p[1], exp(p[2]), log = TRUE),
                                 pnorm(dd$t, p[1], exp(p[2]), lower.tail = FALSE, log.p = TRUE)))
  o <- optim(c(5000, log(3000)), nll, control = list(maxit = 5000, reltol = 1e-14))
  expect_equal(c(f$mu, f$sigma), c(o$par[1], exp(o$par[2])), tolerance = 1e-4)
})

test_that("optional column pickers use a non-empty 'none' value", {
  source(file.path("..", "..", "R", "utils.R"), local = TRUE)
  expect_null(empty_to_null(PICKER_NONE)); expect_null(empty_to_null("")); expect_equal(empty_to_null("x"), "x")
  expect_true(nzchar(PICKER_NONE))
})
