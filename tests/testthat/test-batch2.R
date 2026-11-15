library(ggplot2)
for (f in c("spc.R", "reliability.R", "spc_more.R", "capability.R", "capability_more.R", "regression.R", "regression_more.R",
            "anova_general.R", "gage_expanded.R", "stats_more.R"))
  source(file.path("..", "..", "R", f), local = TRUE)

test_that("each Nelson test fires at the end of its pattern", {
  hits <- function(y, rule) which(vapply(spc_rules(y, 0, -3, 3), function(r) rule %in% r, logical(1)))
  expect_equal(hits(c(0, 0, 4), 1), 3)
  expect_equal(hits(c(rep(-0.1, 3), rep(0.5, 9)), 2), 12)
  expect_equal(hits(c(0, 5, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), 3)[1], 8)
  expect_equal(hits(c(0, rep(c(0.5, -0.5), 7)), 4)[1], 14)
  expect_equal(hits(c(0, 0, 2.5, 0, 2.2), 5), 5)
  expect_equal(hits(c(0, 1.5, 1.2, 0.2, 1.1, 1.3), 6), 6)
  expect_equal(hits(c(2, rep(c(0.3, -0.3), 8)), 7)[1], 16)
  expect_equal(hits(c(0, rep(c(1.5, -1.5), 4)), 8), 9)
  ch <- apply_rules(new_chart("t", "y", c(0, rep(c(1.5, -1.5), 4)), 0, -3, 3), 1:3)
  expect_true(all(lengths(ch$violations) == 0))
})

test_that("historical limits replace the estimates", {
  x <- c(10, 11, 9, 10.5, 9.5, 10.2)
  r <- spc_imr(x, hist = list(mu = 12, sigma = 0.5))
  expect_equal(c(r$charts[[1]]$lcl[1], r$charts[[1]]$cl[1], r$charts[[1]]$ucl[1]), c(10.5, 12, 13.5))
  p <- spc_p(c(2, 3, 1), c(50, 50, 50), hist = list(p = 0.1))
  expect_equal(p$charts[[1]]$ucl[1], 0.1 + 3 * sqrt(0.1 * 0.9 / 50))
})

test_that("Laney charts scale the limits by sigma Z", {
  set.seed(1); n <- rep(5000, 30); d <- rbinom(30, 5000, pmax(0.001, 0.02 + rnorm(30, 0, 0.004)))
  l <- spc_laney(d, n, "p"); pbar <- sum(d) / sum(n)
  z <- (d / n - pbar) / sqrt(pbar * (1 - pbar) / n)
  sz <- mean(abs(diff(z))) / spc_d2(2)
  expect_equal(l$sigma_z, sz)
  expect_equal(l$charts[[1]]$ucl[1], pbar + 3 * sqrt(pbar * (1 - pbar) / 5000) * sz)
  expect_gt(sz, 1)
})

test_that("G, T, Z-MR, I-MR-R and staged charts", {
  g <- spc_g(c(30, 45, 12, 60, 25, 80, 3, 40)); p <- 1 / (mean(c(30, 45, 12, 60, 25, 80, 3, 40)) + 1)
  expect_equal(g$charts[[1]]$ucl[1], qgeom(1 - 0.00135, p))
  t <- spc_t(c(30, 45, 12, 60, 25, 80, 3, 40))
  expect_lt(t$charts[[1]]$lcl[1], t$charts[[1]]$cl[1])
  z <- spc_zmr(c(10, 10.2, 9.9, 50, 50.5, 49.8, 10.1, 50.2), c("A", "A", "A", "B", "B", "B", "A", "B"))
  expect_equal(z$parts$Mean, c(10.05, 50.125))
  expect_equal(z$charts[[1]]$ucl[1], 3)
  set.seed(2); sg <- split(rnorm(60, rep(rnorm(20, 0, 2), each = 3)), rep(1:20, each = 3))
  ir <- spc_imr_rs(sg)
  means <- vapply(sg, mean, 0)
  expect_equal(ir$sigma, mean(abs(diff(means))) / spc_d2(2))
  expect_length(ir$charts, 3)
  x <- c(rnorm(20), rnorm(20, 5)); st <- rep(c("Before", "After"), each = 20)
  r <- spc_by_stage(st, function(i) spc_imr(x[i]))
  expect_equal(r$charts[[1]]$cl[c(1, 40)], c(mean(x[1:20]), mean(x[21:40])))
  expect_equal(r$charts[[1]]$stage_breaks, c(20, 40))
  expect_no_error(ggplot2::ggplot_build(plot_chart(r$charts[[1]])))
})

test_that("between/within, sixpack, attribute and multivariate capability", {
  p <- read.csv(file.path("..", "..", "data", "qcr", "pistonrings.csv"))
  bw <- cap_between_within(p$diameter, p$sample)
  expect_equal(bw$total, sqrt(bw$within^2 + bw$between^2))
  s <- cap_sixpack(p$diameter, p$sample, 73.95, 74.05)
  expect_length(s$plots, 6); for (g in s$plots) expect_no_error(ggplot2::ggplot_build(g))
  b <- cap_binomial(c(5, 3, 4), c(100, 100, 100))
  expect_equal(b$summary$Value[4], 4)
  expect_equal(b$summary$Value[5:6], 100 * binom.test(12, 300)$conf.int[1:2])
  pz <- cap_poisson(c(2, 3, 5), c(10, 10, 10))
  expect_equal(pz$summary$Value[4], 1 / 3)
  d <- read.csv(file.path("..", "..", "data", "qcr", "dowel1.csv"))
  m <- cap_multivariate(d, c(0.47, 0.95), c(0.53, 1.05))
  S <- cov(d); half <- sqrt(qchisq(0.9973, 2) * diag(S))
  expect_equal(m$vector$Value[1], sqrt(prod(c(0.06, 0.1)) / prod(2 * half)))
  expect_no_error(ggplot2::ggplot_build(cap_multivariate_plot(m)))
})

test_that("binary logistic matches glm; stepwise and best subsets match known results", {
  m <- logit_fit(mtcars, "am", c("hp", "wt"))
  expect_equal(unname(coef(m$fit)), unname(coef(glm(am ~ hp + wt, binomial, mtcars))))
  co <- logit_coefficients(m)
  expect_equal(co$`Odds ratio`[2], exp(coef(m$fit)[[2]]))
  expect_lt(logit_summary(m)$fit$Value[6], 1)
  expect_error(logit_fit(mtcars, "gear", "hp"), "exactly two")
  o <- ordinal_fit(mtcars, "gear", "mpg")
  expect_equal(unname(coef(o$fit)), unname(coef(MASS::polr(factor(gear, ordered = TRUE) ~ mpg, mtcars))))
  st <- stepwise_reg(mtcars, "mpg", c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"))
  expect_equal(st$selected, c("wt", "cyl", "hp"))
  bs <- best_subsets(mtcars, "mpg", c("cyl", "disp", "hp", "wt", "qsec", "am"), best = 1)
  expect_equal(round(bs$`R-sq (%)`[1:3], 2), c(75.28, 83.02, 84.97))
  expect_equal(bs$wt[1], "X"); expect_equal(c(bs$wt[3], bs$qsec[3], bs$am[3]), c("X", "X", "X"))
})

test_that("general ANOVA: EMS tests, nested designs and adjusted SS", {
  r <- SixSigma::ss.data.rr
  g <- glm_anova(r, "time1", c("prototype", "operator"), random = c("prototype", "operator"))
  ref <- summary(aov(time1 ~ prototype * operator, r))[[1]]
  expect_equal(g$table$F[1], ref$`Mean Sq`[1] / ref$`Mean Sq`[3])
  expect_equal(g$table$Denominator[1], "prototype*operator")
  set.seed(3); nd <- expand.grid(rep = 1:3, batch = 1:4, supplier = c("S1", "S2", "S3"))
  nd$batch <- paste0(nd$supplier, "-", nd$batch)
  nd$purity <- 90 + as.integer(factor(nd$supplier)) + rnorm(12)[as.integer(factor(nd$batch))] + rnorm(36, 0, 0.5)
  gn <- glm_anova(nd, "purity", c("supplier", "batch"), random = "batch", nested = list(batch = "supplier"))
  strata <- summary(aov(purity ~ supplier + Error(batch), nd))
  expect_equal(gn$table$F[1], strata$`Error: batch`[[1]]$`F value`[1], tolerance = 1e-8)
  fx <- glm_anova(mtcars, "mpg", c("cyl", "am"), covariates = "wt")
  ref2 <- drop1(lm(mpg ~ factor(cyl) * factor(am) + wt, mtcars, contrasts = list(`factor(cyl)` = "contr.sum", `factor(am)` = "contr.sum")), ~ ., test = "F")
  expect_equal(fx$table$`Adj SS`[fx$table$Source == "wt"], ref2["wt", "Sum of Sq"])
  expect_equal(fx$table$`Adj SS`[fx$table$Source == "cyl*am"], ref2["factor(cyl):factor(am)", "Sum of Sq"])
})

test_that("REML and expanded Gage R&R reproduce SixSigma's variance components", {
  r <- SixSigma::ss.data.rr
  g <- gage_expanded(r, "time1", "prototype", "operator", interactions = FALSE)
  grDevices::pdf(NULL); s <- SixSigma::ss.rr(time1, prototype, operator, data = r, print_plot = FALSE); grDevices::dev.off()
  ref <- s$varComp[, 1]
  expect_equal(g$table$`Variance component`[c(1, 2, 3, 5, 6)], unname(ref[c(1, 2, 3, 5, 6)]), tolerance = 1e-5)
  expect_equal(g$ndc, s$ncat)
  u <- gage_expanded(r[-c(2, 11, 20), ], "time1", "prototype", "operator")
  expect_false(u$balanced)
  expect_true(all(u$table$`Variance component` >= 0))
})

test_that("tolerance factors, TOST, correlation and outlier tests", {
  expect_equal(round(tol_k_one(10, 0.95, 0.95), 3), 2.911)
  expect_equal(round(tol_k_one(20, 0.90, 0.95), 3), 1.926)
  expect_equal(round(tol_k_two(10, 0.95, 0.95), 2), 3.39)    # exact Odeh-Owen value (confirmed by simulation)
  set.seed(1); x <- rnorm(30, 10, 2)
  t <- tost(x, lower = -1, upper = 1, target = 10)
  expect_equal(t$equivalent, max(t$table$Value[6:7]) < 0.05)
  ct <- correlation_table(mtcars, c("mpg", "wt"))
  expect_equal(ct$Correlation, cor(mtcars$mpg, mtcars$wt))
  g <- grubbs_test(c(199.31, 199.53, 200.19, 200.82, 201.92, 201.95, 202.18, 245.57))
  expect_equal(round(g$G, 4), 2.4688); expect_equal(g$Conclusion, "Outlier")
  ros <- c(-0.25, 0.68, 0.94, 1.15, 1.20, 1.26, 1.26, 1.34, 1.38, 1.43, 1.49, 1.49, 1.55, 1.56, 1.58, 1.65, 1.69, 1.70, 1.76, 1.77,
           1.81, 1.91, 1.94, 1.96, 1.99, 2.06, 2.09, 2.10, 2.14, 2.15, 2.23, 2.24, 2.26, 2.35, 2.37, 2.40, 2.47, 2.54, 2.62, 2.64,
           2.90, 2.92, 2.92, 2.93, 3.21, 3.26, 3.30, 3.59, 3.68, 4.30, 4.64, 5.34, 5.42, 6.01)
  e <- esd_test(ros, 10)
  expect_equal(sum(e$Outlier == "Yes"), 3)            # NIST e-Handbook example
  expect_equal(round(e$`Test statistic`[1:3], 3), c(3.119, 2.943, 3.179))
})

test_that("time-series methods run and forecast", {
  for (m in c("linear", "quadratic", "exponential", "moving", "single", "double")) {
    r <- ts_analyze(as.numeric(Nile), m, h = 5)
    expect_equal(nrow(r$forecast), 5)
  }
  d <- ts_analyze(as.numeric(AirPassengers), "decomposition", period = 12, decomp = "multiplicative")
  expect_lt(d$accuracy$Value[1], 10)
  expect_equal(ts_analyze(1:10 * 2 + 1, "linear", 2)$forecast$Forecast, c(23, 25))
})
