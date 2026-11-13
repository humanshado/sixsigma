library(ggplot2)
for (f in c("spc.R", "multivariate.R", "capability.R", "capability_nonnormal.R"))
  source(file.path("..", "..", "R", f), local = TRUE)
qcr_csv <- function(n) read.csv(file.path("..", "..", "data", "qcr", paste0(n, ".csv")))

# Montgomery, Introduction to SQC, Table 9.1: 20 values from N(10, 1), then a
# shift of one sigma.
mont <- c(9.45, 7.99, 9.29, 11.66, 12.16, 10.18, 8.04, 11.46, 9.20, 10.34, 9.03, 11.47, 10.51, 9.40, 10.08,
          9.37, 10.62, 10.31, 8.52, 10.84, 10.90, 9.33, 12.29, 11.50, 10.60, 11.08, 10.38, 11.62, 11.31, 10.52)

test_that("tabular CUSUM reproduces Montgomery's example 9.1", {
  ch <- spc_cusum(mont, k = 0.5, h = 5, mu0 = 10, sigma = 1)$charts[[1]]
  expect_equal(round(ch$y[c(4, 5, 29, 30)], 2), c(1.16, 2.82, 5.28, 5.30))
  expect_equal(which(lengths(ch$violations) > 0)[1], 29)
  expect_true(all(ch$y2 <= 0))
  expect_s3_class(plot_chart(ch), "ggplot")
})

test_that("EWMA reproduces Montgomery's example 9.2 (lambda 0.1, L 2.7)", {
  ch <- spc_ewma(mont, lambda = 0.1, L = 2.7, mu0 = 10, sigma = 1)$charts[[1]]
  expect_equal(round(ch$y[1:3], 3), c(9.945, 9.749, 9.704))
  expect_equal(round(ch$ucl[1], 2), 10.27)
  expect_equal(round(ch$ucl[30], 2), 10.62)  # approaches the steady-state limit
  expect_true(all(lengths(ch$violations[1:20]) == 0))
})

test_that("EWMA and CUSUM on subgroup means estimate sigma from ranges", {
  p <- qcr_csv("pistonrings")
  sg <- spc_subgroups(p$diameter, groups = p$sample)
  e <- spc_ewma(p$diameter, sg, n_est = 25)
  rbar <- mean(vapply(sg[1:25], function(v) diff(range(v)), 0))
  expect_equal(e$sigma, rbar / spc_d2(5), tolerance = 1e-8)
  expect_equal(length(e$charts[[1]]$y), 40)
  expect_gt(length(which(lengths(spc_cusum(p$diameter, sg, n_est = 25)$charts[[1]]$violations) > 0)), 0)
})

test_that("Hotelling T2 equals the Mahalanobis distance from the reference period", {
  d <- qcr_csv("dowel1")
  r <- mv_t2(mv_groups(d, c("diameter", "length")), alpha = 0.01, n_ref = 20)
  X <- as.matrix(d[1:20, ])
  expect_equal(unname(r$charts[[1]]$y), unname(mahalanobis(as.matrix(d), colMeans(X), cov(X))))
  m <- 20; p <- 2
  expect_equal(r$charts[[1]]$ucl[1], (m - 1)^2 / m * qbeta(0.99, p / 2, (m - p - 1) / 2))
  expect_equal(r$charts[[1]]$ucl[40], p * (m + 1) * (m - 1) / (m * (m - p)) * qf(0.99, p, m - p))
  expect_equal(r$charts[[1]]$ref_end, 20)
})

test_that("T2 for subgroups uses the pooled covariance and F limits", {
  a <- qcr_csv("archery1")
  r <- mv_t2(mv_groups(a, c("x", "y"), group = "end"), alpha = 0.0027)
  s <- r$setup
  expect_equal(s$n, 3); expect_equal(s$m, 24)
  i <- 5
  d <- s$means[i, ] - s$center
  expect_equal(unname(r$charts[[1]]$y[i]), 3 * drop(t(d) %*% solve(s$S) %*% d))
  expect_error(mv_t2(mv_groups(a[-1, ], c("x", "y"), group = "end")), "same size")
})

test_that("T2 contributions flag the variable that moved", {
  set.seed(5)
  X <- data.frame(a = rnorm(60), b = rnorm(60))
  X$b[55] <- 8
  r <- mv_t2(mv_groups(X, c("a", "b")), alpha = 0.001, n_ref = 50)
  expect_true("55" %in% r$contributions$Sample)
  expect_equal(r$contributions$`Largest contributor`[r$contributions$Sample == "55"], "b")
})

test_that("MEWMA uses the tabulated limit and detects a sustained shift", {
  expect_equal(mewma_h(0.1, 2), 8.6336)
  expect_equal(mewma_h(0.2, 3), 11.8961)
  expect_true(is.na(mewma_h(0.15, 2)))
  set.seed(2)
  X <- data.frame(a = rnorm(80), b = rnorm(80))
  X[51:80, ] <- X[51:80, ] + 0.8
  r <- mv_mewma(mv_groups(X, c("a", "b")), lambda = 0.1, n_ref = 50)
  sig <- which(lengths(r$charts[[1]]$violations) > 0)
  expect_true(length(sig) > 0 && min(sig) > 50)
  expect_error(mv_mewma(mv_groups(X, c("a", "b")), lambda = 0.15), "Enter h")
})

test_that("MCUSUM stays near zero in control and grows after a shift", {
  set.seed(3)
  X <- data.frame(a = rnorm(80), b = rnorm(80))
  X[51:80, "a"] <- X[51:80, "a"] + 1
  y <- mv_mcusum(mv_groups(X, c("a", "b")), n_ref = 50)$charts[[1]]$y
  expect_lt(mean(y[1:50]), mean(y[61:80]))
  expect_true(any(y[51:80] > 5.5))
})

test_that("depth ranks are uniform in control and near zero for outliers", {
  set.seed(7)
  G <- matrix(rnorm(1000), 500)
  for (m in c("mahalanobis", "spatial")) {
    r <- depth_ranks(matrix(rnorm(400), 200), G, m)
    expect_equal(mean(r), 0.5, tolerance = 0.08)
    expect_lt(max(depth_ranks(matrix(c(6, 6, -6, 6), 2, byrow = TRUE), G, m)), 0.01)
  }
})

test_that("r, Q and S charts signal a shift with few false alarms before it", {
  set.seed(11)
  d <- as.data.frame(rbind(matrix(rnorm(400), 200), matrix(rnorm(60), 30), matrix(rnorm(60, 2.5), 30)))
  r <- np_r_chart(d, c("V1", "V2"), 200, alpha = 0.01)$charts[[1]]
  expect_lte(sum(lengths(r$violations[1:30]) > 0), 2)
  expect_gte(sum(lengths(r$violations[31:60]) > 0), 10)
  q <- np_q_chart(d, c("V1", "V2"), 200, size = 5, alpha = 0.01)$charts[[1]]
  expect_equal(length(q$y), 12)
  expect_true(all(lengths(q$violations[7:12]) > 0))
  s <- np_s_chart(d, c("V1", "V2"), 200, alpha = 0.01)$charts[[1]]
  expect_true(any(lengths(s$violations[31:60]) > 0))
  expect_error(np_r_chart(d, c("V1", "V2"), 3), "at least")
})

test_that("Vannman's family gives Cp, Cpk, Cpm and Cpmk", {
  mu <- 10.2; s <- 0.5; lsl <- 8; usl <- 12; T <- 10
  expect_equal(vannman_index(mu, s, lsl, usl, T, 0, 0), (usl - lsl) / (6 * s))
  expect_equal(vannman_index(mu, s, lsl, usl, T, 1, 0), min(usl - mu, mu - lsl) / (3 * s))
  expect_equal(vannman_index(mu, s, lsl, usl, T, 0, 1), (usl - lsl) / (6 * sqrt(s^2 + (mu - T)^2)))
  expect_equal(vannman_index(mu, s, lsl, usl, T, 1, 1), min(usl - mu, mu - lsl) / (3 * sqrt(s^2 + (mu - T)^2)))
})

test_that("capability table: within vs overall sigma, CIs and one-sided specs", {
  p <- qcr_csv("pistonrings")
  x <- p$diameter[1:125]
  sw <- cap_within_sigma(x, "pooled", p$sample[1:125])
  sds <- tapply(x, p$sample[1:125], sd)
  expect_equal(sw, sqrt(mean(sds^2)) / unname(SixSigma::ss.cc.getc4(101)))
  t <- cap_table(x, 73.99, 74.01, 74, sw)
  expect_equal(t$Within[1], 0.02 / (6 * sw))
  expect_equal(t$Overall[1], 0.02 / (6 * sd(x)))
  expect_true(all(t$`Within CI lower`[1:5] < t$Within[1:5] & t$Within[1:5] < t$`Within CI upper`[1:5]))
  one <- cap_table(x, NA, 74.01, 74)
  expect_true(is.na(one$Overall[1]) && !is.na(one$Overall[4]))
  expect_equal(cap_within_sigma(c(1, 3, 2, 4), "mr"), mean(c(2, 1, 2)) / unname(SixSigma::ss.cc.getd2(2)))
})

test_that("nonparametric indices use percentiles and the median", {
  set.seed(1)
  x <- rnorm(5000, 10, 1)
  np <- cap_nonparametric(x, 7, 13, 10)
  expect_equal(np$Value[1], 1, tolerance = 0.06)   # close to Cp for normal data
  expect_error(cap_nonparametric(x, NA, 13, 10), "both")
})

test_that("non-normal capability: lognormal data prefer the lognormal fit", {
  set.seed(4)
  x <- rlnorm(300, 1, 0.4)
  cmp <- nn_compare(x)
  expect_true(cmp$Code[1] %in% c("lognormal", "boxcox"))
  f <- nn_fit(x, "lognormal")
  expect_equal(unname(f$par), c(mean(log(x)), sqrt(mean((log(x) - mean(log(x)))^2))), tolerance = 1e-6)
  cap <- nn_capability(f, 0.5, 12)
  q <- qlnorm(c(0.00135, 0.5, 0.99865), f$par[1], f$par[2])
  expect_equal(cap$indices$Value[1], (12 - 0.5) / (q[3] - q[1]))
  expect_equal(cap$indices$Value[4], min((q[2] - 0.5) / (q[2] - q[1]), (12 - q[2]) / (q[3] - q[2])))
  expect_equal(cap$ppm$Expected[2], 1e6 * (1 - plnorm(12, f$par[1], f$par[2])))
})

test_that("normal fit gives the usual Pp; Box-Cox round-trips its quantiles", {
  set.seed(9)
  x <- rnorm(200, 50, 2)
  f <- nn_fit(x, "normal")
  expect_equal(nn_capability(f, 44, 56)$indices$Value[1], 12 / (6 * sd(x)), tolerance = 1e-4)
  y <- rgamma(200, 3, 1)
  b <- nn_fit(y, "boxcox")
  expect_equal(b$pfun(b$qfun(c(0.1, 0.5, 0.9))), c(0.1, 0.5, 0.9), tolerance = 1e-8)
  expect_equal(nn_fit(y, "boxcox", round_lambda = TRUE)$par[["lambda"]] %% 0.5, 0)
  expect_error(nn_fit(c(-1, y), "weibull"), "positive")
  for (d in c("weibull", "gamma", "exponential", "logistic")) expect_true(is.finite(nn_fit(y, d)$aic))
})

test_that("every new chart type can be drawn", {
  d <- qcr_csv("dowel1"); g <- mv_groups(d, c("diameter", "length"))
  charts <- list(mv_t2(g, n_ref = 20), mv_mewma(g, n_ref = 20), mv_mcusum(g, n_ref = 20),
                 np_r_chart(d, c("diameter", "length"), 20), np_q_chart(d, c("diameter", "length"), 20, size = 5),
                 np_s_chart(d, c("diameter", "length"), 20), spc_ewma(mont), spc_cusum(mont))
  for (r in charts) {
    g <- plot_chart(r$charts[[1]])
    expect_s3_class(g, "ggplot")
    expect_no_error(ggplot2::ggplot_build(g))
  }
  expect_no_error(ggplot2::ggplot_build(vannman_plot(10, 0.5, 8, 12, 10)))
  f <- nn_fit(rgamma(100, 3, 1), "gamma")
  expect_no_error(ggplot2::ggplot_build(nn_plot(f, 0.1, 12)))
  expect_no_error(ggplot2::ggplot_build(nn_probability_plot(f)))
})
