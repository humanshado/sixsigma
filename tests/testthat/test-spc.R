source(file.path("..", "..", "R", "spc.R"), local = TRUE)

test_that("moving range limits match SixSigma::ss.cc", {
  d <- SixSigma::ss.data.pb1
  ref <- suppressWarnings({
    grDevices::pdf(NULL); on.exit(grDevices::dev.off())
    utils::capture.output(r <- SixSigma::ss.cc("mr", d, CTQ = "pb.humidity"))
    r
  })
  mr <- spc_imr(d$pb.humidity)$charts[[2]]
  expect_equal(mr$cl[1], ref$CL, tolerance = 1e-8)
  expect_equal(mr$ucl[1], ref$UCL, tolerance = 1e-8)
  expect_equal(mr$lcl[1], ref$LCL, tolerance = 1e-8)
})

test_that("Xbar-R limits equal the tabulated A2, D3, D4 constants for n = 5", {
  set.seed(1)
  x <- rnorm(100, 10, 1)
  sg <- spc_subgroups(x, size = 5)
  res <- spc_xbar_r(sg)
  rbar <- mean(vapply(sg, function(v) diff(range(v)), 0))
  xbb <- mean(x)
  expect_equal(res$charts[[1]]$ucl[1], xbb + 0.577 * rbar, tolerance = 1e-3)
  expect_equal(res$charts[[1]]$lcl[1], xbb - 0.577 * rbar, tolerance = 1e-3)
  expect_equal(res$charts[[2]]$ucl[1], 2.114 * rbar, tolerance = 1e-3)
  expect_equal(res$charts[[2]]$lcl[1], 0)
})

test_that("Xbar-S limits equal the tabulated A3, B4 constants for n = 5", {
  set.seed(2)
  x <- rnorm(100, 5, 2)
  sg <- spc_subgroups(x, size = 5)
  res <- spc_xbar_s(sg)
  sbar <- mean(vapply(sg, sd, 0))
  expect_equal(res$charts[[1]]$ucl[1], mean(x) + 1.427 * sbar, tolerance = 1e-3)
  expect_equal(res$charts[[2]]$ucl[1], 2.089 * sbar, tolerance = 1e-3)
  expect_equal(res$charts[[2]]$lcl[1], 0)
})

test_that("I chart uses MRbar / 1.128 as sigma", {
  x <- c(10, 12, 11, 13, 12, 11, 10, 12)
  res <- spc_imr(x)
  sigma <- mean(abs(diff(x))) / 1.128379
  expect_equal(res$charts[[1]]$ucl[1], mean(x) + 3 * sigma, tolerance = 1e-6)
})

test_that("attribute charts follow the standard formulas", {
  d <- c(4, 6, 3, 5, 8, 2)
  n <- c(100, 120, 90, 100, 110, 100)
  p <- spc_p(d, n)$charts[[1]]
  pbar <- sum(d) / sum(n)
  expect_equal(p$cl[1], pbar)
  expect_equal(p$ucl, pbar + 3 * sqrt(pbar * (1 - pbar) / n))

  c_ch <- spc_c(d)$charts[[1]]
  expect_equal(c_ch$ucl[1], mean(d) + 3 * sqrt(mean(d)))
  expect_equal(c_ch$lcl[1], max(0, mean(d) - 3 * sqrt(mean(d))))

  u <- spc_u(d, n)$charts[[1]]
  ubar <- sum(d) / sum(n)
  expect_equal(u$ucl, ubar + 3 * sqrt(ubar / n))

  np <- spc_np(d, rep(100, 6))$charts[[1]]
  expect_equal(np$cl[1], mean(d))
  expect_error(spc_np(d, n), "constant sample size")
})

test_that("run rules flag the right points", {
  y <- c(0, 0.5, -0.5, -4, rep(0.5, 9), 0, -1, 1, 2, 3, 4, 5, 6)
  v <- spc_rules(y, cl = 0, lcl = -3, ucl = 3)
  expect_true(1L %in% v[[4]])                 # beyond LCL
  expect_true(2L %in% v[[13]])                # ninth point above CL
  expect_false(2L %in% v[[12]])
  expect_true(3L %in% v[[20]])                # sixth rising point (-1 .. 4)
  expect_false(3L %in% v[[19]])
})

test_that("limits can be estimated from the first N points", {
  x <- c(rep(c(10, 11), 10), rep(c(20, 21), 10))
  res <- spc_imr(x, n_est = 20)
  expect_equal(res$charts[[1]]$cl[1], 10.5)
  expect_true(all(lengths(res$charts[[1]]$violations[21:40]) > 0))
})

test_that("bad input is rejected with a clear message", {
  expect_error(spc_subgroups(1:3, size = 5), "two subgroups")
  expect_error(spc_p(c(5, 200), c(100, 100)), "larger than its sample size")
  expect_error(spc_c(c(-1, 2)), "non-negative")
})
