library(ggplot2)
`%||%` <- function(a, b) if (is.null(a)) b else a
for (f in c("utils.R", "spc.R", "reliability.R", "capability_nonnormal.R", "worksheets.R", "help.R", "mod_basic.R"))
  source(file.path("..", "..", "R", f), local = TRUE)

test_that("run rules handle 100,000 points quickly", {
  set.seed(1); x <- rnorm(1e5)
  t <- system.time(v <- spc_rules(x, 0, -3, 3))[["elapsed"]]
  expect_lt(t, 10)
  expect_length(v, 1e5)
  expect_true(all(which(lengths(v) > 0 & vapply(v, function(r) 1L %in% r, logical(1))) == which(abs(x) > 3)))
})

test_that("vectorized Kaplan-Meier counts match a direct count", {
  set.seed(2); t <- round(rexp(300, 0.1), 1); f <- runif(300) > 0.3; w <- sample(1:3, 300, TRUE)
  k <- km_counts(t, f, w)
  direct_risk <- vapply(k$time, function(v) sum(w[t >= v]), 0)
  direct_fail <- vapply(k$time, function(v) sum(w[t == v & f]), 0)
  expect_equal(k$at_risk, direct_risk); expect_equal(k$fails, direct_fail)
})

test_that("3-parameter fits on large samples stay accurate", {
  set.seed(3); x <- 50 + rweibull(50000, 2, 100)
  t <- system.time(f <- nn_fit(x, "weibull3"))[["elapsed"]]
  expect_lt(t, 15)
  expect_equal(unname(f$par), c(50, 2, 100), tolerance = 0.03)
})

test_that("thinning keeps the first and last rows", {
  d <- data.frame(i = 1:10000)
  th <- thin_rows(d, 200)
  expect_equal(nrow(th), 200); expect_equal(th$i[c(1, 200)], c(1, 10000))
  expect_identical(thin_rows(d[1:5, , drop = FALSE]), d[1:5, , drop = FALSE])
  tf <- thin_first(data.frame(a = 1:300, b = 1), 100, "signals")
  expect_equal(nrow(tf), 101); expect_match(tf$a[101], "200 more signals")
})

test_that("large-sample confidence interval for the mean", {
  set.seed(4); v <- rnorm(20000, 5, 2)
  expect_equal(ci_mean_interval(v, 0.05), unname(t.test(v)$conf.int[1:2]))
  expect_match(ci_mean_text(v, 0.05, "x"), "confidence interval")
})

test_that("every screen in the menus has help", {
  app <- readLines(file.path("..", "..", "app.R"))
  panels <- regmatches(app, regexpr('nav_panel\\("[^"]+"', app))
  panels <- setdiff(sub('nav_panel\\("', "", sub('"$', "", panels)), "Help & About")
  expect_gt(length(panels), 55)
  expect_equal(setdiff(panels, names(help_topics)), character(0))
  for (t in names(help_topics)) expect_true(all(nzchar(unlist(help_topics[[t]][c("what", "data", "read")]))), info = t)
})
