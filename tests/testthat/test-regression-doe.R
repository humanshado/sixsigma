library(ggplot2)
source(file.path("..", "..", "R", "regression.R"), local = TRUE)
source(file.path("..", "..", "R", "doe.R"), local = TRUE)

st <- SixSigma::ss.data.strings

test_that("simple regression matches lm and reports R-sq and predicted R-sq", {
  m <- fit_regression(st, "power", "len")
  ref <- lm(power ~ len, data = st[complete.cases(st[, c("power", "len")]), ])
  expect_equal(unname(coef(m$fit)), unname(coef(ref)))
  s <- reg_summary(m)
  expect_equal(s$`R-sq (%)`, 100 * summary(ref)$r.squared)
  press <- sum((residuals(ref) / (1 - hatvalues(ref)))^2)
  expect_equal(s$PRESS, press)
  expect_true(m$single_num)
  expect_s3_class(reg_fitted_line(m), "ggplot")
})

test_that("quadratic model adds a squared term", {
  m <- fit_regression(st, "power", "len", poly = 2)
  expect_equal(nrow(reg_coefficients(m)), 3)
  expect_match(reg_equation(m), "len\\^2")
})

test_that("multiple regression with a categorical predictor and VIF", {
  m <- fit_regression(st, "power", c("len", "type"))
  ref <- lm(power ~ len + type, data = st)
  expect_equal(unname(coef(m$fit)), unname(coef(ref)))
  co <- reg_coefficients(m)
  expect_true(is.na(co$VIF[1]))
  expect_true(all(co$VIF[-1] >= 1))
  a <- reg_anova(m)
  expect_equal(a$`F`[1], summary(ref)$fstatistic[["value"]])
})

test_that("interactions and prediction intervals", {
  m <- fit_regression(st, "power", c("len", "res"), interact = TRUE)
  expect_true(any(grepl(":", reg_coefficients(m)$Term)))
  p <- reg_predict(m, data.frame(len = 950, res = 6))
  expect_lt(p$`PI lower`, p$`CI lower`)
  expect_gt(p$`PI upper`, p$`CI upper`)
  ref <- predict(m$fit, data.frame(len = 950, res = 6), interval = "prediction")
  expect_equal(p$`PI upper`, unname(ref[, "upr"]))
})

test_that("regression input errors are explained", {
  expect_error(fit_regression(st, "power", character(0)), "at least one predictor")
  expect_error(fit_regression(st, "power", "power"), "cannot also be")
  expect_error(fit_regression(st, "type", "len"), "must be numeric")
})

test_that("Montgomery's unreplicated 2^4 example: Lenth's method finds A, C, D, AC, AD", {
  d <- as.data.frame(doe_coded(4, 16)$matrix)
  d$rate <- c(45, 71, 48, 65, 68, 60, 80, 65, 43, 100, 45, 104, 75, 86, 70, 96)
  a <- doe_analyze(d, "rate", c("A", "B", "C", "D"), order = 4)
  eff <- setNames(a$table$Effect, a$table$Term)
  expect_equal(unname(eff[c("A", "C", "D", "A*C", "A*D")]), c(21.625, 9.875, 14.625, -18.125, 16.625))
  expect_setequal(a$table$Term[a$table$Significant == "Yes"], c("A", "C", "D", "A*C", "A*D"))
  expect_match(a$method, "PSE = 2.625")
})

test_that("replicated design uses t-tests; effects are twice the coefficients", {
  b <- SixSigma::ss.data.doe1
  a <- doe_analyze(b, "score", c("flour", "salt", "bakPow"), order = 3)
  expect_match(a$method, "8 error degrees of freedom")
  expect_equal(a$table$Effect, 2 * a$table$Coef)
  # flour at "+" raises the score by the difference in means
  m <- tapply(b$score, b$flour, mean)
  expect_equal(a$table$Effect[a$table$Term == "flour"], unname(m["+"] - m["-"]))
  expect_s3_class(doe_pareto_plot(a), "ggplot")
  expect_s3_class(doe_main_effects_plot(a), "ggplot")
  expect_s3_class(doe_interaction_plot(a), "ggplot")
  expect_s3_class(doe_normal_plot(a), "ggplot")
})

test_that("generators give orthogonal, balanced designs of the stated resolution", {
  for (key in names(doe_generators)) {
    k <- as.integer(sub("-.*", "", key)); r <- as.integer(sub(".*-", "", key))
    m <- doe_coded(k, r)$matrix
    expect_equal(dim(m), c(r, k), info = key)
    expect_equal(unname(colSums(m)), rep(0, k), info = key)               # balanced
    expect_equal(unname(crossprod(m)), diag(r, k), info = key)            # orthogonal
    expect_equal(nrow(unique(m)), r, info = key)                          # no repeated runs
  }
  expect_equal(doe_aliases(4, 8)$resolution, "IV")
  expect_equal(doe_aliases(5, 16)$resolution, "V")
  expect_equal(doe_aliases(7, 8)$resolution, "III")
  expect_equal(doe_aliases(3, 8)$resolution, "Full")
})

test_that("alias structure of the 2^(5-2) design", {
  al <- doe_aliases(5, 8)
  expect_equal(al$defining, "I = ABD = ACE = BCDE")
  expect_equal(al$table$`Aliased with (up to 3-factor terms)`[al$table$Effect == "A"], "B*D + C*E")
})

test_that("doe_create builds replicated, centered, randomized worksheets", {
  f <- data.frame(name = c("Temp", "Time", "Conc"), low = c(150, 10, 1), high = c(200, 20, 2))
  d <- doe_create(f, runs = 8, replicates = 2, center = 3, seed = 42)
  expect_equal(nrow(d), 19)
  expect_equal(sum(d$CenterPt == 0), 3)
  expect_equal(sort(d$RunOrder), 1:19)
  expect_true(all(d$Temp[d$CenterPt == 0] == 175))
  expect_true(all(is.na(d$Response)))
  # Text levels and a half fraction
  g <- data.frame(name = c("A1", "B1", "C1", "D1"), low = c("x", "lo", "a", "p"), high = c("y", "hi", "b", "q"),
                  stringsAsFactors = FALSE)
  h <- doe_create(g, runs = 8, randomize = FALSE)
  expect_equal(nrow(h), 8)
  expect_equal(h$StdOrder, 1:8)
  expect_error(doe_create(g, runs = 8, center = 2), "numeric")
  expect_error(doe_create(f[c(1, 1, 2), ], runs = 8), "different")
})

test_that("a created design can be analysed after responses are entered", {
  f <- data.frame(name = c("Temp", "Time", "Conc"), low = c(150, 10, 1), high = c(200, 20, 2))
  d <- doe_create(f, runs = 8, replicates = 1, center = 4, seed = 1)
  set.seed(3)
  ct <- function(v, lo, hi) (v - (lo + hi) / 2) / ((hi - lo) / 2)
  d$Response <- 50 + 6 * ct(d$Temp, 150, 200) - 3 * ct(d$Time, 10, 20) +
    4 * (d$CenterPt == 0) + rnorm(nrow(d), sd = 0.5)
  a <- doe_analyze(d, "Response", c("Temp", "Time", "Conc"), order = 2)
  e <- setNames(a$table$Effect, a$table$Term)
  expect_equal(unname(e["Temp"]), 12, tolerance = 0.1)
  expect_equal(unname(e["Time"]), -6, tolerance = 0.1)
  expect_true("Ct Pt (curvature)" %in% a$table$Term)
  expect_equal(a$table$Significant[a$table$Term == "Ct Pt (curvature)"], "Yes")
})

test_that("analysis rejects factors that are not two-level", {
  d <- data.frame(a = c(1, 2, 3, 4), b = c(1, 1, 2, 2), y = 1:4)
  expect_error(doe_analyze(d, "y", c("a", "b")), "two levels")
})
