library(ggplot2)
for (f in c("doe.R", "designs_extra.R", "rsm.R", "desirability.R", "taguchi_mixture.R", "msa_extra.R",
            "capability_nonnormal.R"))
  source(file.path("..", "..", "R", f), local = TRUE)

test_that("Plackett-Burman designs are balanced and orthogonal", {
  for (r in c(12, 20, 24)) {
    m <- pb_matrix(r)
    expect_equal(dim(m), c(r, r - 1))
    expect_equal(unname(crossprod(m)), diag(r, r - 1))
    expect_equal(unname(colSums(m)), rep(0, r - 1))
  }
  expect_true(is_pb(7, 12)); expect_false(is_pb(7, 8))
  expect_true(12 %in% doe_options(7)); expect_true(all(c(12, 20, 24) %in% doe_options(11)))
  d <- doe_create(data.frame(name = paste0("F", 1:9), low = 0, high = 1), runs = 12, randomize = FALSE)
  expect_equal(dim(d), c(12, 3 + 9 + 1))
  expect_match(doe_aliases(9, 12)$defining, "Plackett-Burman")
})

test_that("Taguchi arrays are orthogonal and match the published L8, L9 and L18", {
  expect_equal(apply(taguchi_array("L8"), 1, paste, collapse = ""),
               c("1111111", "1112222", "1221122", "1222211", "2121212", "2122121", "2211221", "2212112"))
  expect_equal(apply(taguchi_array("L9"), 1, paste, collapse = ""),
               c("1111", "1222", "1333", "2123", "2231", "2312", "3132", "3213", "3321"))
  expect_equal(paste(taguchi_array("L18")[18, ], collapse = ""), "23321231")
  # Rows 5 and 6 as published by Taguchi (r6qualitytools 1.0.1 has the last
  # two entries of column 8 swapped, which unbalances that column).
  expect_equal(apply(taguchi_array("L18")[5:6, ], 1, paste, collapse = ""), c("12223311", "12331122"))
  expect_equal(paste(taguchi_array("L27")[27, ], collapse = ""), "3321321213132")
  for (n in names(oa_tables)) {
    m <- taguchi_array(n)
    # Every pair of columns shows each level combination equally often.
    for (i in 1:(ncol(m) - 1)) for (j in (i + 1):ncol(m)) {
      tab <- table(m[, i], m[, j])
      expect_true(length(unique(as.vector(tab))) == 1, info = paste(n, i, j))
    }
  }
})

test_that("Taguchi S/N ratios and response tables", {
  expect_equal(sn_ratio(matrix(c(10, 10), 1), "larger"), 20)
  expect_equal(sn_ratio(matrix(c(10, 10), 1), "smaller"), -20)
  expect_equal(sn_ratio(matrix(c(9, 11), 1), "nominal"), 10 * log10(100 / 2))
  d <- taguchi_create("L9", c("A", "B", "C"), replicates = 2)
  d$Y1 <- 10 + 2 * d$A; d$Y2 <- d$Y1 + 0.5
  a <- taguchi_analyze(d, c("A", "B", "C"), c("Y1", "Y2"), "larger")
  expect_equal(a$best$`Best level (max S/N)`[1], "3")
  expect_equal(unname(a$best$`S/N rank`[1]), 1)
  expect_equal(a$mean_table$table$A[1:3], c(12.25, 14.25, 16.25))
  expect_error(taguchi_analyze(d, c("A", "B", "C"), "Y1", "nominal"), "two or more")
  expect_error(taguchi_create("L4", c("A", "B", "C", "D")), "columns")
})

# Montgomery, Design and Analysis of Experiments, Example 11.2.
ccd <- data.frame(t = c(80, 80, 90, 90, 85, 85, 85, 85, 85, 92.07, 77.93, 85, 85),
                  T = c(170, 180, 170, 180, 175, 175, 175, 175, 175, 175, 175, 182.07, 167.93),
                  y = c(76.5, 77, 78, 79.5, 79.9, 80.3, 80, 79.7, 79.8, 78.4, 75.6, 78.5, 77),
                  v = c(62, 60, 66, 59, 72, 69, 68, 70, 71, 68, 71, 58, 57))

test_that("RSM reproduces Montgomery's example 11.2 stationary point", {
  m <- rsm_fit(ccd, "y", c("t", "T"))
  s <- rsm_stationary(m)
  expect_equal(s$nature, "Maximum")
  expect_equal(round(s$point$Uncoded, 1), c(86.9, 176.5))
  expect_equal(round(s$predicted, 2), 80.21)
  a <- rsm_anova(m)
  expect_true(all(c("  Lack of fit", "  Pure error") %in% a$Source))
  expect_equal(a$DF[a$Source == "  Pure error"], 4)
  expect_s3_class(rsm_contour_plot(m, "t", "T"), "ggplot")
  expect_no_error(ggplot2::ggplot_build(rsm_contour_plot(m, "t", "T")))
  grDevices::pdf(NULL); on.exit(grDevices::dev.off())
  expect_no_error(rsm_surface_plot(m, "t", "T"))
})

test_that("steepest ascent moves along the first-order gradient", {
  m <- rsm_fit(ccd, "y", c("t", "T"), "linear")
  p <- rsm_steepest(m, 3)
  b <- coef(m$fit)[c("t", "T")]
  step_coded <- c((p$t[2] - m$coding$t$mid) / m$coding$t$half, (p$T[2] - m$coding$T$mid) / m$coding$T$half)
  expect_equal(unname(step_coded), unname(b / max(abs(b))))
  expect_true(all(diff(p$`Predicted y`) > 0))
})

test_that("CCD and Box-Behnken designs have the textbook structure", {
  c3 <- rsm_ccd(3, "rotatable", 6)
  expect_equal(nrow(c3$matrix), 8 + 6 + 6); expect_equal(c3$alpha, 8^(1 / 4))
  expect_equal(rsm_ccd(2, "face", 3)$alpha, 1)
  expect_equal(nrow(rsm_bbd(3, 3)$matrix), 15); expect_equal(nrow(rsm_bbd(4, 3)$matrix), 27)
  ws <- rsm_worksheet(rsm_ccd(2, "rotatable", 5), data.frame(name = c("A1", "B1"), low = c(10, 100), high = c(20, 200)),
                      randomize = FALSE)
  expect_equal(range(ws$A1), c(15 - 5 * sqrt(2), 15 + 5 * sqrt(2)))
  expect_equal(sum(ws$PtType == "Center"), 5)
})

test_that("desirability functions and the optimizer", {
  expect_equal(desirability_d(c(0, 5, 10, 12), "maximize", 0, 10, NA), c(0, 0.5, 1, 1))
  expect_equal(desirability_d(c(0, 5, 10), "minimize", NA, 0, 10), c(1, 0.5, 0))
  expect_equal(desirability_d(c(4, 5, 6, 7), "target", 4, 5, 7), c(0, 1, 0.5, 0))
  m1 <- rsm_fit(ccd, "y", c("t", "T")); m2 <- rsm_fit(ccd, "v", c("t", "T"))
  g <- list(list(response = "y", goal = "maximize", lower = 78.5, target = 80, upper = NA, weight = 1, importance = 1),
            list(response = "v", goal = "target", lower = 62, target = 65, upper = 68, weight = 1, importance = 1))
  o <- optimize_desirability(list(m1, m2), g)
  expect_gt(o$D, 0.95)
  expect_true(abs(o$responses$Predicted[2] - 65) < 0.5)
  expect_no_error(ggplot2::ggplot_build(optimizer_plot(list(m1, m2), o, g)))
  bad <- g; bad[[1]]$lower <- 81
  expect_error(optimize_desirability(list(m1, m2), bad), "lower < target")
})

test_that("mixture model reproduces Montgomery's yarn elongation example", {
  m <- mixture_create(c("PE", "PS", "PP"), type = "lattice", degree = 2, randomize = FALSE)
  y <- list(c(11, 12.4), c(9.4, 8.8, 10), c(16.8, 16), c(15, 14.8, 16.1), c(17.7, 16.4, 16.6), c(10, 9.7, 11.8))
  d <- do.call(rbind, lapply(1:6, function(i) data.frame(m[rep(i, length(y[[i]])), c("PE", "PS", "PP")], E = y[[i]])))
  f <- mixture_fit(d, "E", c("PE", "PS", "PP"))
  expect_equal(round(unname(coef(f$fit)), 1), c(11.7, 9.4, 16.4, 19.0, 11.4, -9.6))
  expect_no_error(ggplot2::ggplot_build(mixture_ternary(f)))
  expect_no_error(ggplot2::ggplot_build(mixture_trace(f)))
  expect_equal(nrow(mixture_centroid(4)), 15)
  expect_true(all(abs(rowSums(mixture_lattice(4, 3)) - 1) < 1e-12))
  expect_error(mixture_create(c("A", "B")), "3 to 6")
})

test_that("type 1 gage study follows the Cg/Cgk definitions", {
  set.seed(1); x <- rnorm(30, 10.001, 0.002)
  g <- gage_type1(x, ref = 10, tol = 0.1)
  expect_equal(g$cg, 0.2 * 0.1 / (6 * sd(x)))
  expect_equal(g$cgk, (0.1 * 0.1 - abs(mean(x) - 10)) / (3 * sd(x)))
  expect_error(gage_type1(x[1:5], 10, 0.1), "at least 10")
})

test_that("gage linearity regresses bias on the reference value", {
  set.seed(2)
  d <- data.frame(ref = rep(c(2, 4, 6, 8, 10), each = 12))
  d$meas <- d$ref + 0.7 - 0.13 * d$ref + rnorm(60, 0, 0.2)
  g <- gage_linearity(d, "ref", "meas", pv = 6)
  ref_fit <- lm(I(meas - ref) ~ ref, d)
  expect_equal(unname(g$coef$Coef), unname(coef(ref_fit)))
  expect_equal(g$summary$Value[g$summary$Statistic == "% Linearity"], 100 * abs(coef(ref_fit)[[2]]))
  expect_match(g$verdict[1], "changes significantly")
  expect_equal(nrow(g$per), 5)
})

test_that("Gage R&R worksheet and multi-vari chart", {
  w <- gage_rr_worksheet(10, 3, 2, seed = 4)
  expect_equal(nrow(w), 60)
  expect_true(all(table(w$Part, w$Operator) == 2))
  expect_no_error(ggplot2::ggplot_build(multivari_plot(SixSigma::ss.data.rr, "time1", "prototype", "operator", "run")))
  expect_no_error(ggplot2::ggplot_build(multivari_plot(SixSigma::ss.data.rr, "time1", "prototype")))
  expect_error(multivari_plot(SixSigma::ss.data.rr, "time1", "prototype", "prototype"), "different")
})

test_that("3-parameter fits recover a threshold; bootstrap p-values flag poor fits", {
  set.seed(3); x <- 50 + rweibull(150, 1.8, 10)
  f <- nn_fit(x, "weibull3")
  expect_equal(unname(f$par["threshold"]), 50, tolerance = 0.03)
  expect_lt(f$aic, nn_fit(x, "weibull")$aic)
  expect_lt(max(f$qfun(c(0.001, 0.5)) - c(f$par[["threshold"]], 0)), Inf)
  for (d in c("lognormal3", "gamma3")) expect_true(is.finite(nn_fit(x, d)$aic))
  expect_gt(nn_ad_pvalue(f, 60), 0.05)
  expect_lt(nn_ad_pvalue(nn_fit(rexp(200), "normal"), 60), 0.05)
})

test_that("a factor or component named y does not clash with the response", {
  d <- ccd; names(d)[names(d) == "T"] <- "y"; names(d)[names(d) == "y"][1] <- "y"
  d2 <- data.frame(t = ccd$t, y = ccd$T, resp = ccd$y)
  m <- rsm_fit(d2, "resp", c("t", "y"))
  expect_equal(round(rsm_stationary(m)$predicted, 2), 80.21)
  mix <- data.frame(x = c(1, 0, 0, .5, .5, 0, 1/3), y = c(0, 1, 0, .5, 0, .5, 1/3), z = c(0, 0, 1, 0, .5, .5, 1/3),
                    out = c(5, 6, 7, 6.8, 6.1, 7.2, 6.9))
  expect_equal(length(coef(mixture_fit(mix, "out", c("x", "y", "z"))$fit)), 6)
})
