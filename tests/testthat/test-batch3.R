library(ggplot2)
`%||%` <- function(a, b) if (is.null(a)) b else a
for (f in c("data_tools.R", "doe.R", "designs_extra.R", "designs_more.R", "power.R", "simulation.R", "reliability.R",
            "reliability_more.R", "taguchi_mixture.R"))
  source(file.path("..", "..", "R", f), local = TRUE)

# ---- Blocking ---------------------------------------------------------------------
test_that("blocking a 2^4 confounds ABCD only (Montgomery Example 7.2)", {
  coded <- doe_coded(4, 16)$matrix
  blk <- doe_block_assign(coded, 2)
  expect_equal(attr(blk, "generators"), "ABCD")
  expect_equal(as.vector(table(blk)), c(8, 8))
  abcd <- apply(coded, 1, prod)
  expect_true(all(tapply(abcd, blk, function(v) length(unique(v))) == 1))
  # Filtration data with block 1 lowered by 20: effects except ABCD are unchanged.
  d <- as.data.frame(coded)
  d$rate <- c(45, 71, 48, 65, 68, 60, 80, 65, 43, 100, 45, 104, 75, 86, 70, 96) - 20 * (blk == 1)
  d$Blocks <- blk
  a <- doe_analyze(d, "rate", c("A", "B", "C", "D"), order = 4, blocks = "Blocks")
  eff <- setNames(a$table$Effect, a$table$Term)
  expect_equal(unname(eff[c("A", "C", "D", "A*C", "A*D")]), c(21.625, 9.875, 14.625, -18.125, 16.625))
  expect_false("A*B*C*D" %in% names(eff[!is.na(eff)]))
})

test_that("four and eight blocks never confound main effects", {
  for (b in c(4, 8)) {
    coded <- doe_coded(5, 32)$matrix
    blk <- doe_block_assign(coded, b)
    expect_equal(length(unique(blk)), b)
    for (j in 1:5) expect_true(all(abs(tapply(coded[, j], blk, mean)) < 1))
  }
  expect_error(doe_block_assign(doe_coded(3, 8)$matrix, 8), "Too many blocks")
})

test_that("blocked designs put center points in every block", {
  fs <- data.frame(name = c("A", "B", "C"), low = c(1, 1, 1), high = c(2, 2, 2))
  d <- doe_create(fs, 8, replicates = 1, center = 2, randomize = FALSE, blocks = 2)
  expect_equal(nrow(d), 12)
  expect_equal(as.vector(table(d$Blocks[d$CenterPt == 0])), c(2, 2))
})

# ---- General full factorial and split-plot ------------------------------------------------
test_that("general full factorial has every combination per replicate", {
  d <- gff_create(c("M", "S"), c("a, b, c", "100, 200"), replicates = 2, seed = 1, block_by_replicate = TRUE)
  expect_equal(nrow(d), 12)
  expect_true(is.numeric(d$S))
  expect_equal(as.vector(table(d$M, d$Replicate)), rep(2, 6))
  expect_equal(sort(unique(d$Blocks)), 1:2)
  expect_error(gff_create("M", "a"), "at least two levels")
})

test_that("split-plot REML recovers whole-plot and subplot effects", {
  fs <- data.frame(name = c("Temp", "Coat", "Time"), low = c(300, 1, 5), high = c(400, 2, 10))
  d <- splitplot_create(fs, c(TRUE, FALSE, FALSE), wp_reps = 4, seed = 1)
  expect_equal(nrow(d), 32)
  # Hard-to-change factor is constant within each whole plot.
  expect_true(all(tapply(d$Temp, d$WholePlot, function(v) length(unique(v))) == 1))
  set.seed(5); wpe <- rnorm(max(d$WholePlot), 0, 2)
  d$Response <- 50 + 4 * (d$Temp == 400) + 6 * (d$Coat == 2) + wpe[d$WholePlot] + rnorm(32, 0, 0.3)
  a <- splitplot_analyze(d, "Response", c("Temp", "Coat", "Time"), "WholePlot")
  tab <- setNames(a$table$Effect, a$table$Term)
  expect_equal(unname(tab["Coat"]), 6, tolerance = 0.05)
  expect_equal(a$table$Stratum[a$table$Term == "Temp"], "Whole plot")
  expect_equal(a$table$Stratum[a$table$Term == "Coat"], "Subplot")
  # Whole-plot effect is tested on far fewer degrees of freedom.
  expect_lt(a$table$DF[a$table$Term == "Temp"], a$table$DF[a$table$Term == "Coat"])
  expect_gt(a$vc$`% of total`[1], 80)
})

# ---- DSD and D-optimal ------------------------------------------------------------------
test_that("Paley conference matrices satisfy C'C = (n-1) I", {
  for (n in c(4, 6, 8, 10, 12, 14, 18, 20)) {
    C <- conference_matrix(n)
    expect_equal(crossprod(C), (n - 1) * diag(n))
    expect_equal(unname(diag(C)), rep(0, n))
  }
})

test_that("definitive screening designs: 2m+1 runs, main effects clear of 2nd-order terms", {
  fs <- data.frame(name = paste0("X", 1:6), low = 0, high = 10)
  d <- dsd_create(fs, randomize = FALSE)
  X <- attr(d, "coded")
  expect_equal(nrow(X), 13)
  expect_equal(crossprod(X)[upper.tri(diag(6))], rep(0, 15))
  expect_equal(max(abs(crossprod(X, X^2))), 0)
  tfi <- apply(utils::combn(6, 2), 2, function(p) X[, p[1]] * X[, p[2]])
  expect_equal(max(abs(crossprod(X, tfi))), 0)
  expect_equal(nrow(dsd_create(data.frame(name = paste0("X", 1:7), low = 0, high = 1))), 17)  # odd m uses order m+1
})

test_that("D-optimal: 9-run quadratic in 2 factors is the 3^2 factorial; constraints are respected", {
  f2 <- data.frame(name = c("Temp", "Time"), low = c(100, 10), high = c(200, 30))
  o <- doptimal_create(f2, 9, "quadratic", seed = 1, randomize = FALSE)
  expect_equal(nrow(unique(o[, c("Temp", "Time")])), 9)
  expect_setequal(unique(o$Temp), c(100, 150, 200))
  o2 <- doptimal_create(f2, 8, "quadratic", constraints = "Temp + 5*Time <= 300", seed = 2)
  expect_true(all(o2$Temp + 5 * o2$Time <= 300))
  f4 <- data.frame(name = LETTERS[1:4], low = -1, high = 1)
  expect_equal(unname(attr(doptimal_create(f4, 8, "linear", seed = 3), "efficiency")[1]), 100)
  expect_error(doptimal_create(f2, 4, "quadratic"), "at least 6 runs")
  expect_error(doptimal_create(f2, 9, "quadratic", constraints = "system('ls')"), "not allowed")
})

# ---- Constrained mixtures -----------------------------------------------------------
test_that("extreme vertices match the textbook region", {
  V <- mixture_vertices(c(.4, .1, .1), c(.8, .5, .3))
  expect_equal(nrow(V), 4)
  expect_true(any(apply(V, 1, function(v) all(abs(v - c(.8, .1, .1)) < 1e-9))))
  d <- mixture_constrained(c("A", "B", "C"), c(.4, .1, .1), c(.8, .5, .3), randomize = FALSE)
  expect_equal(nrow(d), 13)
  expect_equal(rowSums(d[, c("A", "B", "C")]), rep(1, 13))
  expect_equal(unname(unlist(d[d$PtType == "Centroid", c("A", "B", "C")])), c(.55, .25, .2))
  expect_error(mixture_vertices(c(.5, .5, .2), c(.8, .8, .8)), "No blend")
})

# ---- Design power ------------------------------------------------------------------
test_that("general factorial power equals one-way ANOVA power for a single factor", {
  expect_equal(power_value("gff", 5, 1.5, 1, levels = 4), power_value("anova", 5, 1.5, 1, groups = 4))
  expect_equal(power_value("pb", 2, 1, 1, k = 7, runs = 12), power_value("factorial", 2, 1, 1, k = 7, runs = 12, terms = 8))
  expect_true(is.na(power_value("gff", 1, 1, 1, levels = c(3, 2))))  # no error df without replication
  r <- power_solve("gff", power = 0.8, delta = 2, sd = 1.5, levels = c(3, 2))
  expect_equal(r$n, 6)
})

# ---- Stack-up and Monte Carlo ---------------------------------------------------------
test_that("stack-up worst case and RSS follow the textbook formulas", {
  p <- data.frame(name = c("H", "S", "W"), nominal = c(50, 30, 19.8), tol_minus = c(.1, .05, .05),
                  tol_plus = c(.1, .05, .05), sign = c(1, -1, -1), dist = "normal")
  r <- stackup(p, n = 2e5, seed = 1)
  expect_equal(r$nominal, 0.2)
  expect_equal(unlist(r$limits[1, 2:3], use.names = FALSE), c(0, 0.4))
  expect_equal(r$sd, sqrt(sum((c(.1, .05, .05) / 3)^2)))
  expect_equal(unlist(r$limits[2, 2:3], use.names = FALSE), 0.2 + c(-3, 3) * r$sd)
  expect_equal(unname(r$limits$Maximum[3]), 0.2 + 3 * r$sd, tolerance = 0.02)
  # Uniform contributor: sd = half-width / sqrt(3)
  p$dist[1] <- "uniform"
  expect_equal(stackup(p, n = 1000, seed = 1)$contributions$`Std dev`[1], 0.1 / sqrt(3))
})

test_that("Monte Carlo of a sum of normals matches theory; formulas are sandboxed", {
  inp <- data.frame(name = c("a", "b"), dist = "normal", p1 = c(1, 2), p2 = c(3, 4), p3 = NA)
  m <- monte_carlo(inp, "a + b", 2e5, lsl = -7, usl = 13, seed = 1)
  s <- setNames(m$summary$Value, m$summary$Statistic)
  expect_equal(unname(s["Mean"]), 3, tolerance = 0.01)
  expect_equal(unname(s["StDev"]), 5, tolerance = 0.01)
  expect_equal(unname(s["PPM out of spec"]), 1e6 * 2 * pnorm(-2), tolerance = 0.05)
  expect_equal(m$sensitivity$Input[1], "b")
  expect_error(monte_carlo(inp, "system('id')", 1000), "not allowed")
  tri <- sim_draw(2e5, "triangular", 0, 1, 4); expect_equal(mean(tri), 5 / 3, tolerance = 0.01)
})

# ---- Accelerated life, warranty, demonstration -------------------------------------------
test_that("Nelson's Class-B insulation lognormal-Arrhenius fit (Meeker & Escobar)", {
  d <- read.csv(file.path("..", "..", "data", "examples", "insulation_alt.csv"))
  f <- alt_fit(d$Hours, d$Status == "Failed", d$TempC, dist = "lognormal", rel = "arrhenius")
  expect_equal(f$sigma / log(10), 0.2592, tolerance = 2e-3)   # 0.2592 in log10 hours
  expect_equal(f$b0 / log(10), -6.019, tolerance = 2e-3)
  s <- alt_summary(f, 180)
  expect_equal(s$acceleration$Failures, c(0, 7, 5, 5))
  expect_gt(s$acceleration$`Acceleration factor vs use`[4], 1)
  p <- alt_percentile(f, c(150, 220), 0.5)
  expect_gt(p$Estimate[1], p$Estimate[2])
  expect_error(alt_fit(d$Hours, NULL, rep(1, nrow(d))), "two stress levels")
})

test_that("demonstration test plans match the success-run and chi-square formulas", {
  expect_equal(demo_sample_size(0.9, 0.9), 22)          # ln(0.1)/ln(0.9) = 21.85
  expect_equal(demo_sample_size(0.9, 0.9, failures = 1), 38)
  expect_equal(demo_sample_size(0.9, 0.9, 0, 2, 2), ceiling(log(0.1) / (4 * log(0.9))))
  m <- demo_test_time(0.9, 0.9, 10, 0, 2)
  expect_equal(0.9^(10 * m^2), 0.1, tolerance = 1e-6)
  expect_equal(demo_mtbf_time(1000, 0.9, 0), 1000 * log(10), tolerance = 1e-9)
  expect_equal(demo_mtbf_time(1000, 0.9, 1), 1000 * qchisq(0.9, 4) / 2)
})

test_that("warranty data become censored life data and forecasts are positive", {
  ship <- c(1000, 1200); R <- rbind(c(2, 3, 4), c(3, 4, NA))
  ld <- warranty_life_data(ship, R)
  expect_equal(sum(ld$w), 2200)
  expect_equal(ld$w[!ld$f], c(991, 1193))
  d <- read.csv(file.path("..", "..", "data", "examples", "warranty_returns.csv"))
  w <- warranty_analyze(d$Shipped, d[, paste0("M", 1:8)], horizon = 4)
  expect_true(all(w$forecast$`Expected returns` > 0))
  expect_true(all(diff(w$forecast$Cumulative) > 0))
  w2 <- warranty_analyze(d$Shipped, d[, paste0("M", 1:8)], horizon = 4, limit = 8)
  expect_lt(w2$forecast$Cumulative[4], w$forecast$Cumulative[4])
})
