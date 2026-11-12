library(ggplot2)
source(file.path("..", "..", "R", "hyptests.R"), local = TRUE)
source(file.path("..", "..", "R", "mod_pareto.R"), local = TRUE)

pc <- SixSigma::ss.data.pc

test_that("one-sample t matches t.test", {
  x <- pc$pc.volume
  r <- ht_one_t(x, 16, "two.sided", 0.95, "pc.volume")
  ref <- t.test(x, mu = 16)
  expect_equal(r$p, ref$p.value)
  expect_equal(r$result$T, unname(ref$statistic))
  expect_equal(c(r$result$`CI lower`, r$result$`CI upper`), ref$conf.int[1:2])
  expect_s3_class(r$plot, "ggplot")
})

test_that("two-sample t supports Welch and pooled variances", {
  sp <- by_group(pc$pc.volume, pc$pc.col, max_groups = 2)
  welch <- ht_two_t(sp)
  pooled <- ht_two_t(sp, equal_var = TRUE)
  expect_equal(welch$p, t.test(sp[[1]], sp[[2]])$p.value)
  expect_equal(pooled$p, t.test(sp[[1]], sp[[2]], var.equal = TRUE)$p.value)
  expect_equal(pooled$result$DF, length(unlist(sp)) - 2)
  expect_error(by_group(pc$pc.volume, pc$pc.op, max_groups = 2), "exactly 2")
})

test_that("paired t drops incomplete pairs", {
  a <- c(10, 12, NA, 14, 15, 11)
  b <- c(9, 11, 13, 13, 15, 10)
  r <- ht_paired_t(a, b)
  ref <- t.test(a[-3], b[-3], paired = TRUE)
  expect_equal(r$p, ref$p.value)
  expect_equal(r$result$`Mean difference`, mean(a[-3] - b[-3]))
})

test_that("one-way ANOVA matches aov and reports Tukey comparisons", {
  sp <- by_group(pc$pc.volume, pc$pc.op)
  r <- ht_anova(sp)
  ref <- summary(aov(pc.volume ~ pc.op, data = pc))[[1]]
  expect_equal(r$p, ref$`Pr(>F)`[1])
  expect_equal(r$result$F[1], ref$`F value`[1])
  expect_equal(nrow(r$extra), choose(4, 2))
  expect_equal(r$result$DF, c(3, 20, 23))
})

test_that("equal variances uses Levene (Brown-Forsythe) for the conclusion", {
  sp <- by_group(pc$pc.volume, pc$pc.op)
  r <- ht_variances(sp)
  # Brown-Forsythe by hand.
  dev <- abs(pc$pc.volume - ave(pc$pc.volume, pc$pc.op, FUN = median))
  ref <- summary(aov(dev ~ pc$pc.op))[[1]]$`Pr(>F)`[1]
  expect_equal(r$p, ref)
  expect_equal(r$result$`p-value`[1], bartlett.test(sp)$p.value)
  sp2 <- by_group(pc$pc.volume, pc$pc.col)
  expect_equal(ht_variances(sp2)$result$`p-value`[1], var.test(sp2[[1]], sp2[[2]])$p.value)
})

test_that("nonparametric tests pick Mann-Whitney or Kruskal-Wallis", {
  sp2 <- by_group(pc$pc.volume, pc$pc.col)
  expect_equal(ht_nonparametric(sp2)$title, "Mann-Whitney test")
  sp4 <- by_group(pc$pc.volume, pc$pc.op)
  r <- ht_nonparametric(sp4)
  expect_equal(r$p, kruskal.test(sp4)$p.value)
})

test_that("proportion tests match R and validate counts", {
  r1 <- ht_one_prop(12, 200, 0.05)
  expect_equal(r1$p, binom.test(12, 200, 0.05)$p.value)
  expect_error(ht_one_prop(12, 10, 0.05), "events")
  expect_error(ht_one_prop(1, 10, 1.5), "between 0 and 1")

  r2 <- ht_two_prop(18, 300, 32, 310)
  ref <- prop.test(c(18, 32), c(300, 310), correct = FALSE)
  expect_equal(r2$p, ref$p.value)
  expect_equal(r2$result$Z[1]^2, unname(ref$statistic))
  expect_lt(r2$result$Z[1], 0)
  # Small counts switch the conclusion to Fisher's exact test.
  r3 <- ht_two_prop(1, 10, 4, 12)
  expect_equal(r3$p, fisher.test(matrix(c(1, 9, 4, 8), 2))$p.value)
})

test_that("chi-square test of association matches chisq.test", {
  r <- ht_chisq(pc$pc.col, pc$pc.op, c("pc.col", "pc.op"))
  ref <- suppressWarnings(chisq.test(table(pc$pc.col, pc$pc.op), correct = FALSE))
  expect_equal(r$p, ref$p.value)
  expect_error(ht_chisq(rep("a", 5), letters[1:5]), "two different categories")
})

test_that("conclusion text follows the p-value", {
  expect_match(test_conclusion(0.01, 0.05), "reject the null")
  expect_match(test_conclusion(0.2, 0.05), "do not reject")
})

test_that("Pareto table sorts, tallies and accumulates", {
  t1 <- pareto_table(c("b", "a", "b", "c", "b", "a"))
  expect_equal(t1$Category, c("b", "a", "c"))
  expect_equal(t1$Count, c(3, 2, 1))
  expect_equal(t1$`Cumulative %`[3], 100)

  r <- SixSigma::ss.data.pc.r
  t2 <- pareto_table(r$pc.regions, r$pc.def)
  expect_equal(t2$Count, sort(r$pc.def, decreasing = TRUE))
  expect_s3_class(pareto_plot(t2), "ggplot")
})

test_that("Pareto groups the smallest categories into Other, placed last", {
  cats <- c("A", "B", "C", "D", "E", "F")
  counts <- c(50, 30, 10, 5, 3, 2)
  t <- pareto_table(cats, counts, other_pct = 10)
  expect_equal(t$Category, c("A", "B", "C", "Other"))
  expect_equal(t$Count, c(50, 30, 10, 10))
  expect_equal(sum(t$Count), 100)
  expect_equal(nrow(pareto_table(cats, counts, other_pct = 0)), 6)
  expect_error(pareto_table(c("a", "b"), c(1, -1)), "negative")
})

test_that("typed Pareto input is parsed", {
  p <- parse_typed_counts("Scratches: 54\nDents: 38\n\nTime: 10:30 shift: 4")
  expect_equal(p$cats, c("Scratches", "Dents", "Time: 10:30 shift"))
  expect_equal(p$counts, c(54, 38, 4))
  expect_error(parse_typed_counts("Scratches 54"), "colon")
})
