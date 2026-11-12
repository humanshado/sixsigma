# Hypothesis test calculations. Each function returns a list with
#   title, h0, h1        test name and hypotheses (text)
#   summary              data.frame of sample statistics
#   result               data.frame of test statistics
#   p                    p-value used for the conclusion
#   extra                optional data.frame (e.g. Tukey comparisons)
#   extra_title          heading for `extra`
#   plot                 ggplot object
# Plain vectors in, so the functions can be tested without Shiny.

alt_symbol <- function(alternative) switch(alternative, two.sided = "≠", less = "<", greater = ">")

test_conclusion <- function(p, alpha) {
  if (is.na(p)) return("The test could not be computed.")
  if (p < alpha) {
    sprintf("p-value = %s < α = %s: reject the null hypothesis. The difference is statistically significant.",
            format.pval(p, digits = 4), alpha)
  } else {
    sprintf("p-value = %s ≥ α = %s: do not reject the null hypothesis. There is not enough evidence of a difference.",
            format.pval(p, digits = 4), alpha)
  }
}

clean_num <- function(x, what = "The response") {
  if (!is.numeric(x)) stop(what, " column must be numeric.")
  x <- x[!is.na(x)]
  if (length(x) < 2) stop(what, " needs at least two values.")
  x
}

# Response and group vectors -> list of numeric vectors per group.
by_group <- function(y, g, min_groups = 2, max_groups = Inf) {
  if (!is.numeric(y)) stop("The response column must be numeric.")
  keep <- !is.na(y) & !is.na(g)
  g <- droplevels(factor(g[keep])); y <- y[keep]
  sp <- split(y, g)
  if (length(sp) < min_groups) stop("The group column needs at least ", min_groups, " groups.")
  if (length(sp) > max_groups) stop("The group column has ", length(sp), " groups; this test needs exactly ", max_groups,
                                    ". Use one-way ANOVA for more groups.")
  if (any(lengths(sp) < 2)) stop("Every group needs at least two values.")
  sp
}

group_summary <- function(sp, conf) {
  do.call(rbind, lapply(names(sp), function(n) {
    v <- sp[[n]]
    se <- stats::sd(v) / sqrt(length(v))
    tq <- stats::qt(1 - (1 - conf) / 2, length(v) - 1)
    data.frame(Sample = n, N = length(v), Mean = mean(v), `Std. dev.` = stats::sd(v),
               `SE mean` = se, `CI lower` = mean(v) - tq * se, `CI upper` = mean(v) + tq * se,
               check.names = FALSE)
  }))
}

boxplot_groups <- function(sp, ylab, ref = NULL) {
  df <- data.frame(group = factor(rep(names(sp), lengths(sp)), levels = names(sp)), y = unlist(sp, use.names = FALSE))
  g <- ggplot2::ggplot(df, ggplot2::aes(group, y)) +
    ggplot2::geom_boxplot(fill = "#d6e4f0", colour = "#4a6fa5", outlier.colour = "#c0392b") +
    ggplot2::geom_jitter(width = 0.08, height = 0, alpha = 0.5, colour = "#4a6fa5") +
    ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = 3.5, fill = "#c0392b") +
    ggplot2::labs(x = NULL, y = ylab, caption = "Diamond = mean") +
    ggplot2::theme_minimal(base_size = 13)
  if (!is.null(ref)) g <- g + ggplot2::geom_hline(yintercept = ref, linetype = 2, colour = "#2e7d32")
  g
}

test_result_df <- function(...) {
  data.frame(..., check.names = FALSE)
}

# ---- Means -------------------------------------------------------------------

ht_one_t <- function(x, mu0, alternative = "two.sided", conf = 0.95, name = "x") {
  x <- clean_num(x)
  tt <- stats::t.test(x, mu = mu0, alternative = alternative, conf.level = conf)
  sp <- stats::setNames(list(x), name)
  list(
    title = "One-sample t-test",
    h0 = sprintf("μ = %s", mu0), h1 = sprintf("μ %s %s", alt_symbol(alternative), mu0),
    summary = group_summary(sp, conf),
    result = test_result_df(T = unname(tt$statistic), DF = unname(tt$parameter), `p-value` = tt$p.value,
                            `CI lower` = tt$conf.int[1], `CI upper` = tt$conf.int[2]),
    p = tt$p.value,
    plot = boxplot_groups(sp, name, ref = mu0) + ggplot2::labs(subtitle = paste("Dashed line = hypothesised mean", mu0))
  )
}

ht_two_t <- function(sp, alternative = "two.sided", conf = 0.95, equal_var = FALSE, ylab = "Response") {
  tt <- stats::t.test(sp[[1]], sp[[2]], alternative = alternative, conf.level = conf, var.equal = equal_var)
  n <- names(sp)
  list(
    title = if (equal_var) "Two-sample t-test (pooled variance)" else "Two-sample t-test (Welch, unequal variances)",
    h0 = sprintf("μ(%s) - μ(%s) = 0", n[1], n[2]),
    h1 = sprintf("μ(%s) - μ(%s) %s 0", n[1], n[2], alt_symbol(alternative)),
    summary = group_summary(sp, conf),
    result = test_result_df(Difference = mean(sp[[1]]) - mean(sp[[2]]), T = unname(tt$statistic),
                            DF = unname(tt$parameter), `p-value` = tt$p.value,
                            `CI lower` = tt$conf.int[1], `CI upper` = tt$conf.int[2]),
    p = tt$p.value,
    plot = boxplot_groups(sp, ylab)
  )
}

ht_paired_t <- function(x1, x2, alternative = "two.sided", conf = 0.95, names = c("Sample 1", "Sample 2")) {
  if (!is.numeric(x1) || !is.numeric(x2)) stop("Both columns must be numeric.")
  keep <- !is.na(x1) & !is.na(x2)
  if (sum(keep) < 2) stop("At least two complete pairs are needed.")
  x1 <- x1[keep]; x2 <- x2[keep]
  tt <- stats::t.test(x1, x2, paired = TRUE, alternative = alternative, conf.level = conf)
  d <- x1 - x2
  sp <- stats::setNames(list(x1, x2, d), c(names, "Difference"))
  diff_df <- data.frame(d = d)
  list(
    title = "Paired t-test",
    h0 = "μ(difference) = 0", h1 = sprintf("μ(difference) %s 0", alt_symbol(alternative)),
    summary = group_summary(sp, conf),
    result = test_result_df(`Mean difference` = mean(d), T = unname(tt$statistic), DF = unname(tt$parameter),
                            `p-value` = tt$p.value, `CI lower` = tt$conf.int[1], `CI upper` = tt$conf.int[2]),
    p = tt$p.value,
    plot = ggplot2::ggplot(diff_df, ggplot2::aes(d)) +
      ggplot2::geom_histogram(bins = max(5, min(20, length(d) %/% 2)), fill = "#9bb7d4", colour = "white") +
      ggplot2::geom_vline(xintercept = 0, linetype = 2, colour = "#2e7d32") +
      ggplot2::annotate("errorbarh", y = 0, xmin = tt$conf.int[1], xmax = tt$conf.int[2], height = 0.3,
                        colour = "#c0392b", linewidth = 1) +
      ggplot2::annotate("point", x = mean(d), y = 0, colour = "#c0392b", size = 3) +
      ggplot2::labs(x = sprintf("Difference (%s - %s)", names[1], names[2]), y = "Count",
                    subtitle = "Red = mean difference and confidence interval; dashed = 0") +
      ggplot2::theme_minimal(base_size = 13)
  )
}

ht_anova <- function(sp, conf = 0.95, ylab = "Response") {
  df <- data.frame(g = factor(rep(names(sp), lengths(sp)), levels = names(sp)), y = unlist(sp, use.names = FALSE))
  fit <- stats::aov(y ~ g, data = df)
  tab <- summary(fit)[[1]]
  rss <- sum(stats::residuals(fit)^2); tss <- sum((df$y - mean(df$y))^2)
  tk <- stats::TukeyHSD(fit, conf.level = conf)$g
  list(
    title = "One-way ANOVA",
    h0 = "All group means are equal", h1 = "At least one group mean is different",
    summary = group_summary(sp, conf),
    result = test_result_df(Source = c("Factor", "Error", "Total"),
                            DF = c(tab$Df, sum(tab$Df)),
                            SS = c(tab$`Sum Sq`, sum(tab$`Sum Sq`)),
                            MS = c(tab$`Mean Sq`, NA),
                            F = c(tab$`F value`[1], NA, NA),
                            `p-value` = c(tab$`Pr(>F)`[1], NA, NA),
                            `R-sq (%)` = c(100 * (1 - rss / tss), NA, NA)),
    p = tab$`Pr(>F)`[1],
    extra = data.frame(Comparison = rownames(tk), Difference = tk[, "diff"], `CI lower` = tk[, "lwr"],
                       `CI upper` = tk[, "upr"], `Adjusted p` = tk[, "p adj"],
                       Significant = ifelse(tk[, "p adj"] < 1 - conf, "Yes", "No"), check.names = FALSE),
    extra_title = sprintf("Tukey pairwise comparisons (%g%% family confidence)", 100 * conf),
    plot = boxplot_groups(sp, ylab)
  )
}

# Brown-Forsythe version of Levene's test: ANOVA on |y - group median|.
levene_bf <- function(sp) {
  dev <- lapply(sp, function(v) abs(v - stats::median(v)))
  df <- data.frame(g = factor(rep(names(dev), lengths(dev))), y = unlist(dev, use.names = FALSE))
  tab <- summary(stats::aov(y ~ g, data = df))[[1]]
  list(statistic = tab$`F value`[1], p = tab$`Pr(>F)`[1])
}

ht_variances <- function(sp, conf = 0.95, ylab = "Response") {
  sds <- vapply(sp, stats::sd, numeric(1))
  lev <- levene_bf(sp)
  if (length(sp) == 2) {
    ft <- stats::var.test(sp[[1]], sp[[2]], conf.level = conf)
    res <- test_result_df(Test = c("F test (assumes normality)", "Levene / Brown-Forsythe"),
                          Statistic = c(unname(ft$statistic), lev$statistic),
                          `p-value` = c(ft$p.value, lev$p))
  } else {
    bt <- stats::bartlett.test(sp)
    res <- test_result_df(Test = c("Bartlett (assumes normality)", "Levene / Brown-Forsythe"),
                          Statistic = c(unname(bt$statistic), lev$statistic),
                          `p-value` = c(bt$p.value, lev$p))
  }
  summ <- do.call(rbind, lapply(names(sp), function(n) {
    v <- sp[[n]]; k <- length(v) - 1; a <- 1 - conf
    data.frame(Sample = n, N = length(v), `Std. dev.` = sd(v), Variance = stats::var(v),
               `SD CI lower` = sqrt(k * stats::var(v) / stats::qchisq(1 - a / 2, k)),
               `SD CI upper` = sqrt(k * stats::var(v) / stats::qchisq(a / 2, k)), check.names = FALSE)
  }))
  list(
    title = "Test for equal variances",
    h0 = "All variances are equal", h1 = "At least one variance is different",
    summary = summ, result = res,
    p = lev$p,
    note = "The conclusion uses Levene's test, which does not assume normal data.",
    plot = boxplot_groups(sp, ylab)
  )
}

ht_nonparametric <- function(sp, alternative = "two.sided", ylab = "Response") {
  med <- data.frame(Sample = names(sp), N = lengths(sp),
                    Median = vapply(sp, stats::median, numeric(1)), check.names = FALSE)
  if (length(sp) == 2) {
    wt <- suppressWarnings(stats::wilcox.test(sp[[1]], sp[[2]], alternative = alternative))
    n <- names(sp)
    list(title = "Mann-Whitney test",
         h0 = sprintf("η(%s) = η(%s)", n[1], n[2]),
         h1 = sprintf("η(%s) %s η(%s)", n[1], alt_symbol(alternative), n[2]),
         summary = med,
         result = test_result_df(W = unname(wt$statistic), `p-value` = wt$p.value),
         p = wt$p.value, plot = boxplot_groups(sp, ylab))
  } else {
    kt <- stats::kruskal.test(sp)
    list(title = "Kruskal-Wallis test", h0 = "All medians are equal", h1 = "At least one median is different",
         summary = med,
         result = test_result_df(H = unname(kt$statistic), DF = unname(kt$parameter), `p-value` = kt$p.value),
         p = kt$p.value, plot = boxplot_groups(sp, ylab))
  }
}

# ---- Proportions ---------------------------------------------------------------

check_xn <- function(x, n) {
  if (any(is.na(c(x, n)))) stop("Enter the number of events and trials.")
  if (any(n < 1) || any(x < 0) || any(x > n) || any(x != round(x)) || any(n != round(n)))
    stop("Events and trials must be whole numbers with 0 ≤ events ≤ trials.")
}

prop_plot <- function(labels, p, lo, hi, ref = NULL) {
  df <- data.frame(s = factor(labels, levels = labels), p = p, lo = lo, hi = hi)
  g <- ggplot2::ggplot(df, ggplot2::aes(s, p)) +
    ggplot2::geom_col(fill = "#9bb7d4", width = 0.5) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi), width = 0.15, colour = "#c0392b", linewidth = 0.9) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x = NULL, y = "Proportion", subtitle = "Error bars = exact confidence interval") +
    ggplot2::theme_minimal(base_size = 13)
  if (!is.null(ref)) g <- g + ggplot2::geom_hline(yintercept = ref, linetype = 2, colour = "#2e7d32")
  g
}

ht_one_prop <- function(x, n, p0, alternative = "two.sided", conf = 0.95) {
  check_xn(x, n)
  if (is.na(p0) || p0 <= 0 || p0 >= 1) stop("The hypothesised proportion must be between 0 and 1.")
  bt <- stats::binom.test(x, n, p = p0, alternative = alternative, conf.level = conf)
  ci <- stats::binom.test(x, n, conf.level = conf)$conf.int
  list(
    title = "One-proportion test (exact binomial)",
    h0 = sprintf("p = %s", p0), h1 = sprintf("p %s %s", alt_symbol(alternative), p0),
    summary = data.frame(Events = x, Trials = n, `Sample p` = x / n, check.names = FALSE),
    result = test_result_df(`p-value` = bt$p.value, `CI lower` = bt$conf.int[1], `CI upper` = bt$conf.int[2]),
    p = bt$p.value,
    plot = prop_plot("Sample", x / n, ci[1], ci[2], ref = p0)
  )
}

ht_two_prop <- function(x1, n1, x2, n2, alternative = "two.sided", conf = 0.95) {
  check_xn(c(x1, x2), c(n1, n2))
  pt <- suppressWarnings(stats::prop.test(c(x1, x2), c(n1, n2), alternative = alternative,
                                          conf.level = conf, correct = FALSE))
  ft <- stats::fisher.test(matrix(c(x1, n1 - x1, x2, n2 - x2), 2), alternative = alternative)
  p1 <- x1 / n1; p2 <- x2 / n2
  z <- sign(p1 - p2) * sqrt(unname(pt$statistic))
  ci1 <- stats::binom.test(x1, n1, conf.level = conf)$conf.int
  ci2 <- stats::binom.test(x2, n2, conf.level = conf)$conf.int
  small <- any(c(x1, n1 - x1, x2, n2 - x2) < 5)
  list(
    title = "Two-proportion test",
    h0 = "p1 - p2 = 0", h1 = sprintf("p1 - p2 %s 0", alt_symbol(alternative)),
    summary = data.frame(Sample = c("Sample 1", "Sample 2"), Events = c(x1, x2), Trials = c(n1, n2),
                         `Sample p` = c(p1, p2), check.names = FALSE),
    result = test_result_df(Test = c("Normal approximation (pooled)", "Fisher's exact"),
                            `Difference p1 - p2` = c(p1 - p2, NA), Z = c(z, NA),
                            `p-value` = c(pt$p.value, ft$p.value),
                            `CI lower` = c(pt$conf.int[1], NA), `CI upper` = c(pt$conf.int[2], NA)),
    p = if (small) ft$p.value else pt$p.value,
    note = if (small) "Some counts are below 5, so the conclusion uses Fisher's exact test."
           else "The conclusion uses the normal approximation; Fisher's exact test is shown for reference.",
    plot = prop_plot(c("Sample 1", "Sample 2"), c(p1, p2), c(ci1[1], ci2[1]), c(ci1[2], ci2[2]))
  )
}

ht_chisq <- function(a, b, names = c("Row", "Column")) {
  keep <- !is.na(a) & !is.na(b)
  tab <- table(factor(a[keep]), factor(b[keep]))
  if (any(dim(tab) < 2)) stop("Each column needs at least two different categories.")
  ct <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
  obs <- as.data.frame.matrix(tab)
  exp_df <- as.data.frame.matrix(round(ct$expected, 2))
  low <- mean(ct$expected < 5)
  long <- as.data.frame(tab); names(long) <- c("a", "b", "n")
  list(
    title = "Chi-square test of association",
    h0 = sprintf("%s and %s are independent", names[1], names[2]),
    h1 = sprintf("%s and %s are associated", names[1], names[2]),
    summary = data.frame(` ` = rownames(obs), obs, check.names = FALSE),
    summary_title = "Observed counts",
    result = test_result_df(`Chi-square` = unname(ct$statistic), DF = unname(ct$parameter), `p-value` = ct$p.value),
    p = ct$p.value,
    extra = data.frame(` ` = rownames(exp_df), exp_df, check.names = FALSE),
    extra_title = "Expected counts",
    note = if (low > 0.2) sprintf("%.0f%% of expected counts are below 5; the chi-square approximation may be unreliable.", 100 * low),
    plot = ggplot2::ggplot(long, ggplot2::aes(a, n, fill = b)) +
      ggplot2::geom_col(position = "fill") +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::labs(x = names[1], y = "Share", fill = names[2]) +
      ggplot2::theme_minimal(base_size = 13)
  )
}
