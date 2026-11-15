# Sample size and power.
#
# Each test has a power function power(n, delta); the solver finds whichever
# of n, power or delta is left blank. t-tests and ANOVA use the noncentral t
# and F distributions (as power.t.test / power.anova.test); one proportion
# uses the normal approximation; two proportions use power.prop.test's
# formula; 2-level factorials use the t-test of an effect with the design's
# error degrees of freedom (as Minitab does).

power_value <- function(test, n, delta, sd = 1, alpha = 0.05, alternative = "two.sided", p0 = 0.5, groups = 3,
                        k = 3, runs = 8, center = 0, terms = NA, levels = c(3, 2)) {
  sides <- if (alternative == "two.sided") 2 else 1
  switch(test,
    one_t = , paired = stats::power.t.test(n = n, delta = abs(delta), sd = sd, sig.level = alpha, type = "one.sample",
                                           alternative = if (sides == 2) "two.sided" else "one.sided", strict = TRUE)$power,
    two_t = stats::power.t.test(n = n, delta = abs(delta), sd = sd, sig.level = alpha, type = "two.sample",
                                alternative = if (sides == 2) "two.sided" else "one.sided", strict = TRUE)$power,
    one_prop = {
      p1 <- p0 + delta
      if (p1 <= 0 || p1 >= 1) return(NA_real_)
      za <- stats::qnorm(1 - alpha / sides)
      num <- abs(delta) * sqrt(n) - za * sqrt(p0 * (1 - p0))
      pw <- stats::pnorm(num / sqrt(p1 * (1 - p1)))
      if (sides == 2) pw <- pw + stats::pnorm((-abs(delta) * sqrt(n) - za * sqrt(p0 * (1 - p0))) / sqrt(p1 * (1 - p1)))
      pw
    },
    two_prop = {
      p2 <- p0 + delta
      if (p2 <= 0 || p2 >= 1) return(NA_real_)
      stats::power.prop.test(n = n, p1 = p0, p2 = p2, sig.level = alpha,
                             alternative = if (sides == 2) "two.sided" else "one.sided", strict = TRUE)$power
    },
    anova = {
      # Least favourable spread for a given maximum difference between means.
      bv <- delta^2 / (2 * (groups - 1))
      stats::power.anova.test(groups = groups, n = n, between.var = bv, within.var = sd^2, sig.level = alpha)$power
    },
    factorial = {
      # n = number of replicates of the base design.
      N <- n * runs + center
      p <- if (is.na(terms)) 1 + k + choose(k, 2) else terms
      df <- N - p - (if (center > 0) 1 else 0)
      if (df < 1) return(NA_real_)
      ncp <- abs(delta) * sqrt(n * runs) / (2 * sd)
      tc <- stats::qt(1 - alpha / sides, df)
      pw <- stats::pt(tc, df, ncp, lower.tail = FALSE)
      if (sides == 2) pw <- pw + stats::pt(-tc, df, ncp)
      pw
    },
    pb = power_value("factorial", n, delta, sd, alpha, alternative, k = k, runs = runs, center = center, terms = 1 + k),
    gff = {
      # General full factorial, full interaction model; power for the main
      # effect of the factor with the most levels, least favourable case (two
      # level means differ by delta, the others at the grand mean).
      levels <- as.integer(levels)
      if (length(levels) < 1 || any(is.na(levels)) || any(levels < 2)) return(NA_real_)
      cells <- prod(levels); a <- max(levels)
      df2 <- cells * (n - 1)
      if (df2 < 1) return(NA_real_)
      ncp <- n * cells / a * delta^2 / (2 * sd^2)
      stats::pf(stats::qf(1 - alpha, a - 1, df2), a - 1, df2, ncp, lower.tail = FALSE)
    },
    stop("Unknown test."))
}

power_n_label <- function(test) switch(test, two_t = , two_prop = , anova = "Sample size per group",
                                        factorial = , pb = , gff = "Replicates", paired = "Number of pairs", "Sample size")

# Solve for the missing quantity. Exactly one of n, power, delta is NA.
power_solve <- function(test, n = NA, power = NA, delta = NA, ...) {
  if (test %in% c("one_prop", "two_prop") && !is.na(delta)) {
    p0 <- list(...)$p0 %||% 0.5
    if (p0 + delta <= 0 || p0 + delta >= 1)
      stop(sprintf("For proportions the difference must keep %g + difference between 0 and 1 (e.g. 0.05 or 0.1).", p0))
  }
  pv <- function(nn, dd) power_value(test, nn, dd, ...)
  min_n <- if (test %in% c("factorial", "pb", "gff")) 1 else 2
  if (is.na(n)) {
    if (!isTRUE(power > 0 && power < 1)) stop("Power must be between 0 and 1.")
    if (!isTRUE(delta != 0)) stop("Enter a non-zero difference.")
    for (nn in min_n:100000) {
      p <- pv(nn, delta)
      if (!is.na(p) && p >= power) return(list(n = nn, power = p, delta = delta))
      if (nn > 2000 && !(test %in% c("factorial", "pb", "gff"))) {
        # Jump with a bisection once the loop gets long.
        lo <- nn; hi <- nn * 2
        while (is.na(pv(hi, delta)) || pv(hi, delta) < power) { hi <- hi * 2; if (hi > 1e8) stop("Sample size exceeds 100,000,000.") }
        while (hi - lo > 1) { mid <- (lo + hi) %/% 2; if (pv(mid, delta) >= power) hi <- mid else lo <- mid }
        return(list(n = hi, power = pv(hi, delta), delta = delta))
      }
    }
  }
  if (is.na(power)) {
    if (!isTRUE(n >= min_n)) stop("Sample size must be at least ", min_n, ".")
    p <- pv(n, delta)
    if (is.na(p)) stop("This combination is not possible (check the proportions or the design's degrees of freedom).")
    return(list(n = n, power = p, delta = delta))
  }
  if (is.na(delta)) {
    if (!isTRUE(power > 0 && power < 1)) stop("Power must be between 0 and 1.")
    f <- function(d) { p <- pv(n, d); if (is.na(p)) -1 else p - power }
    hi <- list(...)$sd %||% 1
    lim <- if (test %in% c("one_prop", "two_prop")) 1 - (list(...)$p0 %||% 0.5) - 1e-6 else hi * 100
    if (f(lim) < 0) stop("Even the largest possible difference does not reach that power with this sample size.")
    d <- stats::uniroot(f, c(1e-8, lim), tol = 1e-10)$root
    return(list(n = n, power = pv(n, d), delta = d))
  }
  stop("Leave exactly one of sample size, power and difference blank.")
}

`%||%` <- function(a, b) if (is.null(a)) b else a

power_curve_plot <- function(test, ns, deltas, target = NA, ...) {
  d <- do.call(rbind, lapply(ns, function(nn) data.frame(n = factor(nn), delta = deltas,
                                                         power = vapply(deltas, function(dd) power_value(test, nn, dd, ...), numeric(1)))))
  g <- ggplot2::ggplot(d, ggplot2::aes(delta, power, colour = n)) + ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::labs(title = "Power curve", x = if (test %in% c("one_prop", "two_prop")) "Difference in proportion" else "Difference",
                  y = "Power", colour = power_n_label(test)) +
    ggplot2::theme_bw(base_size = 12) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  if (!is.na(target)) g <- g + ggplot2::geom_hline(yintercept = target, linetype = 2, colour = "#c0392b")
  g
}
