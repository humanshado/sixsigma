# More capability analysis: between/within sigma, the capability sixpack,
# attribute (binomial / Poisson) capability and multivariate capability.

# Between/within sigma for batch processes (Minitab): sigma_between^2 =
# (MRbar of subgroup means / d2)^2 - sigma_within^2 / n, total = sqrt(sum).
cap_between_within <- function(x, groups) {
  keep <- !is.na(x) & !is.na(groups)
  sp <- split(x[keep], factor(groups[keep], levels = unique(groups[keep])))
  sp <- sp[lengths(sp) >= 2]
  if (length(sp) < 3) stop("Between/within capability needs at least three subgroups of two or more values.")
  sw <- cap_within_sigma(unlist(sp), "pooled", rep(seq_along(sp), lengths(sp)))
  means <- vapply(sp, mean, numeric(1))
  nbar <- mean(lengths(sp))
  sb2 <- (mean(abs(diff(means))) / unname(SixSigma::ss.cc.getd2(2)))^2 - sw^2 / nbar
  sb <- sqrt(max(0, sb2))
  list(within = sw, between = sb, total = sqrt(sb^2 + sw^2))
}

# ---- Capability sixpack -------------------------------------------------------

cap_sixpack <- function(x, groups = NULL, lsl = NA, usl = NA, target = NA) {
  x0 <- x
  keep <- !is.na(x) & (if (is.null(groups)) TRUE else !is.na(groups))
  x <- x[keep]
  if (length(x) < 10) stop("A sixpack needs at least 10 values.")
  if (is.na(lsl) && is.na(usl)) stop("Enter at least one specification limit.")
  if (is.null(groups)) {
    r <- spc_imr(x); sw <- r$sigma; sg_list <- NULL
  } else {
    g <- groups[keep]
    sg_list <- spc_subgroups(x, groups = g)
    r <- spc_xbar_r(sg_list); sw <- r$sigma
  }
  if (is.na(target) && !is.na(lsl) && !is.na(usl)) target <- (lsl + usl) / 2
  m <- mean(x); so <- stats::sd(x)
  tab <- cap_table(x, lsl, usl, if (is.na(target)) m else target, sw)
  th <- ggplot2::theme_bw(base_size = 11) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11))
  p1 <- plot_chart(r$charts[[1]]) + th
  p2 <- plot_chart(r$charts[[2]]) + th
  # Last 25 subgroups (or observations).
  if (is.null(sg_list)) {
    last <- utils::tail(seq_along(x), 25)
    d3 <- data.frame(s = last, v = x[last])
    p3 <- ggplot2::ggplot(d3, ggplot2::aes(s, v)) + ggplot2::geom_point(colour = "#4a6fa5") +
      ggplot2::labs(title = "Last 25 observations", x = "Observation", y = "Value") + th
  } else {
    lastg <- utils::tail(seq_along(sg_list), 25)
    d3 <- do.call(rbind, lapply(lastg, function(i) data.frame(s = i, v = sg_list[[i]])))
    p3 <- ggplot2::ggplot(d3, ggplot2::aes(s, v)) + ggplot2::geom_point(colour = "#4a6fa5", alpha = 0.7) +
      ggplot2::stat_summary(fun = mean, geom = "line", colour = "#c0392b") +
      ggplot2::labs(title = "Last 25 subgroups", x = "Subgroup", y = "Values") + th
  }
  specs <- data.frame(v = c(lsl, usl), lab = c("LSL", "USL")); specs <- specs[!is.na(specs$v), ]
  rng <- range(c(x, lsl, usl), na.rm = TRUE); rng <- rng + c(-1, 1) * 0.1 * diff(rng)
  grid <- seq(rng[1], rng[2], length.out = 300)
  curves <- rbind(data.frame(x = grid, y = stats::dnorm(grid, m, sw), Sigma = "Within"),
                  data.frame(x = grid, y = stats::dnorm(grid, m, so), Sigma = "Overall"))
  p4 <- ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = max(8, min(25, length(x) %/% 5)),
                            fill = "#c9d8ea", colour = "white") +
    ggplot2::geom_line(data = curves, ggplot2::aes(x, y, linetype = Sigma), colour = "#4a6fa5", linewidth = 0.9) +
    ggplot2::geom_vline(data = specs, ggplot2::aes(xintercept = v), colour = "#c0392b", linetype = 2) +
    ggplot2::coord_cartesian(xlim = rng) + ggplot2::labs(title = "Capability histogram", x = NULL, y = NULL, linetype = NULL) +
    th + ggplot2::theme(legend.position = "bottom")
  ad <- if (length(x) >= 8) nortest::ad.test(x)$p.value else NA
  p5 <- ggplot2::ggplot(data.frame(v = x), ggplot2::aes(sample = v)) + ggplot2::stat_qq(colour = "#4a6fa5", size = 1.2) +
    ggplot2::stat_qq_line(colour = "#c0392b") +
    ggplot2::labs(title = "Normal probability plot", subtitle = sprintf("Anderson-Darling p = %s", format.pval(ad, digits = 3)),
                  x = "Normal quantile", y = NULL) + th
  # Capability plot: process spreads (+/- 3 sigma) against the specification.
  bars <- data.frame(Which = factor(c("Specs", "Overall", "Within"), levels = c("Within", "Overall", "Specs")),
                     lo = c(if (is.na(lsl)) NA else lsl, m - 3 * so, m - 3 * sw),
                     hi = c(if (is.na(usl)) NA else usl, m + 3 * so, m + 3 * sw))
  ix <- function(name) tab[[if ("Within" %in% names(tab)) "Within" else "Overall"]][match(name, tab$Index)]
  lab <- sprintf("Cp %s  Cpk %s\nPp %s  Ppk %s",
                 formatC(tab$Within[1], digits = 3, format = "f"), formatC(tab$Within[4], digits = 3, format = "f"),
                 formatC(tab$Overall[1], digits = 3, format = "f"), formatC(tab$Overall[4], digits = 3, format = "f"))
  p6 <- ggplot2::ggplot(bars, ggplot2::aes(y = Which)) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lo, xmax = hi), height = 0.3, colour = c("#c0392b", "#4a6fa5", "#2e7d32"), na.rm = TRUE) +
    ggplot2::geom_point(data = data.frame(Which = factor(c("Overall", "Within"), levels = levels(bars$Which)), x = m),
                        ggplot2::aes(x = x), colour = "black") +
    ggplot2::labs(title = "Capability plot", subtitle = lab, x = NULL, y = NULL) + th
  list(plots = list(p1, p2, p3, p4, p5, p6), table = tab, sigma_within = sw, sigma_overall = so, ad_p = ad)
}

# ---- Attribute capability ---------------------------------------------------------

cap_binomial <- function(d, n, conf = 0.95) {
  keep <- !is.na(d) & !is.na(n); d <- d[keep]; n <- n[keep]
  if (any(d > n | d < 0)) stop("Defectives must be between 0 and the sample size.")
  D <- sum(d); N <- sum(n); p <- D / N
  ci <- stats::binom.test(D, N, conf.level = conf)$conf.int
  z <- stats::qnorm(1 - c(p, ci[2], ci[1]))
  list(
    summary = data.frame(Statistic = c("Samples", "Total inspected", "Total defective", "% defective", "% defective CI lower",
                                       "% defective CI upper", "PPM defective", "Process Z", "Process Z CI lower", "Process Z CI upper"),
                         Value = c(length(d), N, D, 100 * p, 100 * ci[1], 100 * ci[2], 1e6 * p, z[1], z[2], z[3])),
    data = data.frame(i = seq_along(d), d = d, n = n, p = d / n), p = p, conf = conf
  )
}

cap_poisson <- function(d, n, conf = 0.95) {
  keep <- !is.na(d) & !is.na(n); d <- d[keep]; n <- n[keep]
  if (any(d < 0) || any(n <= 0)) stop("Defects must be non-negative and units positive.")
  D <- sum(d); N <- sum(n); u <- D / N
  a <- 1 - conf
  ci <- c(if (D == 0) 0 else stats::qchisq(a / 2, 2 * D) / 2, stats::qchisq(1 - a / 2, 2 * (D + 1)) / 2) / N
  list(
    summary = data.frame(Statistic = c("Samples", "Total units", "Total defects", "Mean DPU", "DPU CI lower", "DPU CI upper",
                                       "Minimum DPU", "Maximum DPU"),
                         Value = c(length(d), N, D, u, ci[1], ci[2], min(d / n), max(d / n))),
    data = data.frame(i = seq_along(d), d = d, n = n, p = d / n), p = u, conf = conf
  )
}

cap_attribute_plots <- function(res, type = "binomial") {
  d <- res$data
  th <- ggplot2::theme_bw(base_size = 11) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  chart <- if (type == "binomial") spc_p(d$d, d$n)$charts[[1]] else spc_u(d$d, d$n)$charts[[1]]
  lab <- if (type == "binomial") "% defective" else "DPU"
  scale <- if (type == "binomial") 100 else 1
  cum <- data.frame(i = d$i, v = scale * cumsum(d$d) / cumsum(d$n))
  p2 <- ggplot2::ggplot(cum, ggplot2::aes(i, v)) + ggplot2::geom_line(colour = "#4a6fa5") + ggplot2::geom_point(colour = "#4a6fa5") +
    ggplot2::geom_hline(yintercept = scale * res$p, linetype = 2, colour = "#c0392b") +
    ggplot2::labs(title = sprintf("Cumulative %s", lab), subtitle = "Should level off once enough samples are collected",
                  x = "Sample", y = lab) + th
  p3 <- ggplot2::ggplot(d, ggplot2::aes(n, scale * p)) + ggplot2::geom_point(colour = "#4a6fa5") +
    ggplot2::geom_hline(yintercept = scale * res$p, linetype = 2, colour = "#c0392b") +
    ggplot2::labs(title = sprintf("%s by sample size", lab), subtitle = "No pattern expected", x = "Sample size", y = lab) + th
  p4 <- ggplot2::ggplot(d, ggplot2::aes(scale * p)) + ggplot2::geom_histogram(bins = max(5, min(20, nrow(d) %/% 3)), fill = "#c9d8ea", colour = "white") +
    ggplot2::geom_vline(xintercept = scale * res$p, linetype = 2, colour = "#c0392b") +
    ggplot2::labs(title = sprintf("Histogram of %s", lab), x = lab, y = "Samples") + th
  list(plot_chart(chart) + th, p2, p3, p4)
}

# ---- Multivariate capability (Shahriari, Hubele & Lawrence, 1995) -----------------
#   CpM = (volume of the tolerance region / volume of the modified process
#          region)^(1/p), where the modified process region is the smallest
#          box containing the 99.73% process ellipsoid;
#   PV  = p-value of Hotelling's T^2 test that the mean equals the target;
#   LI  = 1 if the modified process region lies inside the tolerance region.
cap_multivariate <- function(X, lsl, usl, target = NULL, sims = 200000, seed = 1) {
  X <- as.matrix(X); X <- X[stats::complete.cases(X), , drop = FALSE]
  p <- ncol(X); n <- nrow(X)
  if (p < 2) stop("Choose at least two variables.")
  if (n <= p + 1) stop("Not enough rows for ", p, " variables.")
  if (length(lsl) != p || length(usl) != p || any(is.na(lsl) | is.na(usl)) || any(lsl >= usl))
    stop("Enter LSL < USL for every variable.")
  if (is.null(target) || any(is.na(target))) target <- (lsl + usl) / 2
  mu <- colMeans(X); S <- stats::cov(X)
  chi <- stats::qchisq(0.9973, p)
  half <- sqrt(chi * diag(S))
  lpr <- mu - half; upr <- mu + half
  cpm <- (prod(usl - lsl) / prod(upr - lpr))^(1 / p)
  t2 <- n * drop(t(mu - target) %*% solve(S) %*% (mu - target))
  f <- (n - p) / (p * (n - 1)) * t2
  pv <- stats::pf(f, p, n - p, lower.tail = FALSE)
  li <- as.integer(all(lpr >= lsl & upr <= usl))
  set.seed(seed)
  sim <- MASS::mvrnorm(sims, mu, S)
  inside <- rowSums(sweep(sim, 2, lsl, `>=`) & sweep(sim, 2, usl, `<=`)) == p
  per_var <- data.frame(Variable = colnames(X), Mean = mu, `Std. dev.` = sqrt(diag(S)), LSL = lsl, USL = usl, Target = target,
                        Cp = (usl - lsl) / (6 * sqrt(diag(S))),
                        Cpk = pmin(usl - mu, mu - lsl) / (3 * sqrt(diag(S))), check.names = FALSE, row.names = NULL)
  list(vector = data.frame(Index = c("CpM (spread)", "PV (centring, p-value)", "LI (location)", "Expected PPM out of spec (simulated)",
                                     "Observed PPM out of spec"),
                           Value = c(cpm, pv, li, 1e6 * mean(!inside),
                                     1e6 * mean(rowSums(sweep(X, 2, lsl, `>=`) & sweep(X, 2, usl, `<=`)) < p))),
       per_var = per_var, mu = mu, S = S, lsl = lsl, usl = usl, target = target, X = X)
}

cap_multivariate_plot <- function(m, i = 1, j = 2) {
  v <- colnames(m$X)
  ang <- seq(0, 2 * pi, length.out = 200)
  Sij <- m$S[c(i, j), c(i, j)]
  e <- eigen(Sij)
  r <- sqrt(stats::qchisq(0.9973, 2))
  ell <- t(m$mu[c(i, j)] + e$vectors %*% diag(sqrt(pmax(e$values, 0)) * r) %*% rbind(cos(ang), sin(ang)))
  box <- data.frame(xmin = m$lsl[i], xmax = m$usl[i], ymin = m$lsl[j], ymax = m$usl[j])
  ggplot2::ggplot() +
    ggplot2::geom_rect(data = box, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#d8ecd8", colour = "#2e7d32", alpha = 0.5) +
    ggplot2::geom_point(data = data.frame(x = m$X[, i], y = m$X[, j]), ggplot2::aes(x, y), colour = "#4a6fa5", alpha = 0.7) +
    ggplot2::geom_path(data = data.frame(x = ell[, 1], y = ell[, 2]), ggplot2::aes(x, y), colour = "#c0392b", linewidth = 1) +
    ggplot2::geom_point(data = data.frame(x = m$target[i], y = m$target[j]), ggplot2::aes(x, y), shape = 3, size = 4, stroke = 1.5) +
    ggplot2::labs(title = "Process region vs tolerance region", x = v[i], y = v[j],
                  subtitle = "Green box: specifications; red: 99.73% process ellipse; cross: target") +
    ggplot2::theme_bw(base_size = 12) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}
