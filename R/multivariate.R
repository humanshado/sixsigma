# Multivariate control charts: Hotelling T^2, MEWMA and MCUSUM, plus the
# nonparametric depth-based r, Q and S charts of Liu (1995).
#
# Methods follow the qcr package (Flores, Fernandez-Casal, Naya,
# Tarrio-Saavedra; GPL >= 2), reimplemented here so the app does not depend on
# qcc, fda.usc or mvtnorm. Differences from qcr: MEWMA and MCUSUM on subgroup
# means use the covariance of the means (S / n); the S chart is standardized
# consistently and compared with -z_alpha.

# Split rows into observations (n = 1) or subgroups. Returns a list of
# numeric matrices (rows = observations, columns = variables).
mv_groups <- function(df, vars, group = NULL, size = NULL) {
  if (length(vars) < 2) stop("Choose at least two variables.")
  if (!all(vapply(df[vars], is.numeric, logical(1)))) stop("All variables must be numeric.")
  X <- as.matrix(df[, vars, drop = FALSE])
  keep <- stats::complete.cases(X) & (if (is.null(group)) TRUE else !is.na(df[[group]]))
  X <- X[keep, , drop = FALSE]
  if (!is.null(group)) {
    g <- df[[group]][keep]
    sp <- split(seq_len(nrow(X)), factor(g, levels = unique(g)))
    out <- lapply(sp, function(i) X[i, , drop = FALSE])
  } else if (!is.null(size) && size > 1) {
    idx <- ceiling(seq_len(nrow(X)) / size)
    out <- lapply(split(seq_len(nrow(X)), idx), function(i) X[i, , drop = FALSE])
  } else {
    out <- lapply(seq_len(nrow(X)), function(i) X[i, , drop = FALSE])
    names(out) <- seq_len(nrow(X))
  }
  out
}

# Common statistics: means, reference mean and covariance.
mv_setup <- function(groups, n_ref = NULL) {
  n <- vapply(groups, nrow, integer(1))
  p <- ncol(groups[[1]])
  m_all <- length(groups)
  m <- if (is.null(n_ref) || is.na(n_ref) || n_ref >= m_all) m_all else as.integer(n_ref)
  if (length(unique(n)) != 1) stop("All subgroups must have the same size (", paste(range(n), collapse = " to "), " found).")
  n <- unname(n[1])
  means <- do.call(rbind, lapply(groups, colMeans))
  ref <- seq_len(m)
  if (n == 1) {
    if (m <= p + 1) stop("The reference period needs more than ", p + 1, " observations for ", p, " variables.")
    S <- stats::cov(means[ref, , drop = FALSE])
  } else {
    if (m < 2) stop("The reference period needs at least two subgroups.")
    S <- Reduce(`+`, lapply(groups[ref], stats::cov)) / m
  }
  if (rcond(S) < 1e-12) stop("The covariance matrix is singular: some variables are perfectly correlated or constant.")
  list(means = means, center = colMeans(means[ref, , drop = FALSE]), S = S, Sinv = solve(S),
       n = n, p = p, m = m, m_all = m_all, labels = names(groups), vars = colnames(means))
}

mv_quad <- function(d, Sinv) rowSums((d %*% Sinv) * d)

mv_t2 <- function(groups, alpha = 0.0027, n_ref = NULL) {
  s <- mv_setup(groups, n_ref)
  d <- sweep(s$means, 2, s$center)
  t2 <- s$n * mv_quad(d, s$Sinv)
  m <- s$m; n <- s$n; p <- s$p
  if (n == 1) {
    ucl1 <- (m - 1)^2 / m * stats::qbeta(1 - alpha, p / 2, (m - p - 1) / 2)
    ucl2 <- p * (m + 1) * (m - 1) / (m * (m - p)) * stats::qf(1 - alpha, p, m - p)
  } else {
    df2 <- m * n - m - p + 1
    ucl1 <- p * (m - 1) * (n - 1) / df2 * stats::qf(1 - alpha, p, df2)
    ucl2 <- p * (m + 1) * (n - 1) / df2 * stats::qf(1 - alpha, p, df2)
  }
  ucl <- ifelse(seq_len(s$m_all) <= m, ucl1, ucl2)
  ch <- new_chart("Hotelling T² chart", "T²", t2, NA, 0, ucl, s$labels, rules = 1)
  ch$ref_end <- m
  ch$subtitle <- sprintf("%d variables, %s; α = %g", p, if (n == 1) "individual observations" else paste("subgroups of", n), alpha)
  list(charts = list(ch), setup = s, contributions = mv_contributions(s, ch))
}

# For each signal, how much T^2 drops when each variable is left out:
# large drops point to the variables responsible.
mv_contributions <- function(s, ch) {
  hit <- which(lengths(ch$violations) > 0)
  if (!length(hit) || s$p < 2) return(NULL)
  d <- sweep(s$means, 2, s$center)
  out <- lapply(hit, function(i) {
    full <- s$n * sum((d[i, ] %*% s$Sinv) * d[i, ])
    drop <- vapply(seq_len(s$p), function(j) {
      Sj <- s$S[-j, -j, drop = FALSE]
      full - s$n * sum((d[i, -j] %*% solve(Sj)) * d[i, -j])
    }, numeric(1))
    data.frame(Sample = s$labels[i], `T²` = full, check.names = FALSE,
               t(stats::setNames(drop, s$vars)),
               `Largest contributor` = s$vars[which.max(drop)])
  })
  do.call(rbind, out)
}

# MEWMA upper limits h for an in-control ARL of about 200 (Prabhu & Runger,
# 1997), as tabulated in qcr. Rows: lambda 0.1..0.9; columns: p = 2..10.
mewma_h_table <- matrix(c(
  8.6336, 9.6476, 10.083, 10.3114, 10.4405, 10.5152, 10.5581, 10.5816, 10.5932,
  10.814, 11.8961, 12.3505, 12.5845, 12.7143, 12.788, 12.8297, 12.8524, 12.8635,
  12.7231, 13.8641, 14.3359, 14.576, 14.7077, 14.7818, 14.8234, 14.846, 14.857,
  14.5363, 15.7293, 16.217, 16.4629, 16.5965, 16.6711, 16.7127, 16.7352, 16.7463,
  16.2634, 17.5038, 18.0063, 18.2578, 18.3935, 18.4687, 18.5105, 18.5331, 18.5442,
  17.9269, 19.2113, 19.7276, 19.9845, 20.1223, 20.1982, 20.2403, 20.2631, 20.2743,
  19.541, 20.8665, 21.396, 21.6581, 21.798, 21.8747, 21.9171, 21.9401, 21.9515,
  21.1152, 22.4796, 23.0217, 23.2887, 23.4307, 23.5082, 23.551, 23.5742, 23.5858,
  22.6565, 24.0579, 24.6119, 24.8838, 25.0278, 25.1062, 25.1493, 25.1728, 25.1846),
  nrow = 9, dimnames = list(sprintf("%.1f", seq(0.1, 0.9, 0.1)), 2:10))

mewma_h <- function(lambda, p) {
  key <- sprintf("%.1f", lambda)
  if (!(key %in% rownames(mewma_h_table)) || !(as.character(p) %in% colnames(mewma_h_table)) ||
      abs(lambda - round(lambda, 1)) > 1e-9) return(NA_real_)
  mewma_h_table[key, as.character(p)]
}

mv_mewma <- function(groups, lambda = 0.1, h = NA, n_ref = NULL) {
  s <- mv_setup(groups, n_ref)
  if (!isTRUE(lambda > 0 && lambda <= 1)) stop("Lambda must be between 0 and 1.")
  if (is.na(h)) h <- mewma_h(lambda, s$p)
  if (is.na(h)) stop("No tabulated limit for λ = ", lambda, " and ", s$p,
                     " variables (table covers λ = 0.1-0.9 in steps of 0.1 and 2-10 variables). Enter h.")
  Sigma <- s$S / s$n
  d <- sweep(s$means, 2, s$center)
  z <- matrix(0, s$m_all, s$p); stat <- numeric(s$m_all); prev <- rep(0, s$p)
  for (i in seq_len(s$m_all)) {
    z[i, ] <- lambda * d[i, ] + (1 - lambda) * prev; prev <- z[i, ]
    Sz <- Sigma * lambda * (1 - (1 - lambda)^(2 * i)) / (2 - lambda)
    stat[i] <- sum((z[i, ] %*% solve(Sz)) * z[i, ])
  }
  ch <- new_chart(sprintf("MEWMA chart (λ = %g)", lambda), "MEWMA statistic", stat, NA, 0, h, s$labels, rules = 1)
  ch$ref_end <- s$m
  ch$subtitle <- sprintf("%d variables; h = %.4g (in-control ARL ≈ 200)", s$p, h)
  list(charts = list(ch), setup = s)
}

# Crosier's multivariate CUSUM.
mv_mcusum <- function(groups, k = 0.5, h = 5.5, n_ref = NULL) {
  s <- mv_setup(groups, n_ref)
  if (!isTRUE(k >= 0 && h > 0)) stop("k must be non-negative and h positive.")
  Sinv <- s$Sinv * s$n
  d <- sweep(s$means, 2, s$center)
  sv <- rep(0, s$p); stat <- numeric(s$m_all)
  for (i in seq_len(s$m_all)) {
    v <- sv + d[i, ]
    ci <- sqrt(sum((v %*% Sinv) * v))
    sv <- if (ci > k) v * (1 - k / ci) else rep(0, s$p)
    stat[i] <- sqrt(sum((sv %*% Sinv) * sv))
  }
  ch <- new_chart(sprintf("MCUSUM chart (k = %g, h = %g)", k, h), "MCUSUM statistic", stat, NA, 0, h, s$labels, rules = 1)
  ch$ref_end <- s$m
  ch$subtitle <- sprintf("%d variables (Crosier's multivariate CUSUM)", s$p)
  list(charts = list(ch), setup = s)
}

# ---- Data depth and the r, Q, S charts --------------------------------------

depth_mahalanobis <- function(x, G) {
  mu <- colMeans(G); Sinv <- solve(stats::cov(G))
  1 / (1 + mv_quad(sweep(x, 2, mu), Sinv))
}

# Affine-invariant spatial depth: 1 - || mean of unit vectors from x to G ||.
depth_spatial <- function(x, G) {
  R <- chol(stats::cov(G)); Ri <- solve(R)
  xs <- x %*% Ri; Gs <- G %*% Ri
  vapply(seq_len(nrow(xs)), function(i) {
    d <- sweep(Gs, 2, xs[i, ], FUN = function(a, b) b - a)
    nr <- sqrt(rowSums(d^2)); keep <- nr > 1e-12
    u <- d[keep, , drop = FALSE] / nr[keep]
    1 - sqrt(sum(colSums(u)^2)) / nrow(Gs)
  }, numeric(1))
}

depth_fun <- function(method) switch(method, mahalanobis = depth_mahalanobis, spatial = depth_spatial,
                                     stop("Unknown depth method."))

# Rank of each new point's depth among the reference sample's depths:
# r = share of reference points that are at most as deep. Small r = outlying.
depth_ranks <- function(Y, G, method = "mahalanobis") {
  f <- depth_fun(method)
  dG <- f(G, G); dY <- f(Y, G)
  vapply(dY, function(v) mean(dG <= v), numeric(1))
}

np_setup <- function(df, vars, n_ref) {
  if (length(vars) < 2) stop("Choose at least two variables.")
  X <- as.matrix(df[, vars, drop = FALSE])
  keep <- stats::complete.cases(X)
  X <- X[keep, , drop = FALSE]
  if (is.na(n_ref) || n_ref < length(vars) + 2) stop("The reference sample needs at least ", length(vars) + 2, " rows.")
  if (n_ref >= nrow(X)) stop("Leave some rows after the reference sample to monitor.")
  list(G = X[seq_len(n_ref), , drop = FALSE], Y = X[-seq_len(n_ref), , drop = FALSE],
       rows = which(keep)[-seq_len(n_ref)], keep = keep)
}

np_r_chart <- function(df, vars, n_ref, alpha = 0.05, method = "mahalanobis") {
  s <- np_setup(df, vars, n_ref)
  r <- depth_ranks(s$Y, s$G, method)
  ch <- new_chart("r chart (depth ranks of individual observations)", "r (rank of depth)", r, 0.5, alpha, NA,
                  s$rows, rules = 1)
  ch$subtitle <- sprintf("%s depth; reference sample = first %d rows; signal when r < α = %g",
                         tools::toTitleCase(method), n_ref, alpha)
  list(charts = list(ch))
}

np_q_chart <- function(df, vars, n_ref, size = NULL, group = NULL, alpha = 0.05, method = "mahalanobis") {
  s <- np_setup(df, vars, n_ref)
  r <- depth_ranks(s$Y, s$G, method)
  g <- if (!is.null(group)) df[[group]][s$keep][-seq_len(n_ref)] else {
    if (is.null(size) || is.na(size) || size < 2) stop("Subgroup size must be at least 2.")
    ceiling(seq_along(r) / size)
  }
  sp <- split(r, factor(g, levels = unique(g)))
  q <- vapply(sp, mean, numeric(1)); nn <- lengths(sp); m <- nrow(s$G)
  z <- stats::qnorm(1 - alpha)
  lcl <- ifelse(nn < 5, (factorial(nn) * alpha)^(1 / nn) / nn, 0.5 - z * sqrt((1 / m + 1 / nn) / 12))
  ch <- new_chart("Q chart (mean depth rank per subgroup)", "Q (mean rank)", unname(q), 0.5, lcl, NA,
                  names(sp), rules = 1)
  ch$subtitle <- sprintf("%s depth; reference sample = first %d rows; α = %g",
                         tools::toTitleCase(method), n_ref, alpha)
  list(charts = list(ch))
}

np_s_chart <- function(df, vars, n_ref, alpha = 0.05, method = "mahalanobis") {
  s <- np_setup(df, vars, n_ref)
  r <- depth_ranks(s$Y, s$G, method)
  m <- nrow(s$G); n <- seq_along(r)
  Sn <- cumsum(r - 0.5) / sqrt(n^2 * (1 / m + 1 / n) / 12)
  ch <- new_chart("S chart (standardized cumulative sum of depth ranks)", "Standardized S", Sn, 0,
                  -stats::qnorm(1 - alpha), NA, s$rows, rules = 1)
  ch$subtitle <- sprintf("%s depth; detects gradual drift; signal when S < -z(α) = %.3g",
                         tools::toTitleCase(method), -stats::qnorm(1 - alpha))
  list(charts = list(ch))
}
