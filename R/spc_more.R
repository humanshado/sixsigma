# More control charts: Laney P' and U', rare-event G and T charts, Z-MR
# short-run charts, I-MR-R/S (between/within) charts, and charts by stage.

# Laney (2002): attribute charts adjusted for over- or under-dispersion,
# common when sample sizes are large. sigma_z = average moving range of the
# z-scores / 1.128 inflates (or deflates) the usual limits.
spc_laney <- function(d, n, type = "p", k = 3, n_est = NULL) {
  check_counts(d, if (type == "p") n else NULL)
  if (any(n <= 0)) stop("Sample sizes must be positive.")
  n <- rep_len(n, length(d))
  ix <- est_index(length(d), n_est)
  rate <- d / n
  bar <- sum(d[ix]) / sum(n[ix])
  se <- if (type == "p") sqrt(bar * (1 - bar) / n) else sqrt(bar / n)
  z <- (rate - bar) / se
  sz <- mean(abs(diff(z[ix]))) / spc_d2(2)
  lo <- pmax(0, bar - k * se * sz); hi <- bar + k * se * sz
  if (type == "p") hi <- pmin(1, hi)
  ch <- new_chart(sprintf("Laney %s' chart", toupper(type)), if (type == "p") "Proportion" else "Defects per unit",
                  rate, bar, lo, hi, k = k)
  ch$subtitle <- sprintf("Sigma Z = %.3f (%s)", sz, if (sz > 1) "over-dispersion: limits widened" else "under-dispersion: limits narrowed")
  list(charts = list(ch), sigma_z = sz)
}

# G chart: number of opportunities (units, days...) between rare events;
# geometric distribution with probability limits (Benneyan, 2001).
spc_g <- function(g, n_est = NULL, alpha = 0.0027) {
  g <- g[!is.na(g)]
  if (any(g < 0)) stop("Counts between events cannot be negative.")
  if (length(g) < 3) stop("At least three intervals are needed.")
  ix <- est_index(length(g), n_est)
  p <- 1 / (mean(g[ix]) + 1)
  q <- function(pr) stats::qgeom(pr, p)
  ch <- new_chart("G chart (opportunities between events)", "Count between events", g, q(0.5),
                  q(alpha / 2), q(1 - alpha / 2), rules = 1)
  ch$subtitle <- sprintf("Geometric, estimated event probability %.4g; long intervals are good, short ones signal more events", p)
  list(charts = list(ch))
}

# T chart: time between rare events, Weibull fit with probability limits.
spc_t <- function(t, n_est = NULL, alpha = 0.0027) {
  t <- t[!is.na(t)]
  if (any(t <= 0)) stop("Times between events must be positive.")
  if (length(t) < 3) stop("At least three intervals are needed.")
  ix <- est_index(length(t), n_est)
  f <- rel_fit(rel_data(t[ix]), "weibull")
  q <- function(pr) exp(f$mu + f$sigma * log(-log(1 - pr)))
  ch <- new_chart("T chart (time between events)", "Time between events", t, q(0.5), q(alpha / 2), q(1 - alpha / 2), rules = 1)
  ch$subtitle <- sprintf("Weibull fit: shape %.3g, scale %.4g; short times signal more frequent events", 1 / f$sigma, exp(f$mu))
  list(charts = list(ch))
}

# Z-MR chart for short runs: each part type is standardized with its own
# mean and sigma (or a pooled sigma), so different products share one chart.
spc_zmr <- function(x, part, sigma_method = "by_part", k = 3) {
  keep <- !is.na(x) & !is.na(part)
  x <- x[keep]; part <- as.character(part[keep])
  if (length(unique(part)) < 1) stop("Choose the part (product) column.")
  mu <- tapply(x, part, mean)
  mr_by <- lapply(split(x, factor(part, levels = unique(part))), function(v) abs(diff(v)))
  if (sigma_method == "by_part") {
    sg <- vapply(mr_by, function(m) if (length(m)) mean(m) / spc_d2(2) else NA_real_, numeric(1))
    if (any(is.na(sg) | sg == 0)) stop("Each part needs at least two different measurements to estimate its sigma (or use a pooled sigma).")
  } else {
    allmr <- unlist(mr_by)
    if (!length(allmr)) stop("Not enough data to estimate sigma.")
    sg <- stats::setNames(rep(mean(allmr) / spc_d2(2), length(mu)), names(mu))
  }
  z <- (x - mu[part]) / sg[part]
  mr <- abs(diff(z))
  r <- spc_d3(2) / spc_d2(2)
  list(charts = list(
         new_chart("Z chart (standardized by part)", "Z", unname(z), 0, -k, k, paste(part, seq_along(z)), k = k),
         new_chart("Moving range of Z", "MR", c(NA, mr), spc_d2(2), 0, spc_d2(2) * (1 + k * r), k = k)),
       parts = data.frame(Part = names(mu), N = as.vector(table(part)[names(mu)]), Mean = as.vector(mu), Sigma = as.vector(sg)))
}

# I-MR-R/S: for subgroups whose means vary more than the within-subgroup
# spread suggests (e.g. batches). Means are charted with limits from their
# moving range (between); ranges or standard deviations show within spread.
spc_imr_rs <- function(sg, within = "R", k = 3, n_est = NULL) {
  means <- vapply(sg, mean, numeric(1))
  ix <- est_index(length(sg), n_est)
  mrbar <- mean(abs(diff(means[ix])))
  sb <- mrbar / spc_d2(2)
  xbar <- mean(means[ix])
  r <- spc_d3(2) / spc_d2(2)
  w <- if (within == "R") spc_xbar_r(sg, k, n_est)$charts[[2]] else spc_xbar_s(sg, k, n_est)$charts[[2]]
  list(sigma = sb, charts = list(
    new_chart("I chart of subgroup means (between)", "Subgroup mean", unname(means), xbar, xbar - k * sb, xbar + k * sb, names(sg), k = k),
    new_chart("Moving range of means", "MR of means", c(NA, abs(diff(unname(means)))), mrbar,
              max(0, mrbar * (1 - k * r)), mrbar * (1 + k * r), names(sg), k = k),
    w))
}

# Charts by stage: limits are computed separately for each stage (e.g. before
# and after an improvement) and joined into one chart per panel.
spc_by_stage <- function(stage, make) {
  st <- as.character(stage)
  lev <- unique(st[!is.na(st)])
  if (length(lev) < 2) stop("The stage column needs at least two different values.")
  parts <- lapply(lev, function(l) make(which(st == l)))
  out <- parts[[1]]
  for (j in seq_along(out$charts)) {
    chs <- lapply(parts, function(p) p$charts[[j]])
    ch <- chs[[1]]
    for (f in c("y", "cl", "lcl", "ucl", "labels", "violations")) ch[[f]] <- do.call(c, lapply(chs, `[[`, f))
    if (inherits(ch, "spc_cusum")) for (f in c("y2", "value")) ch[[f]] <- do.call(c, lapply(chs, `[[`, f))
    ch$stage_breaks <- cumsum(vapply(chs, function(c) length(c$y), integer(1)))
    ch$stage_names <- lev
    out$charts[[j]] <- ch
  }
  out$sigma <- NULL
  out$stage_sigma <- stats::setNames(vapply(parts, function(p) p$sigma %||% NA_real_, numeric(1)), lev)
  out
}
