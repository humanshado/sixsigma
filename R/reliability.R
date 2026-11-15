# Reliability (life data) analysis with right-censored data.
#
# Distributions are fitted by maximum likelihood in log-location-scale form
# (Meeker & Escobar, Statistical Methods for Reliability Data):
#   y = log(t) (or t for the normal), z = (y - mu) / sigma,
#   Weibull = smallest extreme value in log time, lognormal = normal in log
#   time, log-logistic = logistic in log time, exponential = Weibull with
#   sigma = 1. Confidence intervals use the normal approximation with the
#   delta method on (mu, log sigma), as Minitab does by default.

rel_dists <- c("Weibull" = "weibull", "Lognormal" = "lognormal", "Exponential" = "exponential",
               "Log-logistic" = "loglogistic", "Normal" = "normal")

rel_std <- function(dist) {
  switch(dist,
    weibull = , exponential = list(
      d = function(z) exp(z - exp(z)), p = function(z) 1 - exp(-exp(z)),
      q = function(p) log(-log(1 - p)), logS = function(z) -exp(z)),
    lognormal = , normal = list(
      d = stats::dnorm, p = stats::pnorm, q = stats::qnorm,
      logS = function(z) stats::pnorm(z, lower.tail = FALSE, log.p = TRUE)),
    loglogistic = list(
      d = stats::dlogis, p = stats::plogis, q = stats::qlogis,
      logS = function(z) stats::plogis(z, lower.tail = FALSE, log.p = TRUE)),
    stop("Unknown distribution."))
}
rel_is_log <- function(dist) dist != "normal"

# Prepare life data: time, failure indicator (TRUE = failure, FALSE =
# right-censored) and optional frequencies.
rel_data <- function(time, failed = NULL, freq = NULL) {
  if (!is.numeric(time)) stop("Times must be numeric.")
  n <- length(time)
  failed <- if (is.null(failed)) rep(TRUE, n) else as.logical(failed)
  freq <- if (is.null(freq)) rep(1, n) else freq
  keep <- !is.na(time) & !is.na(failed) & !is.na(freq) & freq > 0
  d <- data.frame(t = time[keep], f = failed[keep], w = freq[keep])
  if (sum(d$w[d$f]) < 2) stop("At least two failures are needed.")
  d
}

rel_fit <- function(d, dist = "weibull") {
  std <- rel_std(dist); lg <- rel_is_log(dist)
  if (lg && any(d$t <= 0)) stop("Times must be positive for the ", names(rel_dists)[rel_dists == dist], " distribution.")
  y <- if (lg) log(d$t) else d$t
  fixed_sigma <- dist == "exponential"
  nll <- function(par) {
    mu <- par[1]; ls <- if (fixed_sigma) 0 else par[2]; s <- exp(ls)
    if (ls < -12 || ls > 8 || abs(mu) > 1e6) return(1e300)  # keep the search away from degenerate fits
    z <- (y - mu) / s
    ll_f <- log(pmax(std$d(z), 1e-300)) - ls - (if (lg) y else 0)
    ll_c <- std$logS(z)
    -sum(d$w * ifelse(d$f, ll_f, ll_c))
  }
  yf <- y[d$f]
  start <- c(mean(yf) + if (any(!d$f)) 0.5 * stats::sd(y) else 0, log(max(stats::sd(y), 1e-6)))
  if (fixed_sigma) start <- start[1]
  # Heavily censored data (e.g. warranty returns) need a start near the
  # probability-plot line: a least-squares fit to Kaplan-Meier estimates.
  km_start <- tryCatch({
    tt <- sort(unique(y[d$f])); atrisk <- vapply(tt, function(v) sum(d$w[y >= v]), 0)
    dead <- vapply(tt, function(v) sum(d$w[y == v & d$f]), 0)
    Fk <- 1 - cumprod(1 - dead / atrisk); ok <- Fk > 0 & Fk < 1
    if (sum(ok) >= 2) { cf <- stats::coef(stats::lm(tt[ok] ~ std$q(Fk[ok]))); unname(c(cf[1], log(max(cf[2], 1e-3)))) } else NULL
  }, error = function(e) NULL)
  o <- stats::optim(start, nll, method = "BFGS", hessian = TRUE, control = list(maxit = 1000, reltol = 1e-12))
  if (!is.null(km_start) && all(is.finite(km_start))) {
    ks <- if (fixed_sigma) km_start[1] else km_start
    o2 <- tryCatch(stats::optim(ks, nll, method = "BFGS", hessian = TRUE, control = list(maxit = 1000, reltol = 1e-12)),
                   error = function(e) NULL)
    if (!is.null(o2) && o2$convergence == 0 && is.finite(o2$value) && (o$convergence != 0 || !is.finite(o$value) || o2$value < o$value)) o <- o2
  }
  if (o$convergence != 0 || !is.finite(o$value)) {
    # Heavily censored data can defeat BFGS from the default start: try
    # Nelder-Mead first (and a later start), then polish with BFGS.
    best <- NULL
    for (st in list(start, start + c(stats::sd(y), 0)[seq_along(start)], o$par)) {
      nm <- tryCatch(stats::optim(st, nll, method = if (length(st) > 1) "Nelder-Mead" else "BFGS",
                                  control = list(maxit = 5000, reltol = 1e-12)), error = function(e) NULL)
      if (!is.null(nm) && is.finite(nm$value) && (is.null(best) || nm$value < best$value)) best <- nm
    }
    if (!is.null(best)) o <- stats::optim(best$par, nll, method = "BFGS", hessian = TRUE, control = list(maxit = 1000, reltol = 1e-12))
    if (o$convergence != 0 && !is.null(best) && abs(o$value - best$value) < 1e-6 * abs(best$value)) o$convergence <- 0
    if (o$convergence != 0 || !is.finite(o$value)) stop("The ", dist, " fit did not converge.")
  }
  V <- tryCatch(solve(o$hessian), error = function(e) matrix(NA, length(start), length(start)))
  if (fixed_sigma) { par <- c(o$par, 0); V <- rbind(cbind(V, 0), 0) } else par <- o$par
  structure(list(dist = dist, label = names(rel_dists)[rel_dists == dist], mu = par[1], logsigma = par[2],
                 sigma = exp(par[2]), V = V, loglik = -o$value, k = length(start), data = d, log = lg, std = std),
            class = "rel_fit")
}

# Usual parameters with confidence intervals.
rel_parameters <- function(f, conf = 0.95) {
  z <- stats::qnorm(1 - (1 - conf) / 2)
  se_mu <- sqrt(f$V[1, 1]); se_ls <- sqrt(f$V[2, 2])
  ci <- function(est, se, trans = identity) sort(trans(est + c(-1, 1) * z * se))
  row <- function(name, val, lo, hi) data.frame(Parameter = name, Estimate = val, `CI lower` = lo, `CI upper` = hi, check.names = FALSE)
  switch(f$dist,
    weibull = rbind(row("Shape (beta)", 1 / f$sigma, exp(-f$logsigma - z * se_ls), exp(-f$logsigma + z * se_ls)),
                    row("Scale (eta)", exp(f$mu), exp(f$mu - z * se_mu), exp(f$mu + z * se_mu))),
    exponential = row("Mean", exp(f$mu), exp(f$mu - z * se_mu), exp(f$mu + z * se_mu)),
    lognormal = rbind(row("Location (log-mean)", f$mu, f$mu - z * se_mu, f$mu + z * se_mu),
                      row("Scale (log-sd)", f$sigma, exp(f$logsigma - z * se_ls), exp(f$logsigma + z * se_ls))),
    loglogistic = rbind(row("Location", f$mu, f$mu - z * se_mu, f$mu + z * se_mu),
                        row("Scale", f$sigma, exp(f$logsigma - z * se_ls), exp(f$logsigma + z * se_ls))),
    normal = rbind(row("Mean", f$mu, f$mu - z * se_mu, f$mu + z * se_mu),
                   row("Std. dev.", f$sigma, exp(f$logsigma - z * se_ls), exp(f$logsigma + z * se_ls))))
}

# Percentiles (B-lives) with confidence intervals.
rel_percentiles <- function(f, p = c(0.01, 0.05, 0.1, 0.5), conf = 0.95) {
  z <- stats::qnorm(1 - (1 - conf) / 2)
  zp <- f$std$q(p)
  est <- f$mu + f$sigma * zp
  se <- sqrt(f$V[1, 1] + (zp * f$sigma)^2 * f$V[2, 2] + 2 * zp * f$sigma * f$V[1, 2])
  tr <- if (f$log) exp else identity
  data.frame(Percent = 100 * p, Life = tr(est), `CI lower` = tr(est - z * se), `CI upper` = tr(est + z * se),
             check.names = FALSE)
}

# Reliability R(t) = probability of surviving past t, with confidence bounds
# computed on the standardized scale.
rel_reliability <- function(f, t, conf = 0.95) {
  z <- stats::qnorm(1 - (1 - conf) / 2)
  y <- if (f$log) log(t) else t
  zz <- (y - f$mu) / f$sigma
  # Gradient of zz w.r.t. (mu, log sigma) is (-1/sigma, -zz).
  se <- sqrt(f$V[1, 1] / f$sigma^2 + zz^2 * f$V[2, 2] + 2 * zz / f$sigma * f$V[1, 2])
  data.frame(Time = t, Reliability = 1 - f$std$p(zz), `CI lower` = 1 - f$std$p(zz + z * se),
             `CI upper` = 1 - f$std$p(zz - z * se), `Probability of failure` = f$std$p(zz), check.names = FALSE)
}

rel_mttf <- function(f, conf = 0.95) {
  m <- function(mu, ls) {
    s <- exp(ls)
    switch(f$dist, weibull = exp(mu) * gamma(1 + s), exponential = exp(mu), lognormal = exp(mu + s^2 / 2),
           loglogistic = if (s < 1) exp(mu) * pi * s / sin(pi * s) else Inf, normal = mu)
  }
  est <- m(f$mu, f$logsigma)
  if (!is.finite(est)) return(data.frame(Statistic = "Mean time to failure (MTTF)", Estimate = NA, `CI lower` = NA,
                                         `CI upper` = NA, check.names = FALSE))
  h <- 1e-6
  lg <- f$dist != "normal"
  g <- function(mu, ls) if (lg) log(m(mu, ls)) else m(mu, ls)
  grad <- c((g(f$mu + h, f$logsigma) - g(f$mu - h, f$logsigma)) / (2 * h),
            (g(f$mu, f$logsigma + h) - g(f$mu, f$logsigma - h)) / (2 * h))
  se <- sqrt(drop(t(grad) %*% f$V %*% grad))
  z <- stats::qnorm(1 - (1 - conf) / 2)
  b <- g(f$mu, f$logsigma) + c(-1, 1) * z * se
  data.frame(Statistic = "Mean time to failure (MTTF)", Estimate = est,
             `CI lower` = if (lg) exp(b[1]) else b[1], `CI upper` = if (lg) exp(b[2]) else b[2], check.names = FALSE)
}

rel_hazard <- function(f, t) {
  y <- if (f$log) log(t) else t
  z <- (y - f$mu) / f$sigma
  dens <- f$std$d(z) / (f$sigma * (if (f$log) t else 1))
  dens / (1 - f$std$p(z))
}

# Nonparametric estimates of the failure probability at each failure time:
# Johnson's adjusted ranks with Bernard's median-rank approximation.
rel_plot_positions <- function(d) {
  o <- order(d$t, !d$f)
  t <- rep(d$t[o], d$w[o]); f <- rep(d$f[o], d$w[o])
  n <- length(t); prev <- 0; rank <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    if (f[i]) {
      rev <- n - i + 1
      prev <- prev + (n + 1 - prev) / (1 + rev)
      rank[i] <- prev
    }
  }
  keep <- f
  data.frame(t = t[keep], F = (rank[keep] - 0.3) / (n + 0.4))
}

# Kaplan-Meier estimate with Greenwood confidence limits.
rel_kaplan_meier <- function(d, conf = 0.95) {
  z <- stats::qnorm(1 - (1 - conf) / 2)
  times <- sort(unique(d$t[d$f]))
  n_tot <- sum(d$w); S <- 1; gw <- 0
  rows <- lapply(times, function(tt) {
    at_risk <- sum(d$w[d$t >= tt]); fails <- sum(d$w[d$t == tt & d$f])
    S <<- S * (1 - fails / at_risk)
    if (at_risk > fails) gw <<- gw + fails / (at_risk * (at_risk - fails))
    se <- S * sqrt(gw)
    data.frame(Time = tt, `At risk` = at_risk, Failures = fails, Reliability = S,
               `CI lower` = max(0, S - z * se), `CI upper` = min(1, S + z * se), check.names = FALSE)
  })
  do.call(rbind, rows)
}

rel_compare <- function(d) {
  out <- lapply(rel_dists, function(ds) {
    f <- tryCatch(rel_fit(d, ds), error = function(e) NULL)
    if (is.null(f)) return(NULL)
    data.frame(Distribution = f$label, Code = ds, `Log-likelihood` = f$loglik, AIC = -2 * f$loglik + 2 * f$k,
               check.names = FALSE)
  })
  out <- do.call(rbind, out)
  out <- out[order(out$AIC), ]
  rownames(out) <- NULL
  out
}

# Probability plot on the distribution's linearizing scales.
rel_probability_plot <- function(f, conf = 0.95) {
  pp <- rel_plot_positions(f$data)
  tr <- if (f$log) log else identity
  pp$x <- tr(pp$t); pp$y <- f$std$q(pp$F)
  ps <- c(0.001, 0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9, 0.95, 0.99, 0.999)
  pr <- range(c(pp$F, 0.01, 0.99))
  grid_p <- seq(max(1e-4, pr[1] / 2), min(0.9999, 1 - (1 - pr[2]) / 2), length.out = 200)
  band <- rel_percentiles(f, grid_p, conf)
  band$y <- f$std$q(grid_p)
  band$x <- tr(band$Life); band$lo <- tr(band$`CI lower`); band$hi <- tr(band$`CI upper`)
  band <- band[is.finite(band$lo) & is.finite(band$hi), ]
  xb <- if (f$log) {
    r <- range(c(band$Life, pp$t), na.rm = TRUE)
    b <- as.vector(outer(c(1, 2, 5), 10^(floor(log10(r[1])):ceiling(log10(r[2])))))
    b[b >= r[1] / 1.5 & b <= r[2] * 1.5]
  } else NULL
  g <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = band, ggplot2::aes(y = y, xmin = lo, xmax = hi), fill = "#c9d8ea", alpha = 0.7, orientation = "y") +
    ggplot2::geom_line(data = band, ggplot2::aes(x, y), colour = "#4a6fa5", linewidth = 1, orientation = "y") +
    ggplot2::geom_point(data = pp, ggplot2::aes(x, y), colour = "#c0392b", size = 2.3) +
    ggplot2::scale_y_continuous(breaks = f$std$q(ps), labels = paste0(100 * ps, "%"), name = "Percent failed") +
    ggplot2::labs(title = sprintf("%s probability plot", f$label),
                  subtitle = sprintf("Points: adjusted median ranks (failures); line: ML fit with %g%% confidence band", 100 * conf),
                  x = "Time") +
    ggplot2::theme_bw(base_size = 12) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  if (f$log) g <- g + ggplot2::scale_x_continuous(breaks = log(xb), labels = format(xb, big.mark = ",", scientific = FALSE, trim = TRUE))
  g
}

rel_curves_plot <- function(f, conf = 0.95) {
  tmax <- max(f$data$t) * 1.2
  tt <- seq(if (f$log) tmax / 400 else min(0, min(f$data$t)), tmax, length.out = 300)
  r <- rel_reliability(f, tt, conf)
  km <- rel_kaplan_meier(f$data, conf)
  h <- data.frame(Time = tt, h = rel_hazard(f, tt))
  y <- if (f$log) log(tt) else tt
  dens <- data.frame(Time = tt, d = f$std$d((y - f$mu) / f$sigma) / (f$sigma * (if (f$log) tt else 1)))
  th <- ggplot2::theme_bw(base_size = 12)
  p1 <- ggplot2::ggplot(r, ggplot2::aes(Time)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = `CI lower`, ymax = `CI upper`), fill = "#c9d8ea", alpha = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = Reliability), colour = "#4a6fa5", linewidth = 1) +
    ggplot2::geom_step(data = rbind(data.frame(Time = 0, Reliability = 1), km[, c("Time", "Reliability")]),
                       ggplot2::aes(Time, Reliability), colour = "#c0392b", linetype = 2) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::labs(title = "Reliability (survival) function", subtitle = "Blue: fitted; red dashed: Kaplan-Meier", y = "Reliability") + th
  p2 <- ggplot2::ggplot(h, ggplot2::aes(Time, h)) + ggplot2::geom_line(colour = "#4a6fa5", linewidth = 1) +
    ggplot2::labs(title = "Hazard function", subtitle = "Instantaneous failure rate", y = "Hazard rate") + th
  p3 <- ggplot2::ggplot(dens, ggplot2::aes(Time, d)) + ggplot2::geom_area(fill = "#c9d8ea", colour = "#4a6fa5") +
    ggplot2::labs(title = "Probability density function", y = "Density") + th
  list(p1, p2, p3)
}
