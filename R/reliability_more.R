# Accelerated life testing, warranty (Nevada chart) analysis and reliability
# demonstration test planning. Methods follow Meeker & Escobar, Statistical
# Methods for Reliability Data (1998), and Nelson, Accelerated Testing (1990).

alt_relationships <- c("Arrhenius (temperature, °C)" = "arrhenius", "Inverse power (log stress)" = "power",
                       "Linear" = "linear", "Exponential (stress)" = "exponential")

alt_transform <- function(s, rel) switch(rel,
  arrhenius = { if (any(s <= -273.15)) stop("Temperatures must be above absolute zero."); 11604.518 / (s + 273.15) },
  power = { if (any(s <= 0)) stop("Stresses must be positive for the inverse power law."); log(s) },
  linear = , exponential = s,
  stop("Unknown relationship."))

# mu(x) = b0 + b1 * g(stress); common sigma. Right-censoring as in rel_fit.
alt_fit <- function(time, failed, stress, freq = NULL, dist = "weibull", rel = "arrhenius") {
  if (!dist %in% c("weibull", "lognormal", "exponential", "loglogistic")) stop("Use a Weibull, lognormal, exponential or log-logistic distribution.")
  failed <- if (is.null(failed)) rep(TRUE, length(time)) else as.logical(failed)
  freq <- if (is.null(freq)) rep(1, length(time)) else freq
  keep <- !is.na(time) & !is.na(failed) & !is.na(stress) & !is.na(freq) & freq > 0
  d <- data.frame(t = time[keep], f = failed[keep], w = freq[keep], s = stress[keep])
  if (any(d$t <= 0)) stop("Times must be positive.")
  if (length(unique(d$s)) < 2) stop("Accelerated life analysis needs at least two stress levels.")
  if (sum(d$w[d$f]) < 3) stop("At least three failures are needed.")
  if (length(unique(d$s[d$f])) < 2) stop("Failures are needed at two or more stress levels.")
  x <- alt_transform(d$s, rel); y <- log(d$t)
  xc <- mean(x); xs <- stats::sd(x)            # centre for numerical stability
  u <- (x - xc) / xs
  std <- rel_std(dist); fixed <- dist == "exponential"
  nll <- function(p) {
    mu <- p[1] + p[2] * u; ls <- if (fixed) 0 else p[3]
    z <- (y - mu) / exp(ls)
    -sum(d$w * ifelse(d$f, log(pmax(std$d(z), 1e-300)) - ls - y, std$logS(z)))
  }
  st <- stats::coef(stats::lm(y ~ u, weights = d$w))
  st <- c(st, if (!fixed) log(max(stats::sd(y), 0.1)))
  o <- stats::optim(st, nll, method = "Nelder-Mead", control = list(maxit = 10000, reltol = 1e-12))
  o <- stats::optim(o$par, nll, method = "BFGS", hessian = TRUE, control = list(maxit = 2000, reltol = 1e-14))
  if (!is.finite(o$value)) stop("The accelerated life model did not converge.")
  Vc <- tryCatch(solve(o$hessian), error = function(e) matrix(NA, length(st), length(st)))
  # Back-transform the centred coefficients: b1 = a1/xs, b0 = a0 - a1*xc/xs.
  J <- rbind(c(1, -xc / xs), c(0, 1 / xs))
  if (!fixed) J <- rbind(cbind(J, 0), c(0, 0, 1))
  par <- as.vector(J %*% o$par); V <- J %*% Vc %*% t(J)
  if (fixed) { par <- c(par, 0); V <- rbind(cbind(V, 0), 0) }
  structure(list(b0 = par[1], b1 = par[2], logsigma = par[3], sigma = exp(par[3]), V = V, loglik = -o$value,
                 dist = dist, rel = rel, std = std, data = d, k = length(st)), class = "alt_fit")
}

# Location and its variance at a stress (delta method).
alt_mu <- function(f, s) {
  x <- alt_transform(s, f$rel)
  g <- cbind(1, x, 0)
  list(mu = f$b0 + f$b1 * x, var = rowSums((g %*% f$V) * g), x = x)
}

alt_percentile <- function(f, s, p, conf = 0.95) {
  z <- stats::qnorm(1 - (1 - conf) / 2); q <- f$std$q(p)
  x <- alt_transform(s, f$rel)
  g <- cbind(1, x, q * f$sigma)
  yp <- f$b0 + f$b1 * x + q * f$sigma
  se <- sqrt(rowSums((g %*% f$V) * g))
  data.frame(Stress = s, Percent = 100 * p, Estimate = exp(yp), Lower = exp(yp - z * se), Upper = exp(yp + z * se))
}

alt_reliability <- function(f, s, t, conf = 0.95) {
  z <- stats::qnorm(1 - (1 - conf) / 2)
  x <- alt_transform(s, f$rel)
  zz <- (log(t) - f$b0 - f$b1 * x) / f$sigma
  g <- cbind(-1 / f$sigma, -x / f$sigma, -zz)
  se <- sqrt(rowSums((g %*% f$V) * g))
  data.frame(Stress = s, Time = t, Reliability = 1 - f$std$p(zz),
             Lower = 1 - f$std$p(zz + z * se), Upper = 1 - f$std$p(zz - z * se))
}

alt_summary <- function(f, use, conf = 0.95) {
  z <- stats::qnorm(1 - (1 - conf) / 2); se <- sqrt(diag(f$V))
  rows <- data.frame(Parameter = c("Intercept (b0)", switch(f$rel, arrhenius = "Activation energy, eV (b1)", power = "Log stress (b1)", "Stress (b1)"),
                                   if (f$dist != "exponential") (if (f$dist == "weibull") "Scale σ (shape β = 1/σ)" else "Scale σ")),
                     Estimate = c(f$b0, f$b1, if (f$dist != "exponential") f$sigma),
                     `SE` = c(se[1], se[2], if (f$dist != "exponential") f$sigma * se[3]), check.names = FALSE)
  rows$Lower <- c(f$b0 - z * se[1], f$b1 - z * se[2], if (f$dist != "exponential") f$sigma * exp(-z * se[3]))
  rows$Upper <- c(f$b0 + z * se[1], f$b1 + z * se[2], if (f$dist != "exponential") f$sigma * exp(z * se[3]))
  levels <- sort(unique(f$data$s))
  af <- exp(alt_mu(f, use)$mu - alt_mu(f, levels)$mu)
  acc <- data.frame(Stress = levels, Failures = vapply(levels, function(s) sum(f$data$w[f$data$s == s & f$data$f]), numeric(1)),
                    Censored = vapply(levels, function(s) sum(f$data$w[f$data$s == s & !f$data$f]), numeric(1)),
                    `Acceleration factor vs use` = af, check.names = FALSE)
  list(parameters = rows, acceleration = acc, loglik = f$loglik)
}

alt_plot <- function(f, use, conf = 0.95) {
  levels <- sort(unique(c(f$data$s, use)))
  grid <- seq(min(levels), max(levels), length.out = 100)
  pc <- do.call(rbind, lapply(c(0.1, 0.5), function(p) cbind(alt_percentile(f, grid, p, conf), Line = sprintf("B%g", 100 * p))))
  pts <- f$data
  ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = pc, ggplot2::aes(Stress, ymin = Lower, ymax = Upper, fill = Line), alpha = 0.15) +
    ggplot2::geom_line(data = pc, ggplot2::aes(Stress, Estimate, colour = Line), linewidth = 1) +
    ggplot2::geom_point(data = pts, ggplot2::aes(s, t, shape = ifelse(f, "Failure", "Censored")), size = 2.5) +
    ggplot2::geom_vline(xintercept = use, linetype = "dashed", colour = "grey40") +
    ggplot2::scale_y_log10() + ggplot2::scale_shape_manual(values = c(Failure = 16, Censored = 1)) +
    ggplot2::labs(title = "Life vs stress", x = "Stress", y = "Time (log scale)", shape = NULL, colour = NULL, fill = NULL) +
    ggplot2::theme_minimal(base_size = 13)
}

# ---- Warranty (Nevada chart) ----------------------------------------------------------
# shipped: units shipped per cohort; returns: matrix (cohort x months in
# service), NA for ages a cohort has not reached yet.
warranty_life_data <- function(shipped, returns) {
  returns <- as.matrix(returns)
  if (length(shipped) != nrow(returns)) stop("Each shipment row needs its return counts.")
  rows <- list()
  for (i in seq_along(shipped)) {
    r <- returns[i, ]; age <- sum(!is.na(r))
    if (age == 0 || is.na(shipped[i])) next
    r <- r[seq_len(age)]; r[is.na(r)] <- 0
    if (any(r < 0)) stop("Return counts cannot be negative.")
    surv <- shipped[i] - sum(r)
    if (surv < 0) stop("Cohort ", i, " has more returns than units shipped.")
    # Failures in period j are taken at the period's end (age j).
    for (j in seq_len(age)) if (r[j] > 0) rows[[length(rows) + 1]] <- c(i, j, 1, r[j])
    if (surv > 0) rows[[length(rows) + 1]] <- c(i, age, 0, surv)
  }
  m <- do.call(rbind, rows)
  data.frame(Cohort = m[, 1], t = m[, 2], f = m[, 3] == 1, w = m[, 4])
}

warranty_analyze <- function(shipped, returns, dist = "weibull", horizon = 6, limit = NA) {
  ld <- warranty_life_data(shipped, returns)
  fit <- rel_fit(ld[, c("t", "f", "w")], dist)
  Fp <- function(t) { if (fit$log) t <- log(pmax(t, 1e-12)); fit$std$p((t - fit$mu) / fit$sigma) }
  returns <- as.matrix(returns)
  ages <- rowSums(!is.na(returns))
  surv <- shipped - rowSums(returns, na.rm = TRUE)
  fc <- matrix(0, length(shipped), horizon)
  for (i in seq_along(shipped)) for (h in seq_len(horizon)) {
    a <- ages[i]; if (!is.na(limit) && a + h > limit) next   # out of warranty
    if (a == 0) next
    fc[i, h] <- surv[i] * (Fp(a + h) - Fp(a + h - 1)) / (1 - Fp(a))
  }
  forecast <- data.frame(`Future period` = seq_len(horizon), `Expected returns` = colSums(fc), check.names = FALSE)
  forecast$Cumulative <- cumsum(forecast$`Expected returns`)
  sd_pois <- sqrt(forecast$Cumulative)
  forecast$`Approx. 95% upper (Poisson)` <- stats::qpois(0.975, forecast$Cumulative)
  nevada <- data.frame(Cohort = seq_along(shipped), Shipped = shipped, `Months in service` = ages,
                       `Returns so far` = rowSums(returns, na.rm = TRUE), `Return rate %` = 100 * rowSums(returns, na.rm = TRUE) / shipped,
                       check.names = FALSE)
  list(fit = fit, forecast = forecast, cohorts = nevada, life = ld, by_cohort = fc)
}

# ---- Demonstration test planning --------------------------------------------------------
# Pass if at most `failures` units fail during a test of `m` x mission time
# per unit. Weibull shape beta converts test time to mission reliability.
demo_pass_prob <- function(n, R_test, failures) stats::pbinom(failures, n, 1 - R_test)

demo_sample_size <- function(R, conf, failures = 0, multiplier = 1, beta = 1) {
  if (!(R > 0 && R < 1 && conf > 0 && conf < 1)) stop("Reliability and confidence must be between 0 and 1.")
  if (multiplier <= 0 || beta <= 0) stop("The test-time multiplier and shape must be positive.")
  Rt <- R^(multiplier^beta)
  if (failures == 0) return(ceiling(log(1 - conf) / log(Rt) - 1e-9))
  for (n in (failures + 1):1e6) if (demo_pass_prob(n, Rt, failures) <= 1 - conf) return(n)
  stop("Sample size exceeds 1,000,000.")
}

demo_test_time <- function(R, conf, n, failures = 0, beta = 1) {
  if (n <= failures) stop("Test more units than allowed failures.")
  # Find the smallest multiplier m with P(pass | R) <= 1 - conf.
  f <- function(m) demo_pass_prob(n, R^(m^beta), failures) - (1 - conf)
  hi <- 1; while (f(hi) > 0) { hi <- hi * 2; if (hi > 1e6) stop("Test time too long; add units.") }
  lo <- 0; if (f(1e-9) <= 0) return(1e-9)
  stats::uniroot(f, c(lo + 1e-12, hi), tol = 1e-10)$root
}

demo_table <- function(R, conf, beta = 1, failures = 0:3, multipliers = c(0.5, 1, 1.5, 2, 3, 4)) {
  t <- outer(failures, multipliers, Vectorize(function(f, m) demo_sample_size(R, conf, f, m, beta)))
  out <- data.frame(`Failures allowed` = failures, t, check.names = FALSE)
  names(out)[-1] <- sprintf("%g× mission", multipliers)
  out
}

# Exponential (constant failure rate) time-terminated test: total unit-hours.
demo_mtbf_time <- function(mtbf, conf, failures = 0) mtbf * stats::qchisq(conf, 2 * failures + 2) / 2

# Operating characteristic: probability of passing for a range of true reliabilities.
demo_oc <- function(n, multiplier, beta, failures, R_grid = seq(0.5, 0.9999, length.out = 200)) {
  data.frame(R = R_grid, Pass = demo_pass_prob(n, R_grid^(multiplier^beta), failures))
}
