# Capability analysis for non-normal data (percentile method), following
# qcr's qcs.pcr(): fit a distribution, then use its 0.135%, 50% and 99.865%
# quantiles in place of mean +/- 3 sigma (ISO 22514-2 / Clements):
#   Pp  = (USL - LSL) / (Q99.865 - Q0.135)
#   PPU = (USL - median) / (Q99.865 - median)
#   PPL = (median - LSL) / (median - Q0.135)
#   Ppk = min(PPL, PPU)
# Box-Cox is offered as a "distribution": normal on the transformed scale.

nn_distributions <- c("Normal" = "normal", "Lognormal" = "lognormal", "Weibull" = "weibull",
                      "Gamma" = "gamma", "Exponential" = "exponential", "Logistic" = "logistic",
                      "3-parameter lognormal" = "lognormal3", "3-parameter Weibull" = "weibull3",
                      "3-parameter gamma" = "gamma3", "Box-Cox transformation" = "boxcox")
nn_positive <- c("lognormal", "weibull", "gamma", "exponential", "boxcox")

# Box-Cox lambda by maximum profile likelihood:
#   l(lambda) = -n/2 log(sigma_hat^2(lambda)) + (lambda - 1) sum(log x)
boxcox_lambda <- function(x, round_half = FALSE) {
  grid <- seq(-5, 5, 0.01); slx <- sum(log(x)); n <- length(x)
  ll <- vapply(grid, function(l) { y <- bc_t(x, l); -n / 2 * log(mean((y - mean(y))^2)) + (l - 1) * slx }, numeric(1))
  l <- grid[which.max(ll)]
  if (round_half) l <- round(l * 2) / 2
  l
}
bc_t <- function(x, l) if (abs(l) < 1e-8) log(x) else (x^l - 1) / l
bc_inv <- function(y, l) if (abs(l) < 1e-8) exp(y) else pmax(l * y + 1, 0)^(1 / l)

# Fit one distribution. Returns density, cdf and quantile functions in the
# original units, the parameters and the log-likelihood.
nn_fit <- function(x, dist, round_lambda = FALSE) {
  x <- x[!is.na(x)]
  if (length(x) < 8) stop("At least 8 values are needed to fit a distribution.")
  if (dist %in% c("lognormal3", "weibull3", "gamma3")) return(nn_fit3(x, dist))
  if (dist %in% nn_positive && any(x <= 0)) stop(names(nn_distributions)[nn_distributions == dist],
                                                 " needs all values to be positive.")
  m <- mean(x); v <- stats::var(x)
  f <- switch(dist,
    normal = list(par = c(mean = m, sd = stats::sd(x)),
                  d = function(q, p) stats::dnorm(q, p[1], p[2]), p = function(q, p) stats::pnorm(q, p[1], p[2]),
                  q = function(pr, p) stats::qnorm(pr, p[1], p[2])),
    lognormal = list(fd = "lognormal", d = function(q, p) stats::dlnorm(q, p[1], p[2]),
                     p = function(q, p) stats::plnorm(q, p[1], p[2]), q = function(pr, p) stats::qlnorm(pr, p[1], p[2])),
    weibull = {
      # Starting values from the method of moments on the log scale.
      sh <- 1.2 / stats::sd(log(x)); sc <- exp(mean(log(x)) + 0.5772 / sh)
      list(fd = "weibull", start = list(shape = sh, scale = sc),
           d = function(q, p) stats::dweibull(q, p[1], p[2]), p = function(q, p) stats::pweibull(q, p[1], p[2]),
           q = function(pr, p) stats::qweibull(pr, p[1], p[2]))
    },
    gamma = list(fd = "gamma", start = list(shape = m^2 / v, rate = m / v), lower = c(1e-10, 1e-10),
                 d = function(q, p) stats::dgamma(q, p[1], p[2]), p = function(q, p) stats::pgamma(q, p[1], p[2]),
                 q = function(pr, p) stats::qgamma(pr, p[1], p[2])),
    exponential = list(par = c(rate = 1 / m),
                       d = function(q, p) stats::dexp(q, p[1]), p = function(q, p) stats::pexp(q, p[1]),
                       q = function(pr, p) stats::qexp(pr, p[1])),
    logistic = list(fd = "logistic", d = function(q, p) stats::dlogis(q, p[1], p[2]),
                    p = function(q, p) stats::plogis(q, p[1], p[2]), q = function(pr, p) stats::qlogis(pr, p[1], p[2])),
    boxcox = {
      l <- boxcox_lambda(x, round_lambda)
      y <- bc_t(x, l)
      list(par = c(lambda = l, mean = mean(y), sd = stats::sd(y)),
           d = function(q, p) ifelse(q > 0, stats::dnorm(bc_t(q, p[1]), p[2], p[3]) * q^(p[1] - 1), 0),
           p = function(q, p) ifelse(q > 0, stats::pnorm(bc_t(q, p[1]), p[2], p[3]), 0),
           q = function(pr, p) bc_inv(stats::qnorm(pr, p[2], p[3]), p[1]))
    },
    stop("Unknown distribution."))
  if (!is.null(f$fd)) {
    args <- list(x = x, densfun = f$fd)
    if (!is.null(f$start)) args$start <- f$start
    if (!is.null(f$lower)) args$lower <- f$lower
    fit <- tryCatch(suppressWarnings(do.call(MASS::fitdistr, args)),
                    error = function(e) stop("Could not fit the ", dist, " distribution: ", conditionMessage(e)))
    f$par <- fit$estimate
  }
  dens <- f$d(x, f$par)
  ll <- sum(log(pmax(dens, 1e-300)))
  k <- length(f$par)
  list(dist = dist, label = names(nn_distributions)[nn_distributions == dist], par = f$par,
       dfun = function(q) f$d(q, f$par), pfun = function(q) f$p(q, f$par), qfun = function(pr) f$q(pr, f$par),
       loglik = ll, aic = -2 * ll + 2 * k, x = x)
}

# Three-parameter versions: a threshold (lower bound) theta is estimated by
# maximising the profile likelihood of the two-parameter fit to x - theta.
nn_fit3 <- function(x, dist) {
  base <- sub("3$", "", dist)
  span <- diff(range(x)); if (span == 0) stop("All values are identical.")
  prof <- function(th) tryCatch(nn_fit(x - th, base)$loglik, error = function(e) -Inf)
  lo <- min(x) - 3 * span; hi <- min(x) - 1e-3 * span
  o <- stats::optimize(prof, c(lo, hi), maximum = TRUE)
  th <- o$maximum
  b <- nn_fit(x - th, base)
  par <- c(threshold = th, b$par)
  ll <- b$loglik
  list(dist = dist, label = names(nn_distributions)[nn_distributions == dist], par = par,
       dfun = function(q) ifelse(q > th, b$dfun(q - th), 0), pfun = function(q) ifelse(q > th, b$pfun(q - th), 0),
       qfun = function(pr) th + b$qfun(pr), loglik = ll, aic = -2 * ll + 2 * length(par), x = x)
}

# Anderson-Darling statistic of the data against a fitted cdf.
ad_statistic <- function(x, pfun) {
  x <- sort(x); n <- length(x)
  u <- pmin(pmax(pfun(x), 1e-12), 1 - 1e-12)
  -n - mean((2 * seq_len(n) - 1) * (log(u) + log(1 - rev(u))))
}

# Compare all distributions that apply to the data.
nn_compare <- function(x, round_lambda = FALSE) {
  x <- x[!is.na(x)]
  rows <- lapply(nn_distributions, function(d) {
    f <- tryCatch(nn_fit(x, d, round_lambda), error = function(e) NULL)
    if (is.null(f)) return(NULL)
    data.frame(Distribution = f$label, Code = d, `AD statistic` = ad_statistic(x, f$pfun), AIC = f$aic,
               Parameters = paste(sprintf("%s = %.4g", names(f$par), f$par), collapse = ", "),
               check.names = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) stop("No distribution could be fitted.")
  out <- out[order(out$AIC), ]
  out$Rank <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}

nn_capability <- function(fit, lsl, usl, target = NA) {
  if (is.na(lsl) && is.na(usl)) stop("Enter at least one specification limit.")
  if (!is.na(lsl) && !is.na(usl) && lsl >= usl) stop("LSL must be below USL.")
  q <- fit$qfun(c(0.00135, 0.5, 0.99865))
  pp <- if (!is.na(lsl) && !is.na(usl)) (usl - lsl) / (q[3] - q[1]) else NA
  ppu <- if (!is.na(usl)) (usl - q[2]) / (q[3] - q[2]) else NA
  ppl <- if (!is.na(lsl)) (q[2] - lsl) / (q[2] - q[1]) else NA
  below <- if (is.na(lsl)) 0 else fit$pfun(lsl)
  above <- if (is.na(usl)) 0 else 1 - fit$pfun(usl)
  x <- fit$x
  list(
    indices = data.frame(Index = c("Pp", "PPL", "PPU", "Ppk"),
                         Value = c(pp, ppl, ppu, min(ppl, ppu, na.rm = TRUE))),
    quantiles = data.frame(Percentile = c("0.135%", "50% (median)", "99.865%"), Value = q, check.names = FALSE),
    ppm = data.frame(` ` = c("PPM < LSL", "PPM > USL", "PPM total"),
                     Expected = 1e6 * c(below, above, below + above),
                     Observed = 1e6 * c(if (is.na(lsl)) 0 else mean(x < lsl), if (is.na(usl)) 0 else mean(x > usl),
                                        mean((!is.na(lsl) & x < lsl) | (!is.na(usl) & x > usl))),
                     check.names = FALSE)
  )
}

nn_plot <- function(fit, lsl, usl, target = NA, bins = 20) {
  x <- fit$x
  rng <- range(c(x, lsl, usl, fit$qfun(c(0.001, 0.999))), na.rm = TRUE)
  rng <- rng + c(-1, 1) * 0.05 * diff(rng)
  grid <- seq(rng[1], rng[2], length.out = 400)
  dd <- data.frame(x = grid, y = fit$dfun(grid))
  specs <- data.frame(v = c(lsl, usl, target), lab = c("LSL", "USL", "Target"))
  specs <- specs[!is.na(specs$v), ]
  ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = bins, fill = "#c9d8ea", colour = "white") +
    ggplot2::geom_line(data = dd, ggplot2::aes(x, y), colour = "#4a6fa5", linewidth = 1) +
    ggplot2::geom_vline(data = specs, ggplot2::aes(xintercept = v), colour = ifelse(specs$lab == "Target", "#2e7d32", "#c0392b"),
                        linetype = 2) +
    ggplot2::geom_text(data = specs, ggplot2::aes(x = v, y = Inf, label = lab), vjust = 1.5, hjust = -0.1, size = 3.6) +
    ggplot2::coord_cartesian(xlim = rng) +
    ggplot2::labs(title = if (fit$dist == "boxcox") sprintf("Capability using a Box-Cox transformation (\u03bb = %.2f)", fit$par[["lambda"]])
                          else sprintf("Capability with a fitted %s distribution", tolower(fit$label)), x = NULL, y = "Density") +
    ggplot2::theme_minimal(base_size = 13) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

nn_probability_plot <- function(fit) {
  x <- sort(fit$x); n <- length(x)
  th <- fit$qfun(stats::ppoints(n))
  ggplot2::ggplot(data.frame(th = th, x = x), ggplot2::aes(th, x)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "#c0392b") +
    ggplot2::geom_point(colour = "#4a6fa5") +
    ggplot2::labs(title = sprintf("%s probability plot", fit$label),
                  subtitle = "Points close to the line indicate a good fit",
                  x = "Fitted quantile", y = "Observed value") +
    ggplot2::theme_minimal(base_size = 13) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

# Goodness-of-fit p-values by parametric bootstrap: simulate samples from the
# fitted distribution, refit, and compare their AD statistics with the
# observed one. Needed because the usual AD tables assume known parameters.
nn_ad_pvalue <- function(fit, B = 200, round_lambda = FALSE, seed = 1) {
  set.seed(seed)
  a0 <- ad_statistic(fit$x, fit$pfun); n <- length(fit$x)
  sims <- vapply(seq_len(B), function(i) {
    xs <- fit$qfun(stats::runif(n))
    f <- tryCatch(nn_fit(xs, fit$dist, round_lambda), error = function(e) NULL)
    if (is.null(f)) NA_real_ else ad_statistic(xs, f$pfun)
  }, numeric(1))
  sims <- sims[is.finite(sims)]
  (1 + sum(sims >= a0)) / (length(sims) + 1)
}
