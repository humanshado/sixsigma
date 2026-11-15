# Tolerance intervals, equivalence tests (TOST), correlation, outlier tests
# and basic time-series analysis.

# ---- Tolerance intervals -------------------------------------------------------------

# Exact two-sided normal tolerance factor (Odeh & Owen): the probability that
# mean +/- k s contains at least `coverage` of the population is `conf`.
tol_k_two <- function(n, coverage, conf) {
  nu <- n - 1
  rfun <- function(x) {  # half-width r with Phi(x/sqrt(n)+r) - Phi(x/sqrt(n)-r) = coverage
    m <- x / sqrt(n)
    stats::uniroot(function(r) stats::pnorm(m + r) - stats::pnorm(m - r) - coverage, c(0, abs(m) + 20), tol = 1e-12)$root
  }
  achieved <- function(k) {
    f <- function(x) vapply(x, function(xx) stats::pchisq(nu * rfun(xx)^2 / k^2, nu, lower.tail = FALSE), numeric(1)) * stats::dnorm(x)
    2 * stats::integrate(f, 0, 12, rel.tol = 1e-10)$value  # dnorm is negligible beyond 12
  }
  stats::uniroot(function(k) achieved(k) - conf, c(0.5, 200), tol = 1e-10)$root
}
tol_k_one <- function(n, coverage, conf) stats::qt(conf, n - 1, ncp = stats::qnorm(coverage) * sqrt(n)) / sqrt(n)

tolerance_interval <- function(x, coverage = 0.95, conf = 0.95, sides = "two") {
  x <- x[!is.na(x)]; n <- length(x)
  if (n < 3) stop("At least three values are needed.")
  if (!(coverage > 0 && coverage < 1 && conf > 0 && conf < 1)) stop("Coverage and confidence must be between 0 and 100%.")
  m <- mean(x); s <- stats::sd(x)
  normal <- if (sides == "two") { k <- tol_k_two(n, coverage, conf); c(m - k * s, m + k * s, k) }
            else { k <- tol_k_one(n, coverage, conf); if (sides == "lower") c(m - k * s, NA, k) else c(NA, m + k * s, k) }
  # Distribution-free: the sample extremes, with the confidence they achieve.
  xs <- sort(x)
  np_conf <- if (sides == "two") 1 - n * coverage^(n - 1) + (n - 1) * coverage^n else 1 - coverage^n
  n_needed <- if (sides == "two") { nn <- 2; while (1 - nn * coverage^(nn - 1) + (nn - 1) * coverage^nn < conf && nn < 1e6) nn <- nn + 1; nn }
              else ceiling(log(1 - conf) / log(coverage))
  data.frame(Method = c("Normal", "Nonparametric (sample extremes)"),
             Lower = c(normal[1], if (sides == "upper") NA else xs[1]),
             Upper = c(normal[2], if (sides == "lower") NA else xs[n]),
             `k factor` = c(normal[3], NA),
             `Achieved confidence (%)` = c(100 * conf, 100 * np_conf),
             Note = c(sprintf("Assumes normal data (n = %d, mean %.4g, s %.4g)", n, m, s),
                      sprintf("Needs n >= %d for %g%% confidence", n_needed, 100 * conf)),
             check.names = FALSE)
}

# ---- Equivalence (two one-sided tests) ------------------------------------------------

tost <- function(x, y = NULL, lower, upper, type = "one", target = 0, alpha = 0.05) {
  if (!(lower < upper)) stop("The lower equivalence limit must be below the upper limit.")
  x <- x[!is.na(x)]
  if (type == "one") {
    d <- x - target; est <- mean(d); se <- stats::sd(d) / sqrt(length(d)); df <- length(d) - 1; what <- "Mean - target"
  } else if (type == "paired") {
    ok <- !is.na(y[seq_along(x)]); d <- (x - y[seq_along(x)])[ok]
    est <- mean(d); se <- stats::sd(d) / sqrt(length(d)); df <- length(d) - 1; what <- "Mean difference"
  } else {
    y <- y[!is.na(y)]
    v1 <- stats::var(x) / length(x); v2 <- stats::var(y) / length(y)
    est <- mean(x) - mean(y); se <- sqrt(v1 + v2)
    df <- (v1 + v2)^2 / (v1^2 / (length(x) - 1) + v2^2 / (length(y) - 1)); what <- "Difference of means"
  }
  if (!is.finite(se) || se == 0) stop("Not enough variation in the data.")
  t_lo <- (est - lower) / se; t_hi <- (est - upper) / se
  p_lo <- stats::pt(t_lo, df, lower.tail = FALSE); p_hi <- stats::pt(t_hi, df)
  ci <- est + c(-1, 1) * stats::qt(1 - alpha, df) * se
  eq <- max(p_lo, p_hi) < alpha
  list(table = data.frame(Quantity = c(what, sprintf("%g%% CI lower", 100 * (1 - 2 * alpha)), sprintf("%g%% CI upper", 100 * (1 - 2 * alpha)),
                                       "Lower limit", "Upper limit", "p-value (> lower)", "p-value (< upper)", "DF"),
                          Value = c(est, ci, lower, upper, p_lo, p_hi, df)),
       equivalent = eq, est = est, ci = ci, lower = lower, upper = upper,
       conclusion = if (eq) sprintf("Equivalent: the %g%% confidence interval lies within the equivalence limits (both one-sided p < %g).", 100 * (1 - 2 * alpha), alpha)
                    else "Equivalence not shown: the confidence interval is not entirely inside the limits.")
}

tost_plot <- function(t) {
  ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = t$lower, xmax = t$upper, ymin = -Inf, ymax = Inf, fill = "#d8ecd8", alpha = 0.7) +
    ggplot2::geom_vline(xintercept = c(t$lower, t$upper), colour = "#2e7d32", linetype = 2) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
    ggplot2::geom_errorbarh(ggplot2::aes(y = 1, xmin = t$ci[1], xmax = t$ci[2]), height = 0.2, colour = "#4a6fa5", linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(x = t$est, y = 1), colour = "#c0392b", size = 3) +
    ggplot2::scale_y_continuous(NULL, breaks = NULL, limits = c(0.5, 1.5)) +
    ggplot2::labs(title = "Equivalence test", subtitle = "Green band: equivalence limits; blue: confidence interval", x = "Difference") +
    ggplot2::theme_bw(base_size = 12) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

# ---- Correlation ---------------------------------------------------------------------

correlation_table <- function(df, vars, method = "pearson") {
  if (length(vars) < 2) stop("Choose at least two numeric variables.")
  rows <- list()
  for (i in 1:(length(vars) - 1)) for (j in (i + 1):length(vars)) {
    a <- df[[vars[i]]]; b <- df[[vars[j]]]; ok <- !is.na(a) & !is.na(b)
    if (sum(ok) < 3) next
    ct <- suppressWarnings(stats::cor.test(a[ok], b[ok], method = method))
    ci <- if (!is.null(ct$conf.int)) ct$conf.int else c(NA, NA)
    rows[[length(rows) + 1]] <- data.frame(`Variable 1` = vars[i], `Variable 2` = vars[j], N = sum(ok),
                                           Correlation = unname(ct$estimate), `CI lower` = ci[1], `CI upper` = ci[2],
                                           `p-value` = ct$p.value, check.names = FALSE)
  }
  do.call(rbind, rows)
}

correlation_plot <- function(df, vars, method = "pearson") {
  m <- stats::cor(df[, vars, drop = FALSE], use = "pairwise.complete.obs", method = method)
  long <- data.frame(a = rep(vars, each = length(vars)), b = rep(vars, length(vars)), r = as.vector(m))
  long$a <- factor(long$a, levels = vars); long$b <- factor(long$b, levels = rev(vars))
  ggplot2::ggplot(long, ggplot2::aes(a, b, fill = r)) + ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", r)), size = 3.6) +
    ggplot2::scale_fill_gradient2(low = "#c0392b", mid = "white", high = "#4a6fa5", limits = c(-1, 1), name = "r") +
    ggplot2::labs(title = sprintf("%s correlation matrix", tools::toTitleCase(method)), x = NULL, y = NULL) +
    ggplot2::coord_equal() + ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

scatter_matrix <- function(df, vars) {
  pairs <- utils::combn(vars, 2)
  d <- do.call(rbind, lapply(seq_len(ncol(pairs)), function(j) data.frame(x = df[[pairs[1, j]]], y = df[[pairs[2, j]]],
                                                                          panel = paste(pairs[2, j], "vs", pairs[1, j]))))
  d <- d[stats::complete.cases(d), ]
  ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point(colour = "#4a6fa5", alpha = 0.6) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "#c0392b", linewidth = 0.8) +
    ggplot2::facet_wrap(~panel, scales = "free") + ggplot2::labs(title = "Scatterplots", x = NULL, y = NULL) +
    ggplot2::theme_bw(base_size = 11) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

# ---- Outlier tests -------------------------------------------------------------------

grubbs_test <- function(x, side = "two", alpha = 0.05) {
  x <- x[!is.na(x)]; n <- length(x)
  if (n < 3) stop("Grubbs' test needs at least three values.")
  m <- mean(x); s <- stats::sd(x)
  if (s == 0) stop("All values are identical.")
  i <- switch(side, two = which.max(abs(x - m)), max = which.max(x), min = which.min(x))
  G <- abs(x[i] - m) / s
  t2 <- function(G) n * (n - 2) * G^2 / ((n - 1)^2 - n * G^2)
  pv <- if (G >= (n - 1) / sqrt(n)) 0 else {
    tt <- sqrt(t2(G)); p1 <- n * stats::pt(tt, n - 2, lower.tail = FALSE)
    min(1, if (side == "two") 2 * p1 else p1)
  }
  data.frame(Test = sprintf("Grubbs (%s)", switch(side, two = "two-sided", max = "largest value", min = "smallest value")),
             `Suspect value` = x[i], Row = i, G = G, `p-value` = pv,
             Conclusion = if (pv < alpha) "Outlier" else "No outlier", check.names = FALSE)
}

# Generalized ESD (Rosner, 1983) for up to r outliers.
esd_test <- function(x, r = 3, alpha = 0.05) {
  x0 <- x; idx <- which(!is.na(x)); x <- x[idx]; n <- length(x)
  if (n < 5) stop("The ESD test needs at least five values.")
  r <- min(r, n - 2)
  rows <- list(); cur <- seq_along(x)
  for (i in seq_len(r)) {
    v <- x[cur]; m <- mean(v); s <- stats::sd(v)
    j <- which.max(abs(v - m)); R <- abs(v[j] - m) / s
    p <- 1 - alpha / (2 * (n - i + 1))
    tq <- stats::qt(p, n - i - 1)
    lam <- (n - i) * tq / sqrt((n - i - 1 + tq^2) * (n - i + 1))
    rows[[i]] <- data.frame(Step = i, Value = v[j], Row = idx[cur[j]], `Test statistic` = R, `Critical value` = lam, check.names = FALSE)
    cur <- cur[-j]
  }
  out <- do.call(rbind, rows)
  k <- if (any(out$`Test statistic` > out$`Critical value`)) max(which(out$`Test statistic` > out$`Critical value`)) else 0
  out$Outlier <- ifelse(seq_len(nrow(out)) <= k, "Yes", "No")
  out
}

# ---- Time series ----------------------------------------------------------------------

ts_accuracy <- function(y, fit) {
  e <- y - fit; ok <- !is.na(e)
  data.frame(Measure = c("MAPE (%)", "MAD", "MSD"),
             Value = c(100 * mean(abs(e[ok] / y[ok])[y[ok] != 0]), mean(abs(e[ok])), mean(e[ok]^2)))
}

ts_analyze <- function(y, method = "linear", h = 6, window = 3, alpha = NA, period = 12, decomp = "additive") {
  y <- as.numeric(y); y <- y[!is.na(y)]; n <- length(y); t <- seq_len(n)
  if (n < 4) stop("At least four observations are needed.")
  fc_t <- n + seq_len(h)
  res <- switch(method,
    linear = { f <- stats::lm(y ~ t); list(fit = stats::fitted(f), fc = stats::predict(f, data.frame(t = fc_t)),
                                         model = sprintf("y = %.5g %+.5g t", stats::coef(f)[1], stats::coef(f)[2])) },
    quadratic = { f <- stats::lm(y ~ t + I(t^2)); cf <- stats::coef(f)
                  list(fit = stats::fitted(f), fc = stats::predict(f, data.frame(t = fc_t)),
                       model = sprintf("y = %.5g %+.5g t %+.5g t^2", cf[1], cf[2], cf[3])) },
    exponential = { if (any(y <= 0)) stop("Exponential growth needs positive values.")
                    f <- stats::lm(log(y) ~ t); cf <- stats::coef(f)
                    list(fit = exp(stats::fitted(f)), fc = exp(stats::predict(f, data.frame(t = fc_t))),
                         model = sprintf("y = %.5g x %.5g^t", exp(cf[1]), exp(cf[2]))) },
    moving = { if (window < 2 || window >= n) stop("Choose a moving-average length between 2 and n - 1.")
               ma <- stats::filter(y, rep(1 / window, window), sides = 1)
               fit <- c(NA, utils::head(as.numeric(ma), -1))
               list(fit = fit, fc = rep(utils::tail(as.numeric(ma), 1), h), model = sprintf("Moving average of length %d", window)) },
    single = { hw <- stats::HoltWinters(y, alpha = if (is.na(alpha)) NULL else alpha, beta = FALSE, gamma = FALSE)
               list(fit = c(NA, hw$fitted[, "xhat"]), fc = as.numeric(stats::predict(hw, h)),
                    model = sprintf("Single exponential smoothing, alpha = %.4f", hw$alpha)) },
    double = { hw <- stats::HoltWinters(y, gamma = FALSE)
               list(fit = c(NA, NA, hw$fitted[, "xhat"]), fc = as.numeric(stats::predict(hw, h)),
                    model = sprintf("Double exponential smoothing (Holt), alpha = %.4f, beta = %.4f", hw$alpha, hw$beta)) },
    decomposition = {
      if (n < 2 * period) stop("Decomposition needs at least two full seasonal cycles (", 2 * period, " observations).")
      if (decomp == "multiplicative" && any(y <= 0)) stop("Multiplicative decomposition needs positive values.")
      dc <- stats::decompose(stats::ts(y, frequency = period), type = decomp)
      tr <- stats::lm(as.numeric(dc$trend) ~ t)
      seas <- as.numeric(dc$figure)[((fc_t - 1) %% period) + 1]
      trend_fc <- stats::predict(tr, data.frame(t = fc_t))
      fit <- if (decomp == "additive") stats::fitted(stats::lm(y ~ t)) * 0 + stats::predict(tr, data.frame(t = t)) + as.numeric(dc$seasonal)
             else stats::predict(tr, data.frame(t = t)) * as.numeric(dc$seasonal)
      list(fit = fit, fc = if (decomp == "additive") trend_fc + seas else trend_fc * seas,
           model = sprintf("%s decomposition, period %d; linear trend on the trend component", tools::toTitleCase(decomp), period),
           seasonal = data.frame(Period = seq_len(period), `Seasonal index` = as.numeric(dc$figure), check.names = FALSE))
    },
    stop("Unknown method."))
  res$y <- y; res$h <- h; res$accuracy <- ts_accuracy(y, res$fit); res$method <- method
  res$forecast <- data.frame(Period = fc_t, Forecast = as.numeric(res$fc))
  res
}

ts_plot <- function(r) {
  n <- length(r$y)
  d <- rbind(data.frame(t = seq_len(n), v = r$y, Series = "Actual"),
             data.frame(t = seq_len(n), v = as.numeric(r$fit), Series = "Fitted"),
             data.frame(t = r$forecast$Period, v = r$forecast$Forecast, Series = "Forecast"))
  d <- d[!is.na(d$v), ]
  ggplot2::ggplot(d, ggplot2::aes(t, v, colour = Series)) + ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(data = d[d$Series != "Fitted", ], size = 1.8) +
    ggplot2::scale_colour_manual(values = c(Actual = "#4a6fa5", Fitted = "#c0392b", Forecast = "#2e7d32")) +
    ggplot2::labs(title = "Time series", subtitle = r$model, x = "Time", y = NULL, colour = NULL) +
    ggplot2::theme_bw(base_size = 12) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), legend.position = "top")
}

acf_plot <- function(y, lags = 20) {
  y <- y[!is.na(y)]
  a <- stats::acf(y, lag.max = min(lags, length(y) - 1), plot = FALSE)
  p <- stats::pacf(y, lag.max = min(lags, length(y) - 1), plot = FALSE)
  bound <- stats::qnorm(0.975) / sqrt(length(y))
  d <- rbind(data.frame(lag = a$lag[-1], v = a$acf[-1], What = "Autocorrelation (ACF)"),
             data.frame(lag = as.numeric(p$lag), v = as.numeric(p$acf), What = "Partial autocorrelation (PACF)"))
  ggplot2::ggplot(d, ggplot2::aes(lag, v)) + ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_hline(yintercept = c(-bound, bound), linetype = 2, colour = "#c0392b") +
    ggplot2::geom_segment(ggplot2::aes(xend = lag, yend = 0), colour = "#4a6fa5", linewidth = 1) +
    ggplot2::facet_wrap(~What) + ggplot2::labs(title = "Autocorrelation", subtitle = "Bars beyond the dashed lines are significant (5%)", x = "Lag", y = NULL) +
    ggplot2::theme_bw(base_size = 12) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}
