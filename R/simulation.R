# Tolerance stack-up and Monte Carlo simulation.

sim_draw <- function(n, dist, p1, p2, p3 = NA) {
  switch(dist,
    normal = stats::rnorm(n, p1, p2),
    uniform = { if (p2 <= p1) stop("Uniform: the maximum must exceed the minimum."); stats::runif(n, p1, p2) },
    triangular = {
      a <- p1; c <- p2; b <- p3   # min, mode, max
      if (!(a <= c && c <= b && a < b)) stop("Triangular: need minimum <= mode <= maximum.")
      u <- stats::runif(n); fc <- (c - a) / (b - a)
      ifelse(u < fc, a + sqrt(u * (b - a) * (c - a)), b - sqrt((1 - u) * (b - a) * (b - c)))
    },
    lognormal = stats::rlnorm(n, p1, p2),          # meanlog, sdlog
    weibull = stats::rweibull(n, shape = p1, scale = p2),
    exponential = stats::rexp(n, 1 / p1),           # mean
    constant = rep(p1, n),
    stop("Unknown distribution '", dist, "'."))
}

sim_param_names <- list(normal = c("Mean", "SD", ""), uniform = c("Min", "Max", ""), triangular = c("Min", "Mode", "Max"),
                        lognormal = c("Meanlog", "Sdlog", ""), weibull = c("Shape", "Scale", ""),
                        exponential = c("Mean", "", ""), constant = c("Value", "", ""))

sim_spec_summary <- function(y, lsl = NA, usl = NA) {
  n <- length(y)
  below <- if (is.na(lsl)) NA else mean(y < lsl); above <- if (is.na(usl)) NA else mean(y > usl)
  out <- sum(c(below, above), na.rm = TRUE)
  m <- mean(y); s <- stats::sd(y)
  cpk <- min(c(if (!is.na(usl)) (usl - m) / (3 * s), if (!is.na(lsl)) (m - lsl) / (3 * s)), Inf)
  data.frame(Statistic = c("Mean", "StDev", "Minimum", "0.135 percentile", "Median", "99.865 percentile", "Maximum",
                           "% below LSL", "% above USL", "PPM out of spec", "Ppk (normal)"),
             Value = c(m, s, min(y), stats::quantile(y, 0.00135), stats::median(y), stats::quantile(y, 0.99865), max(y),
                       100 * below, 100 * above, if (is.na(lsl) && is.na(usl)) NA else 1e6 * out,
                       if (is.finite(cpk)) cpk else NA))
}

# Monte Carlo of Y = f(inputs). inputs: data.frame(name, dist, p1, p2, p3).
monte_carlo <- function(inputs, formula, n = 1e5, lsl = NA, usl = NA, seed = NA) {
  nm <- trimws(inputs$name)
  if (!length(nm) || any(!nzchar(nm)) || anyDuplicated(nm)) stop("Input names must be filled in and different.")
  if (any(make.names(nm) != nm)) stop("Input names must be plain names (letters, digits, dots, underscores; start with a letter).")
  if (n < 100 || n > 2e6) stop("Use between 100 and 2,000,000 trials.")
  if (!is.na(seed)) set.seed(seed)
  X <- as.data.frame(stats::setNames(lapply(seq_along(nm), function(i)
    sim_draw(n, inputs$dist[i], inputs$p1[i], inputs$p2[i], inputs$p3[i])), nm))
  if (any(vapply(X, function(v) any(!is.finite(v)), logical(1)))) stop("Check the distribution parameters (e.g. SD must be positive).")
  y <- safe_eval(formula, X)
  if (!is.numeric(y)) stop("The formula must give a number.")
  y <- rep_len(y, n)
  if (any(!is.finite(y))) stop("The formula gives missing or infinite values for some trials (e.g. division by zero).")
  # Sensitivity: share of variance by squared rank correlation (normalized).
  varying <- vapply(X, function(v) stats::sd(v) > 0, logical(1))
  rho <- vapply(X, function(v) if (stats::sd(v) > 0) stats::cor(v, y, method = "spearman") else 0, numeric(1))
  sens <- data.frame(Input = nm, `Rank correlation` = rho,
                     `% contribution` = if (sum(rho^2) > 0) 100 * rho^2 / sum(rho^2) else 0, check.names = FALSE)
  sens <- sens[order(-sens$`% contribution`), , drop = FALSE]; rownames(sens) <- NULL
  list(y = y, X = X, summary = sim_spec_summary(y, lsl, usl), sensitivity = sens, lsl = lsl, usl = usl, formula = formula)
}

sim_histogram <- function(r, title = "Simulated output") {
  d <- data.frame(y = r$y)
  p <- ggplot2::ggplot(d, ggplot2::aes(y)) +
    ggplot2::geom_histogram(bins = 60, fill = "#4C78A8", colour = "white") +
    ggplot2::labs(title = title, x = r$formula %||% "Y", y = "Count") + ggplot2::theme_minimal(base_size = 13)
  for (s in c(r$lsl, r$usl)) if (!is.na(s)) p <- p + ggplot2::geom_vline(xintercept = s, colour = "#D62728", linewidth = 1, linetype = "dashed")
  p
}

# Linear tolerance stack-up. parts: data.frame(name, nominal, tol_minus, tol_plus, sign, dist)
# dist: "normal" (tolerance = 3 sigma unless cp given) or "uniform" (over the tolerance band).
stackup <- function(parts, lsl = NA, usl = NA, n = 1e5, seed = NA, cp = 1) {
  if (!nrow(parts)) stop("Enter at least one dimension.")
  if (any(parts$tol_minus < 0 | parts$tol_plus < 0)) stop("Enter tolerances as positive numbers (e.g. 0.1 for -0.1).")
  s <- parts$sign
  lo <- parts$nominal - parts$tol_minus; hi <- parts$nominal + parts$tol_plus
  mid <- (lo + hi) / 2; half <- (hi - lo) / 2
  nominal <- sum(s * parts$nominal)
  centre <- sum(s * mid)
  wc <- c(centre - sum(half), centre + sum(half))
  sig <- ifelse(parts$dist == "uniform", half / sqrt(3), half / (3 * cp))
  sd_stack <- sqrt(sum(sig^2))
  rss <- c(centre - 3 * sd_stack, centre + 3 * sd_stack)
  if (!is.na(seed)) set.seed(seed)
  sims <- vapply(seq_len(nrow(parts)), function(i)
    if (parts$dist[i] == "uniform") stats::runif(n, lo[i], hi[i]) else stats::rnorm(n, mid[i], sig[i]), numeric(n))
  if (is.null(dim(sims))) sims <- matrix(sims, ncol = nrow(parts))
  y <- as.vector(sims %*% s)
  contrib <- data.frame(Dimension = parts$name, Direction = ifelse(s > 0, "+", "-"),
                        `Tolerance band` = hi - lo, `Std dev` = sig,
                        `% of worst case` = 100 * half / sum(half), `% of variance (RSS)` = 100 * sig^2 / sum(sig^2),
                        check.names = FALSE)
  limits <- data.frame(Method = c("Worst case", "RSS (±3 sigma)", "Monte Carlo (0.135-99.865%)"),
                       Minimum = c(wc[1], rss[1], stats::quantile(y, 0.00135)),
                       Maximum = c(wc[2], rss[2], stats::quantile(y, 0.99865)))
  limits$Range <- limits$Maximum - limits$Minimum
  if (!is.na(lsl) || !is.na(usl)) {
    eps <- 1e-9 * max(1, abs(c(lsl, usl)), na.rm = TRUE)
    pp <- function(lo_, hi_) (if (!is.na(lsl)) lo_ >= lsl - eps else TRUE) & (if (!is.na(usl)) hi_ <= usl + eps else TRUE)
    limits$`Within spec` <- ifelse(pp(limits$Minimum, limits$Maximum), "Yes", "No")
    out_rss <- (if (!is.na(lsl)) stats::pnorm(lsl, centre, sd_stack) else 0) + (if (!is.na(usl)) stats::pnorm(usl, centre, sd_stack, lower.tail = FALSE) else 0)
    out_mc <- mean((if (!is.na(lsl)) y < lsl else FALSE) | (if (!is.na(usl)) y > usl else FALSE))
    limits$`PPM out of spec` <- c(if (pp(wc[1], wc[2])) 0 else NA, 1e6 * out_rss, 1e6 * out_mc)
  }
  list(nominal = nominal, centre = centre, sd = sd_stack, limits = limits, contributions = contrib,
       y = y, lsl = lsl, usl = usl, formula = "Stack (assembly gap)")
}

stackup_plot <- function(r) {
  c <- r$contributions
  c$Dimension <- factor(c$Dimension, levels = c$Dimension[order(c$`% of variance (RSS)`)])
  ggplot2::ggplot(c, ggplot2::aes(Dimension, `% of variance (RSS)`)) + ggplot2::geom_col(fill = "#4C78A8") +
    ggplot2::coord_flip() + ggplot2::labs(title = "Contribution to stack variation", y = "% of variance", x = NULL) +
    ggplot2::theme_minimal(base_size = 13)
}
