# Shewhart control chart calculations.
#
# The SixSigma package only ships a moving-range chart (ss.cc("mr")), so the
# remaining charts are computed here with the textbook Shewhart formulas.
# Bias-correction constants (d2, d3, c4) come from SixSigma itself, which
# derives them exactly for any subgroup size instead of using a lookup table.

spc_d2 <- function(n) unname(SixSigma::ss.cc.getd2(n))
spc_d3 <- function(n) unname(SixSigma::ss.cc.getd3(n))
spc_c4 <- function(n) unname(SixSigma::ss.cc.getc4(n))

# Split a numeric vector into subgroups, either by a grouping vector or by
# consecutive runs of a fixed size. Returns a list of numeric vectors with
# missing values removed; subgroups left with fewer than `min_size` points
# are dropped.
spc_subgroups <- function(x, groups = NULL, size = NULL, min_size = 2) {
  if (!is.numeric(x)) stop("The measurement column must be numeric.")
  if (!is.null(groups)) {
    if (length(groups) != length(x)) stop("Group column length does not match the data.")
    keep <- !is.na(groups)
    sg <- split(x[keep], factor(groups[keep], levels = unique(groups[keep])))
  } else {
    if (is.null(size) || is.na(size) || size < 2) stop("Subgroup size must be at least 2.")
    idx <- ceiling(seq_along(x) / size)
    sg <- split(x, idx)
  }
  sg <- lapply(sg, function(v) v[!is.na(v)])
  sg <- sg[vapply(sg, length, integer(1)) >= min_size]
  if (length(sg) < 2) stop("At least two subgroups with ", min_size, " or more values are needed.")
  sg
}

# Run rules applied to a sequence of plotted points.
#   1: one point beyond the control limits
#   2: nine points in a row on the same side of the centre line
#   3: six points in a row steadily increasing or decreasing
spc_rules <- function(y, cl, lcl, ucl) {
  n <- length(y)
  cl <- rep_len(cl, n); lcl <- rep_len(lcl, n); ucl <- rep_len(ucl, n)
  viol <- vector("list", n)
  add <- function(i, r) viol[[i]] <<- sort(unique(c(viol[[i]], r)))

  for (i in seq_len(n)) {
    if (is.na(y[i])) next
    if ((!is.na(ucl[i]) && y[i] > ucl[i]) || (!is.na(lcl[i]) && y[i] < lcl[i])) add(i, 1L)
  }
  side <- sign(y - cl)
  run <- 0L
  for (i in seq_len(n)) {
    if (i > 1 && !is.na(side[i]) && side[i] != 0 && identical(side[i], side[i - 1])) {
      run <- run + 1L
    } else {
      run <- if (!is.na(side[i]) && side[i] != 0) 1L else 0L
    }
    if (run >= 9) add(i, 2L)
  }
  d <- sign(diff(y))
  run <- 0L
  for (i in seq_along(d)) {
    if (i > 1 && !is.na(d[i]) && d[i] != 0 && identical(d[i], d[i - 1])) {
      run <- run + 1L
    } else {
      run <- if (!is.na(d[i]) && d[i] != 0) 1L else 0L
    }
    # A run of 5 consecutive moves in one direction spans 6 points.
    if (run >= 5) add(i + 1L, 3L)
  }
  viol
}

new_chart <- function(title, ylab, y, cl, lcl, ucl, labels = seq_along(y)) {
  n <- length(y)
  lcl <- rep_len(lcl, n); ucl <- rep_len(ucl, n); cl <- rep_len(cl, n)
  viol <- spc_rules(y, cl, lcl, ucl)
  structure(list(title = title, ylab = ylab, y = y, cl = cl, lcl = lcl,
                 ucl = ucl, labels = labels, violations = viol),
            class = "spc_chart")
}

# Indices of points used to estimate limits (all, or the first `n_est`).
est_index <- function(m, n_est) {
  if (is.null(n_est) || is.na(n_est) || n_est <= 0 || n_est >= m) seq_len(m)
  else seq_len(n_est)
}

spc_imr <- function(x, k = 3, n_est = NULL) {
  x <- x[!is.na(x)]
  if (length(x) < 3) stop("At least three observations are needed.")
  ix <- est_index(length(x), n_est)
  xe <- x[ix]
  mr_e <- abs(diff(xe))
  mrbar <- mean(mr_e)
  sigma <- mrbar / spc_d2(2)
  xbar <- mean(xe)
  mr <- abs(diff(x))
  r <- spc_d3(2) / spc_d2(2)
  list(
    sigma = sigma,
    charts = list(
      new_chart("Individuals (I) chart", "Individual value", x, xbar,
                xbar - k * sigma, xbar + k * sigma),
      new_chart("Moving range (MR) chart", "Moving range", c(NA, mr), mrbar,
                max(0, mrbar * (1 - k * r)), mrbar * (1 + k * r))
    )
  )
}

spc_xbar_r <- function(sg, k = 3, n_est = NULL) {
  n <- vapply(sg, length, integer(1))
  ix <- est_index(length(sg), n_est)
  means <- vapply(sg, mean, numeric(1))
  ranges <- vapply(sg, function(v) diff(range(v)), numeric(1))
  d2 <- vapply(n, spc_d2, numeric(1))
  d3 <- vapply(n, spc_d3, numeric(1))
  sigma <- mean(ranges[ix] / d2[ix])
  grand <- sum(means[ix] * n[ix]) / sum(n[ix])
  rcl <- d2 * sigma
  list(
    sigma = sigma,
    charts = list(
      new_chart("Xbar chart", "Subgroup mean", unname(means), grand,
                grand - k * sigma / sqrt(n), grand + k * sigma / sqrt(n), names(sg)),
      new_chart("R chart", "Subgroup range", unname(ranges), rcl,
                pmax(0, rcl - k * d3 * sigma), rcl + k * d3 * sigma, names(sg))
    )
  )
}

spc_xbar_s <- function(sg, k = 3, n_est = NULL) {
  n <- vapply(sg, length, integer(1))
  ix <- est_index(length(sg), n_est)
  means <- vapply(sg, mean, numeric(1))
  sds <- vapply(sg, sd, numeric(1))
  c4 <- vapply(n, spc_c4, numeric(1))
  sigma <- mean(sds[ix] / c4[ix])
  grand <- sum(means[ix] * n[ix]) / sum(n[ix])
  scl <- c4 * sigma
  list(
    sigma = sigma,
    charts = list(
      new_chart("Xbar chart", "Subgroup mean", unname(means), grand,
                grand - k * sigma / sqrt(n), grand + k * sigma / sqrt(n), names(sg)),
      new_chart("S chart", "Subgroup std. dev.", unname(sds), scl,
                pmax(0, scl - k * sigma * sqrt(1 - c4^2)),
                scl + k * sigma * sqrt(1 - c4^2), names(sg))
    )
  )
}

# Attribute charts. `d` = defectives or defects per sample, `n` = sample size.
check_counts <- function(d, n = NULL) {
  if (!is.numeric(d) || any(d < 0, na.rm = TRUE)) stop("Counts must be non-negative numbers.")
  if (!is.null(n) && (!is.numeric(n) || any(n <= 0, na.rm = TRUE))) stop("Sample sizes must be positive numbers.")
  if (!is.null(n) && any(d > n, na.rm = TRUE)) stop("A count is larger than its sample size.")
}

spc_p <- function(d, n, k = 3, n_est = NULL) {
  check_counts(d, n)
  n <- rep_len(n, length(d))
  ix <- est_index(length(d), n_est)
  pbar <- sum(d[ix]) / sum(n[ix])
  se <- sqrt(pbar * (1 - pbar) / n)
  list(charts = list(new_chart("P chart", "Proportion defective", d / n, pbar,
                               pmax(0, pbar - k * se), pmin(1, pbar + k * se))))
}

spc_np <- function(d, n, k = 3, n_est = NULL) {
  check_counts(d, n)
  if (length(unique(n)) != 1) stop("An NP chart needs a constant sample size; use a P chart instead.")
  n <- n[1]
  ix <- est_index(length(d), n_est)
  pbar <- sum(d[ix]) / (n * length(ix))
  se <- sqrt(n * pbar * (1 - pbar))
  list(charts = list(new_chart("NP chart", "Number defective", d, n * pbar,
                               max(0, n * pbar - k * se), n * pbar + k * se)))
}

spc_c <- function(d, k = 3, n_est = NULL) {
  check_counts(d)
  ix <- est_index(length(d), n_est)
  cbar <- mean(d[ix])
  list(charts = list(new_chart("C chart", "Number of defects", d, cbar,
                               max(0, cbar - k * sqrt(cbar)), cbar + k * sqrt(cbar))))
}

spc_u <- function(d, n, k = 3, n_est = NULL) {
  check_counts(d, n)
  n <- rep_len(n, length(d))
  ix <- est_index(length(d), n_est)
  ubar <- sum(d[ix]) / sum(n[ix])
  se <- sqrt(ubar / n)
  list(charts = list(new_chart("U chart", "Defects per unit", d / n, ubar,
                               pmax(0, ubar - k * se), ubar + k * se)))
}

# Plot one chart with ggplot2. Points that break a run rule are red.
plot_chart <- function(ch) {
  df <- data.frame(i = seq_along(ch$y), y = ch$y, cl = ch$cl,
                   lcl = ch$lcl, ucl = ch$ucl,
                   flag = vapply(ch$violations, length, integer(1)) > 0)
  df <- df[!is.na(df$y), ]
  step_limits <- length(unique(round(ch$ucl, 10))) > 1
  last <- df[nrow(df), ]
  lab <- function(v) formatC(v, digits = 4, format = "g")
  g <- ggplot2::ggplot(df, ggplot2::aes(i, y)) +
    ggplot2::geom_line(colour = "#4a6fa5") +
    ggplot2::geom_point(ggplot2::aes(colour = flag), size = 2) +
    ggplot2::scale_colour_manual(values = c(`FALSE` = "#4a6fa5", `TRUE` = "#c0392b"), guide = "none") +
    ggplot2::labs(title = ch$title, x = "Sample", y = ch$ylab) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  if (step_limits) {
    g <- g + ggplot2::geom_step(ggplot2::aes(y = ucl), colour = "#c0392b", direction = "mid") +
      ggplot2::geom_step(ggplot2::aes(y = lcl), colour = "#c0392b", direction = "mid") +
      ggplot2::geom_line(ggplot2::aes(y = cl), colour = "#2e7d32", linetype = 2)
  } else {
    g <- g + ggplot2::geom_hline(yintercept = c(last$lcl, last$ucl), colour = "#c0392b") +
      ggplot2::geom_hline(yintercept = last$cl, colour = "#2e7d32", linetype = 2)
  }
  g + ggplot2::annotate("label", x = Inf, y = c(last$ucl, last$cl, last$lcl),
                        label = c(paste("UCL =", lab(last$ucl)), paste("CL =", lab(last$cl)),
                                  paste("LCL =", lab(last$lcl))),
                        hjust = 1, size = 3.3, label.size = 0, fill = "white", alpha = 0.8)
}

# Table of rule violations for the results pane.
violation_table <- function(charts) {
  rule_text <- c("Beyond control limits", "9 points on one side of CL", "6 points trending")
  rows <- lapply(charts, function(ch) {
    hit <- which(vapply(ch$violations, length, integer(1)) > 0)
    if (!length(hit)) return(NULL)
    data.frame(Chart = ch$title,
               Sample = as.character(ch$labels[hit]),
               Value = signif(ch$y[hit], 5),
               Rule = vapply(ch$violations[hit], function(r) paste(rule_text[r], collapse = "; "), ""),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
