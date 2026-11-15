# Acceptance sampling: attribute (single and double) and variables plans.
#
# OC = probability of accepting a lot with fraction defective p.
# Attribute plans: binomial (default), hypergeometric when the lot size N is
# given, or Poisson. Rectifying inspection (rejected lots 100% inspected):
#   AOQ = Pa * p * (N - n) / N,  ATI = n + (1 - Pa) (N - n).

pa_single <- function(p, n, c, N = NA, model = "binomial") {
  vapply(p, function(pp) {
    switch(model,
      binomial = stats::pbinom(c, n, pp),
      poisson = stats::ppois(c, n * pp),
      hypergeometric = {
        if (is.na(N)) stop("Enter the lot size for the hypergeometric model.")
        D <- round(pp * N)
        stats::phyper(c, D, N - D, n)
      },
      stop("Unknown model."))
  }, numeric(1))
}

# Double sampling: accept on the first sample if d1 <= c1, reject if
# d1 >= r1; otherwise take n2 more and accept if d1 + d2 <= c2.
pa_double <- function(p, n1, c1, r1, n2, c2) {
  if (!(c1 < r1 && c2 >= c1 && r1 - 1 <= c2 + 1)) {
    if (!(c1 < r1)) stop("The first rejection number must exceed the first acceptance number.")
  }
  vapply(p, function(pp) {
    acc1 <- stats::pbinom(c1, n1, pp)
    mid <- if (r1 - 1 >= c1 + 1) (c1 + 1):(r1 - 1) else integer(0)
    acc2 <- sum(vapply(mid, function(d1) stats::dbinom(d1, n1, pp) * stats::pbinom(c2 - d1, n2, pp), numeric(1)))
    acc1 + acc2
  }, numeric(1))
}

asn_double <- function(p, n1, c1, r1, n2) {
  vapply(p, function(pp) {
    second <- stats::pbinom(r1 - 1, n1, pp) - stats::pbinom(c1, n1, pp)
    n1 + n2 * second
  }, numeric(1))
}

# Smallest single sampling plan with Pa(AQL) >= 1 - alpha and Pa(LTPD) <= beta.
design_attribute <- function(aql, alpha, ltpd, beta, model = "binomial", N = NA, c_max = 50) {
  if (!(aql > 0 && ltpd > aql && ltpd < 1)) stop("Need 0 < AQL < LTPD < 1.")
  if (!(alpha > 0 && alpha < 0.5 && beta > 0 && beta < 0.5)) stop("Risks must be between 0 and 0.5.")
  n_cap <- if (!is.na(N)) N else 100000
  for (c in 0:c_max) {
    # Pa decreases with n: smallest n meeting the consumer's risk ...
    lo <- c + 1; hi <- n_cap
    if (pa_single(ltpd, hi, c, N, model) > beta) next
    while (lo < hi) { mid <- (lo + hi) %/% 2; if (pa_single(ltpd, mid, c, N, model) <= beta) hi <- mid else lo <- mid + 1 }
    n <- lo
    # ... then check the producer's risk at that n.
    if (pa_single(aql, n, c, N, model) >= 1 - alpha) {
      return(list(n = n, c = c, pa_aql = pa_single(aql, n, c, N, model), pa_ltpd = pa_single(ltpd, n, c, N, model)))
    }
  }
  stop("No plan with acceptance number up to ", c_max, " meets both risks; widen the gap between AQL and LTPD.")
}

# Variables sampling (k-method, one specification limit). Accept the lot if
# (USL - mean) / s >= k  (or (mean - LSL) / s >= k).
design_variables <- function(aql, alpha, ltpd, beta, sigma_known = FALSE) {
  if (!(aql > 0 && ltpd > aql && ltpd < 1)) stop("Need 0 < AQL < LTPD < 1.")
  za <- stats::qnorm(1 - alpha); zb <- stats::qnorm(1 - beta)
  z1 <- stats::qnorm(1 - aql); z2 <- stats::qnorm(1 - ltpd)
  k <- (z1 * zb + z2 * za) / (za + zb)
  n <- ((za + zb) / (z1 - z2))^2
  if (!sigma_known) n <- n * (1 + k^2 / 2)
  list(n = ceiling(n), k = k)
}

pa_variables <- function(p, n, k, sigma_known = FALSE) {
  zp <- stats::qnorm(1 - p)
  stats::pnorm(sqrt(n) * (zp - k) / (if (sigma_known) 1 else sqrt(1 + k^2 / 2)))
}

variables_decision <- function(x, n_req, k, lsl = NA, usl = NA, sigma = NA) {
  x <- x[!is.na(x)]
  if (is.na(lsl) && is.na(usl)) stop("Enter the specification limit.")
  s <- if (is.na(sigma)) stats::sd(x) else sigma
  m <- mean(x)
  q <- c(if (!is.na(usl)) (usl - m) / s, if (!is.na(lsl)) (m - lsl) / s)
  qmin <- min(q)
  list(table = data.frame(Statistic = c("Sample size", "Required n", "Mean", if (is.na(sigma)) "Std. dev. (sample)" else "Std. dev. (known)",
                                        "Quality index Q = distance to spec / s", "Acceptability constant k"),
                          Value = c(length(x), n_req, m, s, qmin, k)),
       accept = qmin >= k,
       note = if (length(x) < n_req) sprintf("Warning: the sample has %d values but the plan needs %d.", length(x), n_req) else NULL)
}

sampling_curves <- function(p, pa, n, N = NA, aql = NA, ltpd = NA, asn = NULL) {
  d <- data.frame(p = p, Pa = pa)
  th <- ggplot2::theme_bw(base_size = 12)
  mk <- data.frame(p = c(aql, ltpd), lab = c("AQL", "LTPD"))
  mk <- mk[!is.na(mk$p), ]
  oc <- ggplot2::ggplot(d, ggplot2::aes(p, Pa)) + ggplot2::geom_line(colour = "#4a6fa5", linewidth = 1) +
    ggplot2::scale_x_continuous(labels = scales::percent) + ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::labs(title = "Operating characteristic (OC) curve", x = "Lot fraction defective", y = "Probability of acceptance") + th
  if (nrow(mk)) oc <- oc + ggplot2::geom_vline(data = mk, ggplot2::aes(xintercept = p), linetype = 2, colour = "#c0392b") +
    ggplot2::geom_text(data = mk, ggplot2::aes(x = p, y = 0.05, label = lab), hjust = -0.1, colour = "#c0392b", size = 3.5)
  out <- list(oc)
  Nn <- if (is.na(N)) Inf else N
  aoq <- pa * p * (if (is.finite(Nn)) (Nn - n) / Nn else 1)
  aoql <- max(aoq)
  out[[2]] <- ggplot2::ggplot(data.frame(p = p, aoq = aoq), ggplot2::aes(p, aoq)) +
    ggplot2::geom_line(colour = "#4a6fa5", linewidth = 1) + ggplot2::geom_hline(yintercept = aoql, linetype = 2, colour = "#c0392b") +
    ggplot2::scale_x_continuous(labels = scales::percent) + ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(title = "Average outgoing quality (AOQ)", subtitle = sprintf("AOQL = %.3f%% (rejected lots fully inspected)", 100 * aoql),
                  x = "Incoming fraction defective", y = "AOQ") + th
  if (!is.null(asn)) {
    out[[3]] <- ggplot2::ggplot(data.frame(p = p, asn = asn), ggplot2::aes(p, asn)) + ggplot2::geom_line(colour = "#4a6fa5", linewidth = 1) +
      ggplot2::scale_x_continuous(labels = scales::percent) +
      ggplot2::labs(title = "Average sample number (ASN)", x = "Lot fraction defective", y = "Average units inspected") + th
  } else if (is.finite(Nn)) {
    ati <- n + (1 - pa) * (Nn - n)
    out[[3]] <- ggplot2::ggplot(data.frame(p = p, ati = ati), ggplot2::aes(p, ati)) + ggplot2::geom_line(colour = "#4a6fa5", linewidth = 1) +
      ggplot2::scale_x_continuous(labels = scales::percent) +
      ggplot2::labs(title = "Average total inspection (ATI)", x = "Incoming fraction defective", y = "Units inspected per lot") + th
  }
  list(plots = out, aoql = aoql, aoql_p = p[which.max(aoq)])
}
