# Process capability calculations (normal and nonparametric).
#
# Adds to SixSigma's Cp/Cpk/Z, following the qcr package (GPL >= 2):
#  * within-subgroup sigma (Cp, Cpk) as well as overall sigma (Pp, Ppk);
#  * Cpm (Taguchi) with the Boyles (1991) confidence interval and Cpmk;
#  * Vannman's (1995) unified family Cp(u, v) and her capability plot;
#  * Tong & Chen (1998) nonparametric indices CNp, CNpk, CNpm, CNpmk.

# Within-subgroup sigma.
#   method "mr": average moving range / d2(2) (individual values)
#   method "pooled": pooled standard deviation of subgroups / c4
cap_within_sigma <- function(x, method = "mr", groups = NULL) {
  if (method == "mr") {
    x <- x[!is.na(x)]
    if (length(x) < 2) stop("At least two values are needed.")
    return(mean(abs(diff(x))) / unname(SixSigma::ss.cc.getd2(2)))
  }
  if (is.null(groups)) stop("Choose how the data are split into subgroups.")
  keep <- !is.na(x) & !is.na(groups)
  sp <- split(x[keep], factor(groups[keep], levels = unique(groups[keep])))
  sp <- sp[lengths(sp) >= 2]
  if (!length(sp)) stop("Each subgroup needs at least two values.")
  df <- sum(lengths(sp) - 1)
  sp_pooled <- sqrt(sum(vapply(sp, function(v) (length(v) - 1) * stats::var(v), numeric(1))) / df)
  sp_pooled / unname(SixSigma::ss.cc.getc4(df + 1))
}

# Vannman's family: Cp(u, v) = (d - u|mu - m|) / (3 sqrt(sigma^2 + v (mu - T)^2)).
vannman_index <- function(mu, sigma, lsl, usl, target, u, v, k = 3) {
  if (is.na(lsl) || is.na(usl)) return(NA_real_)
  d <- (usl - lsl) / 2; m <- (usl + lsl) / 2
  (d - u * abs(mu - m)) / (k * sqrt(sigma^2 + v * (mu - target)^2))
}

# Normal-theory indices with confidence intervals.
cap_table <- function(x, lsl, usl, target, sigma_within = NA, conf = 0.95) {
  x <- x[!is.na(x)]
  n <- length(x); mu <- mean(x); so <- stats::sd(x)
  if (n < 3) stop("At least three values are needed.")
  a <- 1 - conf; z <- stats::qnorm(1 - a / 2)
  both <- !is.na(lsl) && !is.na(usl)
  one <- function(s) {
    cp <- if (both) (usl - lsl) / (6 * s) else NA
    cpl <- if (!is.na(lsl)) (mu - lsl) / (3 * s) else NA
    cpu <- if (!is.na(usl)) (usl - mu) / (3 * s) else NA
    cpk <- min(cpl, cpu, na.rm = TRUE)
    cpm <- vannman_index(mu, s, lsl, usl, target, 0, 1)
    cpmk <- vannman_index(mu, s, lsl, usl, target, 1, 1)
    ci_cp <- cp * sqrt(stats::qchisq(c(a / 2, 1 - a / 2), n - 1) / (n - 1))
    bis <- function(c) c * (1 + c(-1, 1) * z * sqrt(1 / (9 * n * c^2) + 1 / (2 * (n - 1))))
    xi <- (mu - target) / s
    nu <- n * (1 + xi^2)^2 / (1 + 2 * xi^2)
    ci_cpm <- cpm * sqrt(stats::qchisq(c(a / 2, 1 - a / 2), nu) / nu)
    list(val = c(cp, cpl, cpu, cpk, cpm, cpmk),
         lo = c(ci_cp[1], bis(cpl)[1], bis(cpu)[1], bis(cpk)[1], ci_cpm[1], NA),
         hi = c(ci_cp[2], bis(cpl)[2], bis(cpu)[2], bis(cpk)[2], ci_cpm[2], NA))
  }
  ov <- one(so)
  out <- data.frame(Index = c("Pp", "PPL", "PPU", "Ppk", "Cpm", "Cpmk"),
                    `Overall` = ov$val, `Overall CI lower` = ov$lo, `Overall CI upper` = ov$hi,
                    check.names = FALSE)
  if (!is.na(sigma_within)) {
    wi <- one(sigma_within)
    out <- data.frame(Index = c("Cp / Pp", "CPL / PPL", "CPU / PPU", "Cpk / Ppk", "Cpm", "Cpmk"),
                      Within = wi$val, `Within CI lower` = wi$lo, `Within CI upper` = wi$hi,
                      out[, -1], check.names = FALSE)
  }
  out
}

cap_ppm <- function(x, lsl, usl, sigma_within = NA) {
  x <- x[!is.na(x)]; mu <- mean(x)
  exp_ppm <- function(s) c(if (is.na(lsl)) 0 else stats::pnorm(lsl, mu, s),
                           if (is.na(usl)) 0 else stats::pnorm(usl, mu, s, lower.tail = FALSE)) * 1e6
  obs <- c(if (is.na(lsl)) 0 else mean(x < lsl), if (is.na(usl)) 0 else mean(x > usl)) * 1e6
  out <- data.frame(` ` = c("PPM < LSL", "PPM > USL", "PPM total"),
                    Observed = c(obs, sum(obs)), `Expected overall` = c(exp_ppm(stats::sd(x)), sum(exp_ppm(stats::sd(x)))),
                    check.names = FALSE)
  if (!is.na(sigma_within)) out$`Expected within` <- c(exp_ppm(sigma_within), sum(exp_ppm(sigma_within)))
  out
}

# Tong & Chen (1998) nonparametric indices: sigma is replaced by the
# 0.135%-99.865% percentile range / 6, and the mean by the median.
cap_nonparametric <- function(x, lsl, usl, target) {
  x <- x[!is.na(x)]
  if (is.na(lsl) || is.na(usl)) stop("Nonparametric indices need both specification limits.")
  q <- stats::quantile(x, c(0.00135, 0.99865), names = FALSE)
  med <- stats::median(x); s <- (q[2] - q[1]) / 6
  data.frame(Index = c("CNp", "CNpk", "CNpm", "CNpmk"),
             Value = c(vannman_index(med, s, lsl, usl, target, 0, 0), vannman_index(med, s, lsl, usl, target, 1, 0),
                       vannman_index(med, s, lsl, usl, target, 0, 1), vannman_index(med, s, lsl, usl, target, 1, 1)),
             Meaning = c("Spread only", "Spread and centring", "Spread and distance from target",
                         "Spread, centring and target"))
}

# Vannman's capability plot: the region under each curve is where the index
# is at least k, in the (delta, gamma) = ((mu - T) / d, sigma / d) plane.
vannman_plot <- function(mu, sigma, lsl, usl, target, k = 1.33, label = "Process") {
  if (is.na(lsl) || is.na(usl)) stop("The capability plot needs both specification limits.")
  d <- (usl - lsl) / 2
  curve <- function(u, v, name) {
    lim <- if (u == 0 && v == 0) 1 else 1 / (u + 3 * k * sqrt(v))
    dl <- seq(-lim, lim, length.out = 401)
    g <- sqrt(pmax(0, (1 - u * abs(dl))^2 / (9 * k^2) - v * dl^2))
    data.frame(delta = dl, gamma = g, Index = name)
  }
  cv <- rbind(curve(0, 0, "Cp"), curve(1, 0, "Cpk"), curve(0, 1, "Cpm"), curve(1, 1, "Cpmk"))
  cv$Index <- factor(cv$Index, levels = c("Cp", "Cpk", "Cpm", "Cpmk"))
  pt <- data.frame(delta = (mu - target) / d, gamma = sigma / d)
  ymax <- max(cv$gamma, pt$gamma) * 1.15
  ggplot2::ggplot(cv, ggplot2::aes(delta, gamma, colour = Index)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(data = pt, ggplot2::aes(delta, gamma), inherit.aes = FALSE, colour = "#c0392b", size = 3.5) +
    ggplot2::annotate("text", x = pt$delta, y = pt$gamma, label = label, vjust = -1.2, colour = "#c0392b") +
    ggplot2::scale_colour_manual(values = c(Cp = "#9bb7d4", Cpk = "#4a6fa5", Cpm = "#2e7d32", Cpmk = "#8e44ad")) +
    ggplot2::coord_cartesian(ylim = c(0, ymax)) +
    ggplot2::labs(title = sprintf("Capability plot (k = %g)", k),
                  subtitle = "The process is capable for an index when its point lies below that index's curve",
                  x = "δ = (mean - target) / d", y = "γ = sigma / d", colour = NULL) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), legend.position = "top")
}
