# Measurement system analysis extras: type 1 gage study (Cg, Cgk), gage
# linearity and bias (AIAG MSA), Gage R&R worksheet generator, and the
# multi-vari chart.

# Type 1 gage study (Bosch Booklet 10 / Minitab): repeated measurements of one
# reference part.
#   Cg  = (K/100 * Tol) / (L * s)
#   Cgk = (K/200 * Tol - |mean - ref|) / (L/2 * s)
gage_type1 <- function(x, ref, tol, K = 20, L = 6, alpha = 0.05) {
  x <- x[!is.na(x)]
  if (length(x) < 10) stop("A type 1 study needs at least 10 measurements (25-50 are recommended).")
  if (!isTRUE(tol > 0)) stop("Enter the tolerance (USL - LSL).")
  m <- mean(x); s <- stats::sd(x)
  if (s == 0) stop("All measurements are identical; the gage resolution is too coarse for this study.")
  cg <- (K / 100 * tol) / (L * s)
  cgk <- (K / 200 * tol - abs(m - ref)) / (L / 2 * s)
  tt <- stats::t.test(x, mu = ref)
  list(
    stats = data.frame(Statistic = c("Reference", "Mean", "Std. dev.", "Bias", "Bias p-value (t-test)",
                                     "Cg", "Cgk", "%Var (repeatability)", "%Var (repeatability and bias)"),
                       Value = c(ref, m, s, m - ref, tt$p.value, cg, cgk, K / cg, K / cgk)),
    cg = cg, cgk = cgk, mean = m, sd = s, ref = ref, tol = tol, K = K, x = x,
    verdict = if (cg >= 1.33 && cgk >= 1.33) "Capable: Cg and Cgk are both at least 1.33."
              else "Not capable: Cg or Cgk is below 1.33."
  )
}

gage_type1_plot <- function(g) {
  d <- data.frame(i = seq_along(g$x), x = g$x)
  band <- g$K / 200 * g$tol
  lines <- data.frame(y = c(g$ref, g$ref - band, g$ref + band, g$mean),
                      lab = c("Ref", sprintf("Ref - %g%% Tol/2", g$K), sprintf("Ref + %g%% Tol/2", g$K), "Mean"),
                      col = c("#2e7d32", "#c0392b", "#c0392b", "#4a6fa5"), lt = c(1, 2, 2, 3))
  ggplot2::ggplot(d, ggplot2::aes(i, x)) +
    ggplot2::geom_hline(data = lines, ggplot2::aes(yintercept = y), colour = lines$col, linetype = lines$lt) +
    ggplot2::geom_line(colour = "#9bb7d4") + ggplot2::geom_point(colour = "#4a6fa5", size = 2) +
    ggplot2::annotate("label", x = Inf, y = lines$y, label = lines$lab, hjust = 1, size = 3.2, label.size = 0,
                      fill = "white", alpha = 0.8) +
    ggplot2::labs(title = "Run chart of the measurements",
                  subtitle = sprintf("Cg = %.2f   Cgk = %.2f   (K = %g%%)", g$cg, g$cgk, g$K),
                  x = "Measurement order", y = "Measured value") +
    ggplot2::theme_minimal(base_size = 13) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

# Gage linearity and bias (AIAG MSA, 4th edition).
gage_linearity <- function(df, ref_col, meas_col, pv = NA, conf = 0.95) {
  ref <- df[[ref_col]]; y <- df[[meas_col]]
  if (!is.numeric(ref) || !is.numeric(y)) stop("Reference and measurement columns must be numeric.")
  keep <- !is.na(ref) & !is.na(y); ref <- ref[keep]; y <- y[keep]
  if (length(unique(ref)) < 2) stop("Linearity needs parts with at least two different reference values.")
  bias <- y - ref
  fit <- stats::lm(bias ~ ref)
  s <- summary(fit)$coefficients
  per <- do.call(rbind, lapply(sort(unique(ref)), function(r) {
    b <- bias[ref == r]
    p <- if (length(b) > 1 && stats::sd(b) > 0) stats::t.test(b)$p.value else NA
    data.frame(Reference = r, N = length(b), `Average bias` = mean(b), `p-value` = p, check.names = FALSE)
  }))
  avg <- mean(bias)
  p_avg <- if (stats::sd(bias) > 0) stats::t.test(bias)$p.value else NA
  slope <- s["ref", 1]
  list(
    fit = fit, ref = ref, bias = bias, per = per, conf = conf,
    coef = data.frame(Term = c("Constant", "Slope"), Coef = s[, 1], `SE coef` = s[, 2], `p-value` = s[, 4],
                      check.names = FALSE, row.names = NULL),
    summary = data.frame(
      Statistic = c("S", "R-sq (%)", "Linearity = |slope| x PV", "% Linearity", "Average bias",
                    "Average bias p-value", "% Bias = |avg bias| / PV"),
      Value = c(summary(fit)$sigma, 100 * summary(fit)$r.squared,
                if (is.na(pv)) NA else abs(slope) * pv, 100 * abs(slope),
                avg, p_avg, if (is.na(pv)) NA else 100 * abs(avg) / pv)),
    verdict = c(
      if (s["ref", 4] < 1 - conf) "Linearity: the bias changes significantly across the measuring range."
      else "Linearity: no significant change in bias across the range.",
      if (!is.na(p_avg) && p_avg < 1 - conf) "Bias: the average bias is significantly different from zero."
      else "Bias: the average bias is not significantly different from zero.")
  )
}

gage_linearity_plot <- function(g) {
  grid <- data.frame(ref = seq(min(g$ref), max(g$ref), length.out = 100))
  pr <- stats::predict(g$fit, grid, interval = "confidence", level = g$conf)
  band <- cbind(grid, as.data.frame(pr))
  ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40") +
    ggplot2::geom_ribbon(data = band, ggplot2::aes(ref, ymin = lwr, ymax = upr), fill = "#c9d8ea", alpha = 0.7) +
    ggplot2::geom_line(data = band, ggplot2::aes(ref, fit), colour = "#4a6fa5", linewidth = 1) +
    ggplot2::geom_point(data = data.frame(ref = g$ref, bias = g$bias), ggplot2::aes(ref, bias), colour = "#4a6fa5", alpha = 0.6) +
    ggplot2::geom_point(data = g$per, ggplot2::aes(Reference, `Average bias`), colour = "#c0392b", size = 3.2, shape = 18) +
    ggplot2::labs(title = "Gage linearity and bias",
                  subtitle = sprintf("Bias = %.4g %s %.4g x Reference   (band: %g%% confidence; red: average bias)",
                                     g$coef$Coef[1], if (g$coef$Coef[2] < 0) "-" else "+", abs(g$coef$Coef[2]), 100 * g$conf),
                  x = "Reference value", y = "Bias (measured - reference)") +
    ggplot2::theme_minimal(base_size = 13) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

# Worksheet for a crossed Gage R&R study.
gage_rr_worksheet <- function(parts, operators, replicates = 2, randomize = TRUE, seed = NA,
                              measurement = "Measurement") {
  if (parts < 2 || operators < 1 || replicates < 1) stop("Use at least 2 parts, 1 operator and 1 replicate.")
  ops <- if (is.character(operators)) operators else paste0("Op", seq_len(operators))
  d <- expand.grid(Part = paste0("P", seq_len(parts)), Operator = ops, Replicate = seq_len(replicates),
                   stringsAsFactors = FALSE)
  d <- d[order(d$Replicate, d$Operator, d$Part), ]
  if (!is.na(seed)) set.seed(seed)
  if (randomize) {
    # Randomize the part order within each operator and trial, as is usual.
    d <- do.call(rbind, lapply(split(d, list(d$Replicate, d$Operator), drop = TRUE),
                               function(b) b[sample(nrow(b)), ]))
    d <- d[order(d$Replicate, match(d$Operator, ops)), ]
  }
  d <- data.frame(Run = seq_len(nrow(d)), d, row.names = NULL)
  d[[measurement]] <- NA_real_
  d
}

# Multi-vari chart: response by up to three factors (x axis, lines, panels).
multivari_plot <- function(df, response, x, lines = NULL, panel = NULL) {
  y <- df[[response]]
  if (!is.numeric(y)) stop("The response must be numeric.")
  vars <- c(x, lines, panel)
  if (anyDuplicated(vars)) stop("Choose different columns for each role.")
  d <- df[!is.na(y) & stats::complete.cases(df[, vars, drop = FALSE]), , drop = FALSE]
  d$.x <- factor(d[[x]]); d$.y <- d[[response]]
  d$.l <- if (is.null(lines)) factor("all") else factor(d[[lines]])
  d$.p <- if (is.null(panel)) factor("all") else factor(d[[panel]])
  m <- stats::aggregate(.y ~ .x + .l + .p, data = d, FUN = mean)
  mp <- stats::aggregate(.y ~ .x + .p, data = d, FUN = mean)
  pd <- ggplot2::position_dodge(width = if (is.null(lines)) 0 else 0.3)
  g <- ggplot2::ggplot(d, ggplot2::aes(.x, .y, colour = .l, group = .l)) +
    ggplot2::geom_point(alpha = 0.45, position = pd) +
    ggplot2::geom_line(data = m, linewidth = 1, position = pd) + ggplot2::geom_point(data = m, size = 2.8, position = pd) +
    ggplot2::geom_line(data = mp, ggplot2::aes(.x, .y, group = 1), inherit.aes = FALSE, colour = "black",
                       linetype = 2, linewidth = 0.8) +
    ggplot2::labs(title = sprintf("Multi-vari chart for %s", response),
                  subtitle = "Small points: observations; large points and lines: means; dashed: overall mean at each level",
                  x = x, y = response, colour = lines) +
    ggplot2::theme_bw(base_size = 13) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  if (is.null(lines)) g <- g + ggplot2::scale_colour_manual(values = "#4a6fa5", guide = "none")
  if (!is.null(panel)) g <- g + ggplot2::facet_wrap(~.p, labeller = ggplot2::labeller(.p = function(v) paste(panel, "=", v)))
  g
}
