# Linear regression calculations (Stat > Regression).

bt <- function(x) paste0("`", gsub("`", "", x, fixed = TRUE), "`")

# Build and fit the model.
#   y          response column name
#   x          predictor column names (numeric or categorical)
#   interact   include all two-way interactions
#   poly       polynomial order when there is exactly one numeric predictor
fit_regression <- function(df, y, x, interact = FALSE, poly = 1) {
  if (!length(x)) stop("Choose at least one predictor.")
  if (y %in% x) stop("The response cannot also be a predictor.")
  if (!is.numeric(df[[y]])) stop("The response must be numeric.")
  d <- df[, c(y, x), drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  for (v in x) if (!is.numeric(d[[v]])) d[[v]] <- factor(d[[v]])
  single_num <- length(x) == 1 && is.numeric(d[[x]])
  terms <- if (single_num && poly > 1) {
    c(bt(x), vapply(2:poly, function(k) sprintf("I(%s^%d)", bt(x), k), ""))
  } else if (interact && length(x) > 1) {
    sprintf("(%s)^2", paste(bt(x), collapse = " + "))
  } else bt(x)
  f <- stats::as.formula(paste(bt(y), "~", paste(terms, collapse = " + ")))
  fit <- stats::lm(f, data = d)
  n <- nrow(d); p <- length(stats::coef(fit))
  if (n <= p) stop("Not enough complete rows (", n, ") to estimate ", p, " coefficients.")
  if (any(is.na(stats::coef(fit)))) stop("Some terms cannot be estimated because predictors are perfectly correlated.")
  list(fit = fit, data = d, y = y, x = x, single_num = single_num, poly = if (single_num) poly else 1)
}

reg_equation <- function(m) {
  cf <- stats::coef(m$fit)
  nm <- gsub("`", "", names(cf))
  parts <- vapply(seq_along(cf)[-1], function(i) {
    sprintf("%s %s %s", if (cf[i] < 0) "-" else "+", formatC(abs(cf[i]), digits = 5, format = "g"), nm[i])
  }, "")
  paste(m$y, "=", formatC(cf[1], digits = 5, format = "g"), paste(parts, collapse = " "))
}

reg_vif <- function(fit) {
  X <- stats::model.matrix(fit)[, -1, drop = FALSE]
  if (ncol(X) < 2) return(rep(NA_real_, ncol(X)))
  vapply(seq_len(ncol(X)), function(j) {
    r2 <- summary(stats::lm(X[, j] ~ X[, -j]))$r.squared
    if (r2 >= 1) Inf else 1 / (1 - r2)
  }, numeric(1))
}

reg_coefficients <- function(m) {
  s <- summary(m$fit)$coefficients
  data.frame(Term = gsub("`", "", rownames(s)), Coefficient = s[, 1], `SE coef` = s[, 2],
             `T` = s[, 3], `p-value` = s[, 4], VIF = c(NA, reg_vif(m$fit)), check.names = FALSE)
}

reg_summary <- function(m) {
  fit <- m$fit; s <- summary(fit)
  h <- stats::hatvalues(fit); r <- stats::residuals(fit)
  y <- stats::model.response(stats::model.frame(fit))
  press <- sum((r / (1 - h))^2)
  data.frame(S = s$sigma, `R-sq (%)` = 100 * s$r.squared, `R-sq (adj) (%)` = 100 * s$adj.r.squared,
             `R-sq (pred) (%)` = 100 * (1 - press / sum((y - mean(y))^2)), PRESS = press,
             N = length(y), check.names = FALSE)
}

reg_anova <- function(m) {
  fit <- m$fit
  y <- stats::model.response(stats::model.frame(fit))
  sst <- sum((y - mean(y))^2); sse <- sum(stats::residuals(fit)^2)
  dfr <- length(stats::coef(fit)) - 1; dfe <- stats::df.residual(fit)
  msr <- (sst - sse) / dfr; mse <- sse / dfe
  overall <- data.frame(Source = c("Regression", "Error", "Total"), DF = c(dfr, dfe, dfr + dfe),
                        `Adj SS` = c(sst - sse, sse, sst), `Adj MS` = c(msr, mse, NA),
                        `F` = c(msr / mse, NA, NA),
                        `p-value` = c(stats::pf(msr / mse, dfr, dfe, lower.tail = FALSE), NA, NA),
                        check.names = FALSE)
  # Adjusted (marginal) SS for each term that can be dropped while respecting hierarchy.
  d1 <- stats::drop1(fit, test = "F")[-1, , drop = FALSE]
  terms <- data.frame(Source = paste0("  ", gsub("`", "", rownames(d1))), DF = d1$Df, `Adj SS` = d1$`Sum of Sq`,
                      `Adj MS` = d1$`Sum of Sq` / d1$Df, `F` = d1$`F value`, `p-value` = d1$`Pr(>F)`,
                      check.names = FALSE)
  rbind(overall[1, ], terms, overall[2:3, ])
}

reg_unusual <- function(m) {
  fit <- m$fit
  sr <- stats::rstandard(fit); h <- stats::hatvalues(fit)
  p <- length(stats::coef(fit)); n <- length(h)
  flag <- abs(sr) > 2 | h > 3 * p / n
  if (!any(flag, na.rm = TRUE)) return(NULL)
  i <- which(flag)
  y <- stats::model.response(stats::model.frame(fit))
  data.frame(Row = names(sr)[i], Observed = y[i], Fit = stats::fitted(fit)[i],
             `Std resid` = sr[i], Leverage = h[i],
             Reason = ifelse(abs(sr[i]) > 2 & h[i] > 3 * p / n, "Large residual and leverage",
                             ifelse(abs(sr[i]) > 2, "Large residual", "High leverage")),
             check.names = FALSE)
}

reg_predict <- function(m, newdata, conf = 0.95) {
  for (v in m$x) if (is.factor(m$data[[v]])) newdata[[v]] <- factor(newdata[[v]], levels = levels(m$data[[v]]))
  ci <- stats::predict(m$fit, newdata, interval = "confidence", level = conf, se.fit = TRUE)
  pi <- stats::predict(m$fit, newdata, interval = "prediction", level = conf)
  data.frame(Fit = ci$fit[, "fit"], `SE fit` = ci$se.fit, `CI lower` = ci$fit[, "lwr"], `CI upper` = ci$fit[, "upr"],
             `PI lower` = pi[, "lwr"], `PI upper` = pi[, "upr"], check.names = FALSE)
}

# Fitted line plot with confidence and prediction bands (one numeric predictor).
reg_fitted_line <- function(m, conf = 0.95) {
  x <- m$x; d <- m$data
  grid <- data.frame(seq(min(d[[x]]), max(d[[x]]), length.out = 200)); names(grid) <- x
  ci <- stats::predict(m$fit, grid, interval = "confidence", level = conf)
  pi <- stats::predict(m$fit, grid, interval = "prediction", level = conf)
  band <- data.frame(x = grid[[x]], fit = ci[, "fit"], clo = ci[, "lwr"], chi = ci[, "upr"],
                     plo = pi[, "lwr"], phi = pi[, "upr"])
  pts <- data.frame(x = d[[x]], y = d[[m$y]])
  s <- reg_summary(m)
  ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = band, ggplot2::aes(x, ymin = plo, ymax = phi), fill = "#f3d9d6", alpha = 0.7) +
    ggplot2::geom_ribbon(data = band, ggplot2::aes(x, ymin = clo, ymax = chi), fill = "#c9d8ea", alpha = 0.9) +
    ggplot2::geom_point(data = pts, ggplot2::aes(x, y), colour = "#4a6fa5", size = 2.2) +
    ggplot2::geom_line(data = band, ggplot2::aes(x, fit), colour = "#c0392b", linewidth = 1) +
    ggplot2::labs(title = "Fitted line plot", x = x, y = m$y,
                  subtitle = sprintf("%s\nS = %.4g   R-sq = %.1f%%   R-sq(adj) = %.1f%%   (blue = %g%% CI, pink = %g%% PI)",
                                     reg_equation(m), s$S, s$`R-sq (%)`, s$`R-sq (adj) (%)`, 100 * conf, 100 * conf)) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

# Four-in-one residual plots.
reg_residual_plots <- function(fit) {
  r <- stats::rstandard(fit); f <- stats::fitted(fit)
  d <- data.frame(r = r, f = f, i = seq_along(r))
  th <- ggplot2::theme_minimal(base_size = 12)
  p1 <- ggplot2::ggplot(d, ggplot2::aes(sample = r)) + ggplot2::stat_qq(colour = "#4a6fa5") +
    ggplot2::stat_qq_line(colour = "#c0392b") + ggplot2::labs(title = "Normal probability plot", x = "Theoretical quantiles", y = "Std. residual") + th
  p2 <- ggplot2::ggplot(d, ggplot2::aes(f, r)) + ggplot2::geom_hline(yintercept = c(-2, 0, 2), linetype = c(3, 2, 3), colour = "grey50") +
    ggplot2::geom_point(colour = "#4a6fa5") + ggplot2::labs(title = "Versus fits", x = "Fitted value", y = "Std. residual") + th
  p3 <- ggplot2::ggplot(d, ggplot2::aes(r)) + ggplot2::geom_histogram(bins = max(5, min(20, nrow(d) %/% 3)), fill = "#9bb7d4", colour = "white") +
    ggplot2::labs(title = "Histogram", x = "Std. residual", y = "Frequency") + th
  p4 <- ggplot2::ggplot(d, ggplot2::aes(i, r)) + ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
    ggplot2::geom_line(colour = "#9bb7d4") + ggplot2::geom_point(colour = "#4a6fa5") +
    ggplot2::labs(title = "Versus order", x = "Observation order", y = "Std. residual") + th
  list(p1, p2, p3, p4)
}

# Draw a list of ggplots in a grid.
draw_grid <- function(plots, ncol = 2) {
  nrow <- ceiling(length(plots) / ncol)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow, ncol)))
  for (i in seq_along(plots)) {
    print(plots[[i]], vp = grid::viewport(layout.pos.row = (i - 1) %/% ncol + 1, layout.pos.col = (i - 1) %% ncol + 1))
  }
  invisible(NULL)
}
