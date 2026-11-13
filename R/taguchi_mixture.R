# Taguchi signal-to-noise analysis and mixture (Scheffe) models.

# ---- Taguchi ---------------------------------------------------------------------

sn_ratio <- function(Y, type) {
  Y <- as.matrix(Y)
  apply(Y, 1, function(y) {
    y <- y[!is.na(y)]
    if (!length(y)) return(NA_real_)
    switch(type,
      larger = -10 * log10(mean(1 / y^2)),
      smaller = -10 * log10(mean(y^2)),
      nominal = if (length(y) < 2) NA_real_ else 10 * log10(mean(y)^2 / stats::var(y)),
      nominal2 = if (length(y) < 2) NA_real_ else -10 * log10(stats::var(y)),
      stop("Unknown S/N type."))
  })
}

sn_types <- c("Larger is better" = "larger", "Smaller is better" = "smaller",
              "Nominal is best (10 log mean²/s²)" = "nominal", "Nominal is best (-10 log s²)" = "nominal2")

# Response tables (mean per factor level), delta and rank for S/N and means.
taguchi_analyze <- function(df, factors, responses, type = "larger") {
  if (!length(factors)) stop("Choose the factor columns.")
  if (!length(responses)) stop("Choose the response column(s).")
  Y <- as.matrix(df[, responses, drop = FALSE])
  if (!is.numeric(Y)) stop("Response columns must be numeric.")
  if (type == "larger" && any(Y <= 0, na.rm = TRUE)) stop("Larger-is-better S/N needs positive responses.")
  if (type %in% c("nominal", "nominal2") && length(responses) < 2)
    stop("Nominal-is-best S/N needs two or more response columns (repeated measurements per run).")
  sn <- sn_ratio(Y, type); mu <- rowMeans(Y, na.rm = TRUE)
  ok <- !is.na(sn) & !is.na(mu)
  if (sum(ok) < 2) stop("Enter response values for the runs first.")
  tab <- function(v) {
    lv <- lapply(factors, function(f) tapply(v[ok], df[[f]][ok], mean))
    nl <- max(lengths(lv))
    out <- data.frame(Level = seq_len(nl))
    for (i in seq_along(factors)) { x <- lv[[i]]; out[[factors[i]]] <- c(unname(x), rep(NA, nl - length(x))) }
    delta <- vapply(lv, function(x) diff(range(x)), numeric(1))
    out <- rbind(out, c(NA, delta))
    out$Level <- c(as.character(seq_len(nl)), "Delta")
    rank_row <- c("Rank", rank(-delta, ties.method = "min"))
    list(table = out, rank = stats::setNames(rank(-delta, ties.method = "min"), factors), levels = lv)
  }
  sn_t <- tab(sn); mu_t <- tab(mu)
  best <- vapply(sn_t$levels, function(x) names(x)[which.max(x)], "")
  grand <- mean(sn[ok])
  pred_sn <- grand + sum(vapply(seq_along(factors), function(i) sn_t$levels[[i]][[best[i]]] - grand, numeric(1)))
  grand_mu <- mean(mu[ok])
  pred_mu <- grand_mu + sum(vapply(seq_along(factors), function(i) mu_t$levels[[i]][[best[i]]] - grand_mu, numeric(1)))
  list(sn = sn, mean = mu, sn_table = sn_t, mean_table = mu_t, factors = factors,
       best = data.frame(Factor = factors, `Best level (max S/N)` = best, `S/N rank` = sn_t$rank,
                         `Means rank` = mu_t$rank, check.names = FALSE, row.names = NULL),
       predicted = c(sn = pred_sn, mean = pred_mu), type = type)
}

taguchi_effects_plot <- function(a) {
  mk <- function(t, what) do.call(rbind, lapply(seq_along(a$factors), function(i) {
    x <- t$levels[[i]]
    data.frame(Factor = a$factors[i], Level = names(x), Value = unname(x), What = what)
  }))
  d <- rbind(mk(a$sn_table, "Mean of S/N ratios"), mk(a$mean_table, "Mean of means"))
  d$Factor <- factor(d$Factor, levels = a$factors)
  d$What <- factor(d$What, levels = c("Mean of S/N ratios", "Mean of means"))
  ggplot2::ggplot(d, ggplot2::aes(Level, Value, group = 1)) +
    ggplot2::geom_line(colour = "#4a6fa5", linewidth = 1) + ggplot2::geom_point(colour = "#4a6fa5", size = 2.5) +
    ggplot2::facet_grid(What ~ Factor, scales = "free") +
    ggplot2::labs(title = "Main effects plots for S/N ratios and means",
                  subtitle = sprintf("S/N: %s", names(sn_types)[sn_types == a$type]), x = "Level", y = NULL) +
    ggplot2::theme_bw(base_size = 12) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

# ---- Mixtures --------------------------------------------------------------------

mixture_fit <- function(df, response, components, model = "quadratic") {
  if (length(components) < 3) stop("Choose at least three components.")
  y <- df[[response]]
  if (!is.numeric(y)) stop("The response must be numeric.")
  keep <- !is.na(y) & stats::complete.cases(df[, components, drop = FALSE])
  X <- as.matrix(df[keep, components, drop = FALSE])
  if (any(X < 0)) stop("Component amounts cannot be negative.")
  X <- X / rowSums(X)  # proportions
  vars <- make.names(components)
  d <- data.frame(X, y[keep]); names(d) <- c(vars, ".resp")
  lin <- paste(vars, collapse = " + ")
  rhs <- switch(model,
    linear = lin,
    quadratic = sprintf("(%s)^2", lin),
    cubic = paste(sprintf("(%s)^2", lin), "+", paste(apply(utils::combn(vars, 3), 2, paste, collapse = ":"), collapse = " + ")),
    stop("Unknown model."))
  fit <- stats::lm(stats::as.formula(paste(".resp ~ -1 +", rhs)), data = d)
  if (nrow(d) <= length(stats::coef(fit))) stop("Not enough blends for the ", model, " model.")
  if (any(is.na(stats::coef(fit)))) stop("The ", model, " model cannot be estimated with these blends.")
  list(fit = fit, data = d, vars = vars, components = components, response = response, model = model)
}

mixture_coefficients <- function(m) {
  s <- summary(m$fit)$coefficients
  lab <- rownames(s)
  for (i in seq_along(m$vars)) lab <- gsub(paste0("\\b", m$vars[i], "\\b"), m$components[i], lab)
  y <- m$data$.resp; sse <- sum(stats::residuals(m$fit)^2)
  list(coef = data.frame(Term = gsub(":", "*", lab), Coef = s[, 1], `SE coef` = s[, 2], T = s[, 3],
                         `p-value` = s[, 4], check.names = FALSE),
       summary = data.frame(S = summary(m$fit)$sigma, `R-sq (%)` = 100 * (1 - sse / sum((y - mean(y))^2)),
                            Blends = length(y), check.names = FALSE))
}

# Ternary contour plot for three components (others held at fixed proportions).
mixture_ternary <- function(m, comps = m$components[1:3], hold = NULL, n = 150) {
  idx <- match(comps, m$components)
  others <- setdiff(seq_along(m$components), idx)
  fixed <- if (length(others)) vapply(others, function(i) if (is.null(hold)) 0 else hold[[m$components[i]]], numeric(1)) else numeric(0)
  rem <- 1 - sum(fixed)
  if (rem <= 0) stop("The fixed components already add up to 1.")
  # Regular Cartesian grid over the triangle (first component at the top);
  # points outside the triangle are left empty.
  h <- sqrt(3) / 2
  g <- expand.grid(x = seq(0, 1, length.out = n + 1), y = seq(0, h, length.out = n + 1))
  a <- g$y / h; b <- g$x - a / 2; c <- 1 - a - b
  inside <- a >= -1e-9 & b >= -1e-9 & c >= -1e-9
  P <- matrix(0, nrow(g), length(m$vars)); P[, idx] <- cbind(pmax(a, 0), pmax(b, 0), pmax(c, 0)) * rem
  if (length(others)) P[, others] <- matrix(fixed, nrow(g), length(others), byrow = TRUE)
  colnames(P) <- m$vars
  z <- stats::predict(m$fit, as.data.frame(P)); z[!inside] <- NA
  d <- data.frame(x = g$x, y = g$y, z = z)
  tri <- data.frame(x = c(0, 1, 0.5, 0), y = c(0, 0, sqrt(3) / 2, 0))
  pts <- m$data[, m$vars[idx]]; s <- rowSums(pts); keep <- s > 0
  pts <- pts[keep, ] / s[keep]
  obs <- data.frame(x = pts[, 2] + pts[, 1] / 2, y = pts[, 1] * sqrt(3) / 2)
  ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_raster(ggplot2::aes(fill = z)) +
    ggplot2::geom_contour(ggplot2::aes(z = z), colour = "white", bins = 10, linewidth = 0.3, na.rm = TRUE) +
    ggplot2::scale_fill_viridis_c(name = m$response, na.value = "transparent") +
    ggplot2::geom_path(data = tri, colour = "grey20") +
    ggplot2::geom_point(data = obs, shape = 21, fill = "white", size = 2.2) +
    ggplot2::annotate("text", x = c(0.5, -0.03, 1.03), y = c(sqrt(3) / 2 + 0.04, -0.04, -0.04), label = comps[c(1, 3, 2)],
                      hjust = c(0.5, 1, 0), fontface = "bold") +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::labs(title = sprintf("Mixture contour plot of %s", m$response),
                  subtitle = if (length(others)) sprintf("Other components fixed: %s",
                    paste(sprintf("%s = %.3g", m$components[others], fixed), collapse = ", ")) else NULL) +
    ggplot2::theme_void(base_size = 13) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), plot.margin = ggplot2::margin(10, 30, 20, 30))
}

# Cox-direction trace plot: each component varied from the centroid while
# the others keep their relative proportions.
mixture_trace <- function(m) {
  q <- length(m$vars); ref <- rep(1 / q, q)
  d <- do.call(rbind, lapply(seq_len(q), function(i) {
    xi <- seq(0, 1, length.out = 101)
    P <- t(vapply(xi, function(v) { r <- ref * (1 - v) / (1 - ref[i]); r[i] <- v; r }, numeric(q)))
    colnames(P) <- m$vars
    data.frame(Component = m$components[i], dev = xi - ref[i], y = stats::predict(m$fit, as.data.frame(P)))
  }))
  ggplot2::ggplot(d, ggplot2::aes(dev, y, colour = Component)) + ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(title = "Response trace plot (Cox direction)", x = "Deviation from the centroid proportion",
                  y = m$response) +
    ggplot2::theme_minimal(base_size = 13) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}
