# Response surface analysis: first- and second-order models in coded units,
# lack of fit, stationary point and canonical analysis, contour and surface
# plots, and the path of steepest ascent.

# Code each factor to [-1, 1] across the range of the data.
rsm_coding <- function(df, factors) {
  lapply(stats::setNames(factors, factors), function(f) {
    v <- df[[f]]
    if (!is.numeric(v)) stop("Factor '", f, "' must be numeric.")
    r <- range(v, na.rm = TRUE)
    if (diff(r) == 0) stop("Factor '", f, "' has only one value.")
    list(mid = mean(r), half = diff(r) / 2)
  })
}
rsm_code <- function(df, coding) {
  as.data.frame(lapply(stats::setNames(names(coding), make.names(names(coding))), function(f) {
    (df[[f]] - coding[[f]]$mid) / coding[[f]]$half
  }), check.names = FALSE)
}
rsm_uncode <- function(xc, coding) mapply(function(v, cd) cd$mid + cd$half * v, xc, coding)

rsm_formula <- function(vars, order) {
  lin <- paste(vars, collapse = " + ")
  terms <- switch(order,
    linear = lin,
    interaction = if (length(vars) > 1) sprintf("(%s)^2", lin) else lin,
    quadratic = paste(c(if (length(vars) > 1) sprintf("(%s)^2", lin) else lin,
                        sprintf("I(%s^2)", vars)), collapse = " + "),
    stop("Unknown model."))
  stats::as.formula(paste(".resp ~", terms))
}

rsm_fit <- function(df, response, factors, order = "quadratic") {
  if (length(factors) < 1) stop("Choose at least one factor.")
  if (response %in% factors) stop("The response cannot also be a factor.")
  y <- df[[response]]
  if (!is.numeric(y)) stop("The response must be numeric.")
  keep <- !is.na(y) & stats::complete.cases(df[, factors, drop = FALSE])
  df <- df[keep, , drop = FALSE]
  coding <- rsm_coding(df, factors)
  X <- rsm_code(df, coding)
  vars <- names(X)
  d <- cbind(X, .resp = df[[response]])
  fit <- stats::lm(rsm_formula(vars, order), data = d)
  p <- length(stats::coef(fit))
  if (nrow(d) <= p) stop("Not enough runs (", nrow(d), ") for the ", order, " model with ", p, " terms.")
  if (any(is.na(stats::coef(fit)))) stop("Some model terms cannot be estimated with this design (for a ",
                                          "quadratic model each factor needs at least three levels).")
  list(fit = fit, data = d, coding = coding, vars = vars, factors = factors, response = response, order = order)
}

# Lack-of-fit test: pure error from replicated factor settings.
rsm_anova <- function(m) {
  fit <- m$fit; d <- m$data
  y <- d$.resp; n <- length(y)
  sse <- sum(stats::residuals(fit)^2); dfe <- stats::df.residual(fit)
  sst <- sum((y - mean(y))^2); dfr <- n - 1 - dfe
  tl <- attr(stats::terms(fit), "term.labels")
  grp <- function(t) if (grepl("^I\\(", t)) "Square" else if (grepl(":", t)) "2-way interaction" else "Linear"
  ss_seq <- stats::anova(fit)
  ss_seq <- ss_seq[rownames(ss_seq) != "Residuals", , drop = FALSE]
  groups <- vapply(rownames(ss_seq), grp, "")
  g <- unique(groups)
  rows <- lapply(g, function(k) {
    s <- sum(ss_seq$`Sum Sq`[groups == k]); df <- sum(ss_seq$Df[groups == k])
    data.frame(Source = paste0("  ", k), DF = df, SS = s, MS = s / df, F = (s / df) / (sse / dfe),
               `p-value` = stats::pf((s / df) / (sse / dfe), df, dfe, lower.tail = FALSE), check.names = FALSE)
  })
  key <- do.call(paste, c(d[m$vars], sep = "\r"))
  pe_ss <- sum(tapply(y, key, function(v) sum((v - mean(v))^2)))
  pe_df <- n - length(unique(key))
  out <- rbind(
    data.frame(Source = "Regression", DF = dfr, SS = sst - sse, MS = (sst - sse) / dfr,
               F = ((sst - sse) / dfr) / (sse / dfe),
               `p-value` = stats::pf(((sst - sse) / dfr) / (sse / dfe), dfr, dfe, lower.tail = FALSE), check.names = FALSE),
    do.call(rbind, rows),
    data.frame(Source = "Residual error", DF = dfe, SS = sse, MS = sse / dfe, F = NA, `p-value` = NA, check.names = FALSE))
  if (pe_df > 0 && dfe - pe_df > 0) {
    lof_ss <- sse - pe_ss; lof_df <- dfe - pe_df
    fl <- (lof_ss / lof_df) / (pe_ss / pe_df)
    out <- rbind(out,
      data.frame(Source = c("  Lack of fit", "  Pure error"), DF = c(lof_df, pe_df), SS = c(lof_ss, pe_ss),
                 MS = c(lof_ss / lof_df, pe_ss / pe_df), F = c(fl, NA),
                 `p-value` = c(stats::pf(fl, lof_df, pe_df, lower.tail = FALSE), NA), check.names = FALSE))
  }
  rbind(out, data.frame(Source = "Total", DF = n - 1, SS = sst, MS = NA, F = NA, `p-value` = NA, check.names = FALSE))
}

rsm_coefficients <- function(m) {
  s <- summary(m$fit)$coefficients
  lab <- rownames(s)
  for (i in seq_along(m$vars)) lab <- gsub(m$vars[i], m$factors[i], lab, fixed = TRUE)
  lab <- gsub("I\\((.*)\\^2\\)", "\\1*\\1", lab)
  lab <- gsub(":", "*", lab, fixed = TRUE)
  data.frame(Term = lab, Coef = s[, 1], `SE coef` = s[, 2], T = s[, 3], `p-value` = s[, 4], check.names = FALSE)
}

rsm_summary <- function(m) {
  s <- summary(m$fit); h <- stats::hatvalues(m$fit); r <- stats::residuals(m$fit)
  press <- sum((r / (1 - h))^2); y <- m$data$.resp
  data.frame(S = s$sigma, `R-sq (%)` = 100 * s$r.squared, `R-sq (adj) (%)` = 100 * s$adj.r.squared,
             `R-sq (pred) (%)` = 100 * (1 - press / sum((y - mean(y))^2)), Runs = length(y), check.names = FALSE)
}

# b (linear) and B (quadratic, symmetric) in coded units.
rsm_bB <- function(m) {
  cf <- stats::coef(m$fit); k <- length(m$vars)
  b <- vapply(m$vars, function(v) cf[[v]], numeric(1))
  B <- matrix(0, k, k, dimnames = list(m$vars, m$vars))
  for (i in seq_len(k)) {
    sq <- sprintf("I(%s^2)", m$vars[i]); if (sq %in% names(cf)) B[i, i] <- cf[[sq]]
    for (j in seq_len(k)) if (i < j) {
      nm <- intersect(c(paste0(m$vars[i], ":", m$vars[j]), paste0(m$vars[j], ":", m$vars[i])), names(cf))
      if (length(nm)) B[i, j] <- B[j, i] <- cf[[nm]] / 2
    }
  }
  list(b0 = cf[["(Intercept)"]], b = b, B = B)
}

rsm_stationary <- function(m) {
  if (m$order != "quadratic") return(NULL)
  q <- rsm_bB(m)
  if (rcond(q$B) < 1e-10) return(list(note = "The quadratic terms are (nearly) singular: there is no single stationary point (ridge system)."))
  xs <- -0.5 * solve(q$B, q$b)
  ev <- eigen(q$B, symmetric = TRUE)
  nature <- if (all(ev$values < 0)) "Maximum" else if (all(ev$values > 0)) "Minimum" else "Saddle point"
  inside <- all(abs(xs) <= 1 + 1e-9)
  yhat <- q$b0 + sum(q$b * xs) + drop(t(xs) %*% q$B %*% xs)
  list(point = data.frame(Factor = m$factors, Coded = xs, Uncoded = rsm_uncode(xs, m$coding), row.names = NULL),
       predicted = yhat, nature = nature, inside = inside, eigenvalues = ev$values,
       note = sprintf("%s at the stationary point (predicted %s = %.5g)%s.", nature, m$response, yhat,
                      if (inside) "" else "; it lies outside the experimental region, so treat it with caution"))
}

# Path of steepest ascent (or descent) from the first-order part of the model.
rsm_steepest <- function(m, steps = 6, direction = "ascent", step_size = 1) {
  q <- rsm_bB(m)
  b <- if (direction == "ascent") q$b else -q$b
  if (all(b == 0)) stop("The linear coefficients are all zero.")
  dir <- b / max(abs(b)) * step_size
  pts <- t(vapply(0:steps, function(s) s * dir, numeric(length(b))))
  if (length(b) == 1) pts <- matrix(pts, ncol = 1)
  colnames(pts) <- m$vars
  pred <- stats::predict(m$fit, as.data.frame(pts))
  out <- data.frame(Step = 0:steps)
  for (i in seq_along(m$vars)) out[[m$factors[i]]] <- m$coding[[m$factors[i]]]$mid + m$coding[[m$factors[i]]]$half * pts[, i]
  out[[paste("Predicted", m$response)]] <- pred
  out
}

# Grid of predictions over two factors, others held at given coded values.
rsm_grid <- function(m, f1, f2, hold = NULL, n = 60) {
  i1 <- match(f1, m$factors); i2 <- match(f2, m$factors)
  if (is.na(i1) || is.na(i2)) stop("Choose plot axes from the model's factors.")
  g <- expand.grid(a = seq(-1, 1, length.out = n), b = seq(-1, 1, length.out = n))
  X <- as.data.frame(matrix(0, nrow(g), length(m$vars), dimnames = list(NULL, m$vars)))
  if (!is.null(hold)) for (v in intersect(names(hold), m$factors)) X[[make.names(v)]] <- hold[[v]]
  X[[m$vars[i1]]] <- g$a; X[[m$vars[i2]]] <- g$b
  data.frame(x = m$coding[[f1]]$mid + m$coding[[f1]]$half * g$a,
             y = m$coding[[f2]]$mid + m$coding[[f2]]$half * g$b,
             z = stats::predict(m$fit, X))
}

rsm_contour_plot <- function(m, f1, f2, hold = NULL, raw = NULL) {
  g <- rsm_grid(m, f1, f2, hold)
  p <- ggplot2::ggplot(g, ggplot2::aes(x, y, z = z)) +
    ggplot2::geom_contour_filled(bins = 10, alpha = 0.9) +
    ggplot2::geom_contour(bins = 10, colour = "white", linewidth = 0.3) +
    ggplot2::scale_fill_viridis_d(option = "D", name = m$response) +
    ggplot2::labs(title = sprintf("Contour plot of %s", m$response), x = f1, y = f2,
                  subtitle = if (length(m$factors) > 2) "Other factors held at the values chosen on the left" else NULL) +
    ggplot2::theme_minimal(base_size = 13) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  if (!is.null(raw)) p <- p + ggplot2::geom_point(data = raw, ggplot2::aes(x, y), inherit.aes = FALSE,
                                                  shape = 21, fill = "white", colour = "black", size = 2)
  p
}

rsm_surface_plot <- function(m, f1, f2, hold = NULL, theta = 35, phi = 25) {
  g <- rsm_grid(m, f1, f2, hold, n = 40)
  xs <- sort(unique(g$x)); ys <- sort(unique(g$y))
  z <- matrix(g$z, length(xs), length(ys))
  zf <- (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)]) / 4
  cols <- grDevices::hcl.colors(100, "viridis")[cut(zf, 100, labels = FALSE)]
  graphics::par(mar = c(1, 1, 3, 1))
  graphics::persp(xs, ys, z, theta = theta, phi = phi, col = cols, border = NA, shade = 0.25,
                  ticktype = "detailed", xlab = f1, ylab = f2, zlab = m$response,
                  main = sprintf("Surface plot of %s", m$response), expand = 0.7, cex.axis = 0.7)
  invisible(NULL)
}
