# Logistic regression (binary and ordinal), stepwise and best subsets.

# ---- Binary logistic --------------------------------------------------------------

logit_fit <- function(df, y, x, event = NULL) {
  if (!length(x)) stop("Choose at least one predictor.")
  if (y %in% x) stop("The response cannot also be a predictor.")
  d <- df[, c(y, x), drop = FALSE]; d <- d[stats::complete.cases(d), , drop = FALSE]
  lv <- sort(unique(as.character(d[[y]])))
  if (length(lv) != 2) stop("A binary response needs exactly two different values (it has ", length(lv), ").")
  if (is.null(event) || !(event %in% lv)) event <- lv[2]
  d$.y <- as.integer(as.character(d[[y]]) == event)
  for (v in x) if (!is.numeric(d[[v]])) d[[v]] <- factor(d[[v]])
  f <- stats::as.formula(paste(".y ~", paste(bt(x), collapse = " + ")))
  fit <- suppressWarnings(stats::glm(f, family = stats::binomial(), data = d))
  if (any(is.na(stats::coef(fit)))) stop("Some terms cannot be estimated (predictors perfectly correlated).")
  sep <- any(stats::fitted(fit) < 1e-8 | stats::fitted(fit) > 1 - 1e-8)
  list(fit = fit, data = d, y = y, x = x, event = event, other = setdiff(lv, event), separation = sep)
}

logit_coefficients <- function(m, conf = 0.95) {
  s <- summary(m$fit)$coefficients; z <- stats::qnorm(1 - (1 - conf) / 2)
  data.frame(Term = gsub("`", "", rownames(s)), Coef = s[, 1], `SE coef` = s[, 2], Z = s[, 3], `p-value` = s[, 4],
             `Odds ratio` = c(NA, exp(s[-1, 1])), `OR CI lower` = c(NA, exp(s[-1, 1] - z * s[-1, 2])),
             `OR CI upper` = c(NA, exp(s[-1, 1] + z * s[-1, 2])), check.names = FALSE, row.names = NULL)
}

logit_deviance <- function(m) {
  fit <- m$fit
  d1 <- stats::drop1(fit, test = "Chisq")[-1, , drop = FALSE]
  overall <- data.frame(Source = "Regression", DF = fit$df.null - fit$df.residual, `Deviance (chi-square)` = fit$null.deviance - fit$deviance,
                        `p-value` = stats::pchisq(fit$null.deviance - fit$deviance, fit$df.null - fit$df.residual, lower.tail = FALSE),
                        check.names = FALSE)
  terms <- data.frame(Source = paste0("  ", gsub("`", "", rownames(d1))), DF = d1$Df, `Deviance (chi-square)` = d1$LRT,
                      `p-value` = d1$`Pr(>Chi)`, check.names = FALSE)
  rbind(overall, terms)
}

# Hosmer-Lemeshow test with g groups of fitted probabilities.
hosmer_lemeshow <- function(y, p, g = 10) {
  br <- unique(stats::quantile(p, seq(0, 1, length.out = g + 1)))
  grp <- cut(p, br, include.lowest = TRUE)
  obs <- tapply(y, grp, sum); expd <- tapply(p, grp, sum); n <- tapply(y, grp, length)
  chi <- sum((obs - expd)^2 / (expd * (1 - expd / n)), na.rm = TRUE)
  df <- length(levels(grp)) - 2
  list(chisq = chi, df = df, p = stats::pchisq(chi, df, lower.tail = FALSE))
}

logit_summary <- function(m) {
  fit <- m$fit; y <- m$data$.y; p <- stats::fitted(fit)
  hl <- hosmer_lemeshow(y, p)
  pos <- p[y == 1]; neg <- p[y == 0]
  auc <- mean(outer(pos, neg, ">") + 0.5 * outer(pos, neg, "=="))
  pred <- as.integer(p >= 0.5)
  list(
    fit = data.frame(Statistic = c("Observations", sprintf("Events (%s)", m$event), "Deviance R-sq (%)", "AIC",
                                   "Hosmer-Lemeshow chi-square", "Hosmer-Lemeshow p-value", "Area under ROC curve"),
                     Value = c(length(y), sum(y), 100 * (1 - fit$deviance / fit$null.deviance), stats::AIC(fit),
                               hl$chisq, hl$p, auc)),
    classification = data.frame(Observed = c(m$other, m$event), `Predicted: other` = c(sum(pred == 0 & y == 0), sum(pred == 0 & y == 1)),
                                `Predicted: event` = c(sum(pred == 1 & y == 0), sum(pred == 1 & y == 1)),
                                `Percent correct` = c(100 * mean(pred[y == 0] == 0), 100 * mean(pred[y == 1] == 1)), check.names = FALSE)
  )
}

logit_plot <- function(m) {
  num <- m$x[vapply(m$x, function(v) is.numeric(m$data[[v]]), logical(1))]
  if (length(num)) {
    v <- num[1]; d <- m$data
    grid <- d[rep(1, 200), , drop = FALSE]
    for (o in setdiff(m$x, v)) grid[[o]] <- if (is.numeric(d[[o]])) mean(d[[o]]) else factor(levels(d[[o]])[1], levels = levels(d[[o]]))
    grid[[v]] <- seq(min(d[[v]]), max(d[[v]]), length.out = 200)
    grid$p <- stats::predict(m$fit, grid, type = "response")
    ggplot2::ggplot() +
      ggplot2::geom_jitter(data = d, ggplot2::aes(.data[[v]], .y), height = 0.03, width = 0, alpha = 0.5, colour = "#4a6fa5") +
      ggplot2::geom_line(data = grid, ggplot2::aes(.data[[v]], p), colour = "#c0392b", linewidth = 1) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::labs(title = sprintf("Probability of '%s' vs %s", m$event, v),
                    subtitle = if (length(m$x) > 1) "Other predictors held at their mean (or first level)" else NULL,
                    x = v, y = "Probability of event") +
      ggplot2::theme_bw(base_size = 12) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  } else {
    d <- m$data; d$p <- stats::fitted(m$fit)
    ggplot2::ggplot(d, ggplot2::aes(.data[[m$x[1]]], p)) + ggplot2::stat_summary(fun = mean, geom = "col", fill = "#9bb7d4") +
      ggplot2::labs(title = sprintf("Probability of '%s' by %s", m$event, m$x[1]), y = "Probability of event") +
      ggplot2::theme_bw(base_size = 12)
  }
}

# ---- Ordinal logistic (proportional odds) -----------------------------------------

ordinal_fit <- function(df, y, x, levels_order = NULL) {
  if (!length(x)) stop("Choose at least one predictor.")
  d <- df[, c(y, x), drop = FALSE]; d <- d[stats::complete.cases(d), , drop = FALSE]
  lv <- if (!is.null(levels_order) && length(levels_order)) levels_order
        else if (is.numeric(d[[y]])) as.character(sort(unique(d[[y]]))) else sort(unique(as.character(d[[y]])))
  if (length(lv) < 3) stop("Ordinal regression needs a response with three or more ordered categories.")
  if (!all(as.character(d[[y]]) %in% lv)) stop("The level order must include every response value.")
  d$.y <- factor(as.character(d[[y]]), levels = lv, ordered = TRUE)
  for (v in x) if (!is.numeric(d[[v]])) d[[v]] <- factor(d[[v]])
  f <- stats::as.formula(paste(".y ~", paste(bt(x), collapse = " + ")))
  fit <- tryCatch(MASS::polr(f, data = d, Hess = TRUE, method = "logistic"),
                  error = function(e) stop("The ordinal model could not be fitted: ", conditionMessage(e)))
  null <- MASS::polr(.y ~ 1, data = d, Hess = TRUE, method = "logistic")
  lr <- 2 * (stats::logLik(fit) - stats::logLik(null))
  dfm <- length(stats::coef(fit))
  list(fit = fit, data = d, y = y, x = x, levels = lv,
       test = data.frame(Statistic = c("Observations", "Log-likelihood", "LR chi-square (all slopes = 0)", "DF", "p-value", "AIC"),
                         Value = c(nrow(d), as.numeric(stats::logLik(fit)), as.numeric(lr), dfm,
                                   stats::pchisq(as.numeric(lr), dfm, lower.tail = FALSE), stats::AIC(fit))))
}

ordinal_coefficients <- function(m, conf = 0.95) {
  s <- summary(m$fit)$coefficients; z <- stats::qnorm(1 - (1 - conf) / 2)
  p <- 2 * stats::pnorm(abs(s[, 3]), lower.tail = FALSE)
  slope <- rownames(s) %in% names(stats::coef(m$fit))
  data.frame(Term = ifelse(slope, gsub("`", "", rownames(s)), paste("Cut-point", rownames(s))),
             Coef = s[, 1], `SE coef` = s[, 2], Z = s[, 3], `p-value` = p,
             `Odds ratio` = ifelse(slope, exp(s[, 1]), NA),
             `OR CI lower` = ifelse(slope, exp(s[, 1] - z * s[, 2]), NA), `OR CI upper` = ifelse(slope, exp(s[, 1] + z * s[, 2]), NA),
             check.names = FALSE, row.names = NULL)
}

ordinal_plot <- function(m) {
  num <- m$x[vapply(m$x, function(v) is.numeric(m$data[[v]]), logical(1))]
  if (!length(num)) return(NULL)
  v <- num[1]; d <- m$data
  grid <- d[rep(1, 150), , drop = FALSE]
  for (o in setdiff(m$x, v)) grid[[o]] <- if (is.numeric(d[[o]])) mean(d[[o]]) else factor(levels(d[[o]])[1], levels = levels(d[[o]]))
  grid[[v]] <- seq(min(d[[v]]), max(d[[v]]), length.out = 150)
  pr <- stats::predict(m$fit, grid, type = "probs")
  long <- data.frame(x = rep(grid[[v]], ncol(pr)), p = as.vector(pr), Category = factor(rep(colnames(pr), each = nrow(pr)), levels = m$levels))
  ggplot2::ggplot(long, ggplot2::aes(x, p, fill = Category)) + ggplot2::geom_area(alpha = 0.85) +
    ggplot2::scale_y_continuous(labels = scales::percent) + ggplot2::scale_fill_viridis_d() +
    ggplot2::labs(title = sprintf("Predicted category probabilities vs %s", v), x = v, y = "Probability") +
    ggplot2::theme_bw(base_size = 12) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

# ---- Stepwise (p-value based, as in Minitab) --------------------------------------

stepwise_reg <- function(df, y, candidates, method = "both", alpha_enter = 0.15, alpha_remove = 0.15, force = character(0)) {
  if (!length(candidates)) stop("Choose candidate predictors.")
  if (method == "both" && alpha_enter > alpha_remove) stop("Alpha to enter must not exceed alpha to remove.")
  d <- df[, unique(c(y, candidates)), drop = FALSE]; d <- d[stats::complete.cases(d), , drop = FALSE]
  if (!is.numeric(d[[y]])) stop("The response must be numeric.")
  for (v in candidates) if (!is.numeric(d[[v]])) d[[v]] <- factor(d[[v]])
  form <- function(vars) stats::as.formula(paste(bt(y), "~", if (length(vars)) paste(bt(vars), collapse = " + ") else "1"))
  current <- if (method == "backward") candidates else force
  steps <- list(); k <- 0
  repeat {
    changed <- FALSE
    fit <- stats::lm(form(current), data = d)
    if (method %in% c("forward", "both")) {
      out <- setdiff(candidates, current)
      if (length(out)) {
        a <- stats::add1(fit, scope = stats::as.formula(paste("~ . +", paste(bt(out), collapse = " + "))), test = "F")[-1, , drop = FALSE]
        best <- which.min(a$`Pr(>F)`)
        if (length(best) && !is.na(a$`Pr(>F)`[best]) && a$`Pr(>F)`[best] <= alpha_enter) {
          v <- gsub("`", "", rownames(a)[best]); current <- c(current, v); changed <- TRUE; k <- k + 1
          steps[[k]] <- data.frame(Step = k, Action = "Enter", Variable = v, `p-value` = a$`Pr(>F)`[best], check.names = FALSE)
          fit <- stats::lm(form(current), data = d)
        }
      }
    }
    if (method %in% c("backward", "both") && length(setdiff(current, force))) {
      r <- stats::drop1(fit, test = "F")[-1, , drop = FALSE]
      r <- r[!(gsub("`", "", rownames(r)) %in% force), , drop = FALSE]
      worst <- which.max(r$`Pr(>F)`)
      if (length(worst) && !is.na(r$`Pr(>F)`[worst]) && r$`Pr(>F)`[worst] > alpha_remove) {
        v <- gsub("`", "", rownames(r)[worst]); current <- setdiff(current, v); changed <- TRUE; k <- k + 1
        steps[[k]] <- data.frame(Step = k, Action = "Remove", Variable = v, `p-value` = r$`Pr(>F)`[worst], check.names = FALSE)
      }
    }
    if (!changed || k > 4 * length(candidates)) break
  }
  final <- stats::lm(form(current), data = d)
  s <- summary(final)
  list(steps = if (length(steps)) do.call(rbind, steps) else data.frame(Result = "No variable met the entry criterion."),
       selected = current, fit = final,
       summary = data.frame(S = s$sigma, `R-sq (%)` = 100 * s$r.squared, `R-sq (adj) (%)` = 100 * s$adj.r.squared,
                            Predictors = length(current), check.names = FALSE))
}

# ---- Best subsets ------------------------------------------------------------------

best_subsets <- function(df, y, x, best = 2, max_size = length(x)) {
  if (length(x) < 2) stop("Choose at least two predictors.")
  if (length(x) > 15) stop("Best subsets is limited to 15 predictors (32,767 models).")
  d <- df[, c(y, x), drop = FALSE]; d <- d[stats::complete.cases(d), , drop = FALSE]
  if (!all(vapply(d, is.numeric, logical(1)))) stop("Best subsets needs numeric response and predictors.")
  Y <- d[[y]]; X <- as.matrix(d[, x, drop = FALSE]); n <- length(Y)
  full <- stats::lm.fit(cbind(1, X), Y); mse_full <- sum(full$residuals^2) / (n - ncol(X) - 1)
  sst <- sum((Y - mean(Y))^2)
  rows <- list()
  for (k in seq_len(min(max_size, length(x), n - 2))) {
    cmb <- utils::combn(length(x), k)
    stats_k <- t(vapply(seq_len(ncol(cmb)), function(j) {
      f <- stats::lm.fit(cbind(1, X[, cmb[, j], drop = FALSE]), Y)
      sse <- sum(f$residuals^2)
      c(sse, 1 - sse / sst, 1 - (sse / (n - k - 1)) / (sst / (n - 1)), sse / mse_full - (n - 2 * (k + 1)), sqrt(sse / (n - k - 1)))
    }, numeric(5)))
    o <- utils::head(order(stats_k[, 1]), best)
    for (j in o) {
      mark <- ifelse(seq_along(x) %in% cmb[, j], "X", "")
      rows[[length(rows) + 1]] <- data.frame(Vars = k, `R-sq (%)` = 100 * stats_k[j, 2], `R-sq (adj) (%)` = 100 * stats_k[j, 3],
                                             `Mallows Cp` = stats_k[j, 4], S = stats_k[j, 5], t(stats::setNames(mark, x)), check.names = FALSE)
    }
  }
  do.call(rbind, rows)
}
