# General linear model ANOVA with crossed/nested, fixed/random factors and
# covariates.
#   * Adjusted (Type III) sums of squares with sum-to-zero coding.
#   * Balanced designs with random factors: expected mean squares (unrestricted
#     model, as in Minitab), F-tests against the matching denominator, or a
#     Satterthwaite synthesized denominator, and ANOVA variance components.
#   * Unbalanced designs with random factors: REML variance components (nlme).

# Terms are character vectors of factor names; nesting is encoded by the term
# containing its parent factors. Labels: "A", "A*B", "B(A)".
glm_terms <- function(factors, nested = list(), order = 2) {
  crossed <- setdiff(factors, names(nested))
  terms <- list()
  for (k in seq_len(min(order, length(crossed)))) {
    for (cmb in utils::combn(crossed, k, simplify = FALSE)) terms[[length(terms) + 1]] <- list(f = cmb, label = paste(cmb, collapse = "*"))
  }
  for (b in names(nested)) {
    within <- nested[[b]]
    parents <- unique(c(within, unlist(lapply(within, function(w) nested[[w]]))))
    terms[[length(terms) + 1]] <- list(f = c(parents, b), label = sprintf("%s(%s)", b, paste(within, collapse = " ")))
  }
  terms
}

glm_anova <- function(df, response, factors, covariates = character(0), random = character(0),
                      nested = list(), order = 2) {
  if (!length(factors)) stop("Choose at least one factor.")
  if (response %in% c(factors, covariates)) stop("The response cannot also be a factor or covariate.")
  if (!all(names(nested) %in% factors) || !all(unlist(nested) %in% factors)) stop("Nested factors must be among the chosen factors.")
  cols <- c(response, factors, covariates)
  d <- df[stats::complete.cases(df[, cols, drop = FALSE]), cols, drop = FALSE]
  if (!is.numeric(d[[response]])) stop("The response must be numeric.")
  for (f in factors) d[[f]] <- factor(d[[f]])
  for (cv in covariates) if (!is.numeric(d[[cv]])) stop("Covariate '", cv, "' must be numeric.")
  vn <- stats::setNames(make.names(c(factors, covariates), unique = TRUE), c(factors, covariates))
  dd <- d; names(dd) <- c(".y", vn)
  # Number nested factors within their parents (1, 2, ... in each parent), so
  # 'A + A:B' codes B within A whether or not B's labels repeat across parents.
  orig_levels <- lapply(dd, function(v) if (is.factor(v)) nlevels(v) else NA)
  for (b in names(nested)) {
    par <- do.call(paste, c(lapply(nested[[b]], function(w) dd[[vn[w]]]), sep = "\r"))
    idx <- stats::ave(as.integer(dd[[vn[b]]]), par, FUN = function(v) match(v, sort(unique(v))))
    dd[[vn[b]]] <- factor(idx)
  }
  terms <- glm_terms(factors, nested, order)
  tl <- vapply(terms, function(t) paste(vn[t$f], collapse = ":"), "")
  rhs <- c(tl, vn[covariates])
  op <- options(contrasts = c("contr.sum", "contr.poly")); on.exit(options(op))
  f <- stats::as.formula(paste(".y ~", paste(rhs, collapse = " + ")))
  # keep.order: R would otherwise move covariates ahead of interactions and
  # the term labels would no longer match the columns.
  f <- stats::terms(f, keep.order = TRUE)
  X <- stats::model.matrix(f, dd)
  asg <- attr(X, "assign"); labs <- c(vapply(terms, `[[`, "", "label"), covariates)
  nz <- colSums(abs(X)) > 0   # unequal numbers of nested levels leave empty columns
  X <- X[, nz, drop = FALSE]; asg <- asg[nz]
  fit <- stats::lm.fit(X, dd$.y)
  if (any(is.na(fit$coefficients))) stop("Some terms cannot be estimated with these data (empty cells or confounded factors).")
  sse <- sum(fit$residuals^2); dfe <- fit$df.residual
  if (dfe < 1) stop("No degrees of freedom are left for error: add replicates or reduce the model.")
  # Adjusted (Type III) SS: drop each term's columns from the full model.
  ss <- vapply(seq_along(labs), function(i) {
    keep <- asg != i
    sum(stats::lm.fit(X[, keep, drop = FALSE], dd$.y)$residuals^2) - sse
  }, numeric(1))
  dft <- vapply(seq_along(labs), function(i) sum(asg == i), integer(1))
  ms <- ss / dft; mse <- sse / dfe
  is_random <- vapply(seq_along(terms), function(i) any(terms[[i]]$f %in% random), logical(1))
  is_random <- c(is_random, rep(FALSE, length(covariates)))
  cells <- table(do.call(paste, c(lapply(factors, function(f) dd[[vn[f]]]), sep = "\r")))
  balanced <- length(unique(as.vector(cells))) == 1 && length(cells) == prod(vapply(factors, function(f) nlevels(dd[[vn[f]]]), integer(1))) / nested_shrink(factors, nested, dd, vn) && !length(covariates)

  tab <- data.frame(Source = labs, Type = ifelse(is_random, "Random", ifelse(seq_along(labs) > length(terms), "Covariate", "Fixed")),
                    DF = dft, `Adj SS` = ss, `Adj MS` = ms, F = ms / mse,
                    `p-value` = stats::pf(ms / mse, dft, dfe, lower.tail = FALSE), Denominator = "Error", check.names = FALSE)
  ems_tab <- NULL; vc <- NULL; note <- NULL
  if (any(is_random)) {
    if (balanced) {
      e <- glm_ems(terms, factors, random, dd, vn, ms, dft, mse, dfe)
      tab$F <- e$F; tab$`p-value` <- e$p; tab$Denominator <- e$denom; tab$`Denominator DF` <- e$df_den
      ems_tab <- e$ems; vc <- e$vc
    } else {
      vc <- reml_components(dd, terms[is_random[seq_along(terms)]], vn, fixed_terms = rhs[!is_random])
      note <- "Unbalanced data with random factors: variance components are REML estimates; F-tests use the error mean square and are approximate."
    }
  }
  extra <- tab[c(1, 1), , drop = FALSE]
  extra$Source <- c("Error", "Total"); extra$Type <- ""; extra$DF <- c(dfe, nrow(dd) - 1)
  extra$`Adj SS` <- c(sse, sum((dd$.y - mean(dd$.y))^2)); extra$`Adj MS` <- c(mse, NA)
  extra$F <- NA; extra$`p-value` <- NA; extra$Denominator <- ""
  if ("Denominator DF" %in% names(extra)) extra$`Denominator DF` <- NA
  tab <- rbind(tab, extra); rownames(tab) <- NULL
  r2 <- 1 - sse / sum((dd$.y - mean(dd$.y))^2)
  list(table = tab, ems = ems_tab, vc = vc, note = note, balanced = balanced, data = d, dd = dd, vn = vn, fit_formula = f,
       factors = factors, random = random, response = response, terms = terms,
       summary = data.frame(S = sqrt(mse), `R-sq (%)` = 100 * r2,
                            `R-sq (adj) (%)` = 100 * (1 - (sse / dfe) / (sum((dd$.y - mean(dd$.y))^2) / (nrow(dd) - 1))),
                            Observations = nrow(dd), Balanced = if (balanced) "Yes" else "No", check.names = FALSE),
       lmfit = { op2 <- options(contrasts = c("contr.sum", "contr.poly")); on.exit(options(op2), add = TRUE); stats::lm(f, data = dd) })
}

# Number of non-existent cells implied by nesting (nested levels are unique
# within their parent, so the full cross is not expected).
nested_shrink <- function(factors, nested, dd, vn) {
  if (!length(nested)) return(1)
  full <- prod(vapply(factors, function(f) nlevels(dd[[vn[f]]]), integer(1)))
  present <- nrow(unique(dd[, vn[factors], drop = FALSE]))
  full / present
}

# Expected mean squares for balanced data (unrestricted mixed model):
# a random term U contributes c_U * sigma_U^2 to the EMS of term T when U
# contains every factor of T; c_U = observations per level combination of U.
glm_ems <- function(terms, factors, random, dd, vn, ms, dft, mse, dfe) {
  N <- nrow(dd); k <- length(terms)
  rnd <- vapply(terms, function(t) any(t$f %in% random), logical(1))
  cells <- function(t) nrow(unique(dd[, vn[t$f], drop = FALSE]))
  coef_c <- vapply(terms, function(t) N / cells(t), numeric(1))
  # Component matrix: rows = mean squares (terms + error), cols = random components (+ error).
  rcols <- which(rnd)
  M <- matrix(0, k + 1, length(rcols) + 1)
  for (i in seq_len(k)) for (jj in seq_along(rcols)) {
    j <- rcols[jj]
    if (all(terms[[i]]$f %in% terms[[j]]$f)) M[i, jj] <- coef_c[j]
  }
  M[, length(rcols) + 1] <- 1
  ems_text <- vapply(seq_len(k + 1), function(i) {
    parts <- c(if (i <= k && !rnd[i]) sprintf("Q[%s]", terms[[i]]$label),
               vapply(seq_along(rcols), function(jj) if (M[i, jj] > 0) sprintf("%g (%s)", M[i, jj], terms[[rcols[jj]]]$label) else "", ""),
               "(Error)")
    paste(parts[nzchar(parts)], collapse = " + ")
  }, "")
  allms <- c(ms[seq_len(k)], mse); alldf <- c(dft[seq_len(k)], dfe)
  Fv <- numeric(k); pv <- numeric(k); den <- character(k); dfd <- numeric(k)
  for (i in seq_len(k)) {
    target <- M[i, ]
    if (rnd[i]) target[match(i, rcols)] <- 0
    # Find a combination of other mean squares with the same components.
    w <- tryCatch(qr.solve(t(M[-i, , drop = FALSE]), target), error = function(e) NULL)
    others <- setdiff(seq_len(k + 1), i)
    if (is.null(w) || max(abs(t(M[-i, , drop = FALSE]) %*% w - target)) > 1e-8) {
      Fv[i] <- NA; pv[i] <- NA; den[i] <- "no exact test"; dfd[i] <- NA; next
    }
    w[abs(w) < 1e-10] <- 0
    dms <- sum(w * allms[others])
    used <- which(w != 0)
    df_den <- dms^2 / sum((w[used] * allms[others][used])^2 / alldf[others][used])
    Fv[i] <- ms[i] / dms; dfd[i] <- df_den
    pv[i] <- stats::pf(Fv[i], dft[i], df_den, lower.tail = FALSE)
    lab <- c(vapply(terms, `[[`, "", "label"), "Error")[others][used]
    den[i] <- if (length(used) == 1 && abs(w[used] - 1) < 1e-8) lab else paste(sprintf("%+g %s", w[used], lab), collapse = " ")
  }
  # Variance components by the ANOVA (method of moments) estimator.
  sol <- tryCatch(qr.solve(M[c(rcols, k + 1), , drop = FALSE], allms[c(rcols, k + 1)]), error = function(e) rep(NA, length(rcols) + 1))
  comp <- data.frame(Source = c(vapply(terms[rcols], `[[`, "", "label"), "Error"), `Variance component` = sol, check.names = FALSE)
  pos <- pmax(comp$`Variance component`, 0)
  comp$`% of total` <- 100 * pos / sum(pos)
  comp$`Std. dev.` <- sqrt(pos)
  list(F = Fv, p = pv, denom = den, df_den = dfd,
       ems = data.frame(Source = c(vapply(terms, `[[`, "", "label"), "Error"), `Expected mean square` = ems_text, check.names = FALSE),
       vc = comp)
}

# REML variance components with nlme: each random term becomes an identity
# block for the factor coding its level combinations (crossed or nested).
reml_components <- function(dd, rterms, vn, fixed_terms = character(0)) {
  if (!length(rterms)) return(NULL)
  dd$.all <- factor(1)
  rnames <- character(0)
  for (t in rterms) {
    nm <- paste0(".r", length(rnames) + 1)
    dd[[nm]] <- factor(do.call(paste, c(lapply(t$f, function(f) dd[[vn[f]]]), sep = ":")))
    rnames <- c(rnames, nm)
  }
  fx <- stats::as.formula(paste(".y ~", if (length(fixed_terms)) paste(fixed_terms, collapse = " + ") else "1"))
  blocks <- lapply(rnames, function(nm) nlme::pdIdent(stats::as.formula(paste("~", nm, "- 1"))))
  fit <- tryCatch(nlme::lme(fx, data = dd, random = list(.all = nlme::pdBlocked(blocks)), method = "REML",
                            control = nlme::lmeControl(returnObject = TRUE, msMaxIter = 200)),
                  error = function(e) stop("The REML fit failed: ", conditionMessage(e)))
  # Extract each block's variance from VarCorr (rows are grouped by block).
  vc <- suppressWarnings(as.numeric(nlme::VarCorr(fit)[, "Variance"]))
  lens <- vapply(rnames, function(nm) nlevels(dd[[nm]]), integer(1))
  starts <- cumsum(c(1, utils::head(lens, -1)))
  comp <- vapply(starts, function(s) vc[s], numeric(1))
  out <- data.frame(Source = c(vapply(rterms, `[[`, "", "label"), "Error"),
                    `Variance component` = c(comp, fit$sigma^2), check.names = FALSE)
  out$`% of total` <- 100 * out$`Variance component` / sum(out$`Variance component`)
  out$`Std. dev.` <- sqrt(out$`Variance component`)
  out
}

glm_effects_plot <- function(g) {
  d <- g$dd; vn <- g$vn
  m <- do.call(rbind, lapply(g$factors, function(f) {
    a <- stats::aggregate(d$.y, list(Level = d[[vn[f]]]), mean)
    data.frame(Factor = f, Level = as.character(a$Level), Mean = a$x)
  }))
  m$Factor <- factor(m$Factor, levels = g$factors)
  ggplot2::ggplot(m, ggplot2::aes(Level, Mean, group = 1)) +
    ggplot2::geom_hline(yintercept = mean(d$.y), linetype = 3, colour = "grey50") +
    ggplot2::geom_line(colour = "#4a6fa5", linewidth = 1) + ggplot2::geom_point(colour = "#4a6fa5", size = 2.5) +
    ggplot2::facet_wrap(~Factor, scales = "free_x", nrow = 1) +
    ggplot2::labs(title = sprintf("Main effects plot for %s", g$response), x = NULL, y = "Mean") +
    ggplot2::theme_bw(base_size = 12) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

glm_interaction_plot <- function(g) {
  if (length(g$factors) < 2) return(NULL)
  d <- g$dd; vn <- g$vn; f1 <- g$factors[1]; f2 <- g$factors[2]
  a <- stats::aggregate(list(Mean = d$.y), list(X = d[[vn[f1]]], Line = d[[vn[f2]]]), mean)
  ggplot2::ggplot(a, ggplot2::aes(X, Mean, group = Line, colour = Line)) +
    ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point(size = 2.5) +
    ggplot2::labs(title = sprintf("Interaction plot: %s by %s", f1, f2), x = f1, y = sprintf("Mean of %s", g$response), colour = f2) +
    ggplot2::theme_bw(base_size = 12) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}
