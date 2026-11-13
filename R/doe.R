# Two-level factorial designs: creation, alias structure and analysis.

# Standard minimum-aberration generators (Box, Hunter & Hunter; Montgomery).
# Keyed by "<factors>-<runs>". Full factorials need no generators.
doe_generators <- list(
  "3-4"  = c(C = "AB"),
  "4-8"  = c(D = "ABC"),
  "5-8"  = c(D = "AB", E = "AC"),
  "5-16" = c(E = "ABCD"),
  "6-8"  = c(D = "AB", E = "AC", F = "BC"),
  "6-16" = c(E = "ABC", F = "BCD"),
  "6-32" = c(F = "ABCDE"),
  "7-8"  = c(D = "AB", E = "AC", F = "BC", G = "ABC"),
  "7-16" = c(E = "ABC", F = "BCD", G = "ACD"),
  "7-32" = c(F = "ABCD", G = "ABDE"),
  "7-64" = c(G = "ABCDEF"),
  "8-16" = c(E = "BCD", F = "ACD", G = "ABC", H = "ABD"),
  "8-32" = c(F = "ABC", G = "ABD", H = "BCDE"),
  "8-64" = c(G = "ABCD", H = "ABEF")
)

DOE_MAX_FACTORS <- 23   # fractional factorials up to 8 factors; Plackett-Burman up to 23

is_pb <- function(k, runs) runs %in% c(12, 20, 24) && runs != 2^k

# Multiply two effects (letters cancel in pairs: A*A = I).
word_mult <- function(a, b) {
  s <- setdiff(union(strsplit(a, "")[[1]], strsplit(b, "")[[1]]),
               intersect(strsplit(a, "")[[1]], strsplit(b, "")[[1]]))
  paste(sort(s), collapse = "")
}

defining_relation <- function(gens) {
  if (!length(gens)) return(character(0))
  words <- paste0(gens, names(gens))
  words <- vapply(words, function(w) paste(sort(strsplit(w, "")[[1]]), collapse = ""), "")
  group <- character(0)
  for (w in words) group <- unique(c(group, w, vapply(group, word_mult, "", b = w)))
  group <- group[nzchar(group)]
  group[order(nchar(group), group)]
}

# Available designs for k factors: full factorial plus listed fractions.
doe_options <- function(k) {
  if (k < 2 || k > DOE_MAX_FACTORS) stop("Choose between 2 and ", DOE_MAX_FACTORS, " factors.")
  full <- 2^k
  fr <- names(doe_generators)[startsWith(names(doe_generators), paste0(k, "-"))]
  pb <- c(12, 20, 24)[c(12, 20, 24) > k & c(12, 20, 24) < full]
  runs <- c(as.integer(sub("^\\d+-", "", fr)), if (full <= 128) full, pb)
  runs <- sort(unique(runs))
  res <- vapply(runs, function(r) {
    if (r == full) return("Full factorial")
    if (is_pb(k, r)) return("Plackett-Burman screening (main effects)")
    w <- defining_relation(doe_generators[[paste0(k, "-", r)]])
    sprintf("1/%d fraction, resolution %s", full / r, as.roman(min(nchar(w))))
  }, "")
  stats::setNames(runs, sprintf("%d runs - %s", runs, res))
}

# Coded design matrix in standard (Yates) order.
doe_coded <- function(k, runs) {
  if (is_pb(k, runs)) {
    m <- pb_matrix(runs)[, seq_len(k), drop = FALSE]
    colnames(m) <- LETTERS[seq_len(k)]
    return(list(matrix = m, generators = character(0), pb = TRUE))
  }
  gens <- if (runs == 2^k) character(0) else doe_generators[[paste0(k, "-", runs)]]
  if (runs != 2^k && is.null(gens)) stop("No standard design with ", runs, " runs for ", k, " factors.")
  base <- k - length(gens)
  m <- as.matrix(expand.grid(rep(list(c(-1, 1)), base)))
  colnames(m) <- LETTERS[seq_len(base)]
  for (g in names(gens)) {
    m <- cbind(m, apply(m[, strsplit(gens[[g]], "")[[1]], drop = FALSE], 1, prod))
    colnames(m)[ncol(m)] <- g
  }
  list(matrix = m, generators = gens)
}

# Create a design worksheet.
#   factors  data.frame(name, low, high)
doe_create <- function(factors, runs, replicates = 1, center = 0, randomize = TRUE, seed = NA,
                       response = "Response") {
  k <- nrow(factors)
  if (k < 2 || k > DOE_MAX_FACTORS) stop("Choose between 2 and ", DOE_MAX_FACTORS, " factors.")
  nm <- trimws(factors$name)
  if (any(!nzchar(nm))) stop("Every factor needs a name.")
  if (anyDuplicated(nm)) stop("Factor names must be different.")
  if (response %in% nm || !nzchar(trimws(response))) stop("Choose a response name different from the factor names.")
  num <- is.numeric(factors$low) && is.numeric(factors$high)
  if (num && any(factors$low >= factors$high)) stop("Each low level must be below its high level.")
  if (replicates < 1 || center < 0) stop("Replicates must be at least 1 and center points cannot be negative.")
  if (center > 0 && !num) stop("Center points need numeric factor levels.")

  coded <- doe_coded(k, runs)$matrix
  rows <- do.call(rbind, rep(list(coded), replicates))
  ctpt <- rep(1L, nrow(rows))
  if (center > 0) {
    rows <- rbind(rows, matrix(0, center, k))
    ctpt <- c(ctpt, rep(0L, center))
  }
  std <- seq_len(nrow(rows))
  if (!is.na(seed)) set.seed(seed)
  run <- if (randomize) sample(std) else std
  out <- data.frame(StdOrder = std, RunOrder = run, CenterPt = ctpt)
  for (j in seq_len(k)) {
    lo <- factors$low[j]; hi <- factors$high[j]
    out[[nm[j]]] <- if (num) ifelse(rows[, j] < 0, lo, ifelse(rows[, j] > 0, hi, (lo + hi) / 2))
                    else ifelse(rows[, j] < 0, as.character(lo), as.character(hi))
  }
  out[[trimws(response)]] <- NA_real_
  out <- out[order(out$RunOrder), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Alias structure for main effects and two-factor interactions.
doe_aliases <- function(k, runs, names = LETTERS[seq_len(k)], max_order = 3) {
  if (is_pb(k, runs)) {
    return(list(defining = "Plackett-Burman design: no defining relation (non-regular design)",
                resolution = "III",
                table = data.frame(Effect = names,
                                   `Aliased with (up to 3-factor terms)` = "Partially aliased with every two-factor interaction that does not involve it",
                                   check.names = FALSE)))
  }
  gens <- if (runs == 2^k) character(0) else doe_generators[[paste0(k, "-", runs)]]
  dr <- defining_relation(gens)
  lab <- function(w) paste(names[match(strsplit(w, "")[[1]], LETTERS)], collapse = "*")
  effects <- c(LETTERS[seq_len(k)], if (k > 1) apply(utils::combn(LETTERS[seq_len(k)], 2), 2, paste, collapse = ""))
  alias <- vapply(effects, function(e) {
    a <- vapply(dr, word_mult, "", a = e)
    a <- a[nchar(a) <= max_order & nchar(a) > 0]
    if (!length(a)) "" else paste(vapply(a[order(nchar(a), a)], lab, ""), collapse = " + ")
  }, "")
  list(
    defining = if (length(dr)) paste("I =", paste(dr, collapse = " = ")) else "Full factorial: no aliasing",
    resolution = if (length(dr)) as.character(as.roman(min(nchar(dr)))) else "Full",
    table = data.frame(Effect = vapply(effects, lab, ""), `Aliased with (up to 3-factor terms)` = alias,
                       check.names = FALSE, row.names = NULL)
  )
}

# ---- Analysis --------------------------------------------------------------

# Code a factor column to -1 / 0 / +1.
doe_code_column <- function(v, name) {
  if (is.factor(v)) v <- as.character(v)
  if (is.character(v)) {
    u <- unique(v[!is.na(v)])
    sym <- c("-" = -1, "+" = 1, "-1" = -1, "1" = 1, "+1" = 1, "low" = -1, "high" = 1, "0" = 0)
    if (all(tolower(u) %in% names(sym))) {
      return(list(x = unname(sym[tolower(v)]), low = "-", high = "+"))
    }
    if (length(u) != 2) stop("Factor '", name, "' must have exactly two levels (it has ", length(u), ").")
    lv <- sort(u)
    return(list(x = ifelse(v == lv[1], -1, 1), low = lv[1], high = lv[2]))
  }
  u <- sort(unique(v[!is.na(v)]))
  if (length(u) == 2) return(list(x = ifelse(v == u[1], -1, 1), low = u[1], high = u[2]))
  if (length(u) == 3 && isTRUE(all.equal(u[2], mean(u[c(1, 3)])))) {
    return(list(x = ifelse(v == u[1], -1, ifelse(v == u[3], 1, 0)), low = u[1], high = u[3]))
  }
  stop("Factor '", name, "' must have two levels (plus an optional center value); it has ", length(u), ".")
}

# Lenth's pseudo standard error for unreplicated designs.
lenth_pse <- function(effects) {
  a <- abs(effects)
  s0 <- 1.5 * stats::median(a)
  1.5 * stats::median(a[a < 2.5 * s0])
}

doe_analyze <- function(df, response, factors, order = 2, alpha = 0.05) {
  if (length(factors) < 2) stop("Choose at least two factors.")
  if (response %in% factors) stop("The response cannot also be a factor.")
  y <- df[[response]]
  if (!is.numeric(y)) stop("The response must be numeric.")
  keep <- !is.na(y) & stats::complete.cases(df[, factors, drop = FALSE])
  if (sum(keep) < 4) stop("Enter response values for at least four runs.")
  df <- df[keep, , drop = FALSE]; y <- y[keep]
  k <- length(factors)
  letters_k <- LETTERS[seq_len(k)]
  codes <- lapply(factors, function(f) doe_code_column(df[[f]], f))
  X <- as.data.frame(stats::setNames(lapply(codes, `[[`, "x"), letters_k))
  is_ct <- rowSums(X == 0) > 0
  if (any(is_ct & rowSums(X == 0) != k)) stop("Center points must be at the center of every factor.")
  d <- cbind(X, y = y)
  order <- max(1, min(order, k))
  form <- paste0("y ~ (", paste(letters_k, collapse = " + "), ")", if (order > 1) paste0("^", order) else "")
  if (any(is_ct)) { d$Ct <- as.numeric(is_ct); form <- paste(form, "+ Ct") }
  fit <- stats::lm(stats::as.formula(form), data = d)
  cf <- stats::coef(fit)
  aliased <- names(cf)[is.na(cf)]
  if (length(aliased)) {
    keep_terms <- setdiff(names(cf)[!is.na(cf)], "(Intercept)")
    fit <- stats::lm(stats::as.formula(paste("y ~", paste(keep_terms, collapse = " + "))), data = d)
    cf <- stats::coef(fit)
  }
  label <- function(t) {
    if (t == "Ct") return("Ct Pt (curvature)")
    paste(factors[match(strsplit(t, ":", fixed = TRUE)[[1]], letters_k)], collapse = "*")
  }
  terms <- setdiff(names(cf), "(Intercept)")
  fac_terms <- setdiff(terms, "Ct")
  dfe <- stats::df.residual(fit)
  effect <- 2 * cf[fac_terms]
  if (dfe > 0) {
    s <- summary(fit)$coefficients
    tab <- data.frame(Term = vapply(terms, label, ""), Code = gsub(":", "", terms),
                      Effect = ifelse(terms == "Ct", NA, 2 * s[terms, 1]), Coef = s[terms, 1],
                      `SE coef` = s[terms, 2], T = s[terms, 3], `p-value` = s[terms, 4],
                      check.names = FALSE, row.names = NULL)
    crit <- stats::qt(1 - alpha / 2, dfe)
    method <- sprintf("t-tests with %d error degrees of freedom", dfe)
  } else {
    if (length(fac_terms) < 3) stop("Too few terms to estimate significance; add replicates or center points.")
    pse <- lenth_pse(effect)
    dfl <- length(effect) / 3
    tval <- effect / pse
    tab <- data.frame(Term = vapply(fac_terms, label, ""), Code = gsub(":", "", fac_terms),
                      Effect = effect, Coef = cf[fac_terms], `SE coef` = NA, T = tval,
                      `p-value` = 2 * stats::pt(abs(tval), dfl, lower.tail = FALSE),
                      check.names = FALSE, row.names = NULL)
    crit <- stats::qt(1 - alpha / 2, dfl)
    method <- sprintf("Lenth's method (PSE = %.4g); the design has no error degrees of freedom", pse)
  }
  tab$Significant <- ifelse(!is.na(tab$`p-value`) & tab$`p-value` < alpha, "Yes", "No")
  s <- summary(fit)
  list(
    fit = fit, table = tab, crit = crit, method = method, alpha = alpha,
    aliased = vapply(aliased, label, ""),
    summary = data.frame(S = if (dfe > 0) s$sigma else NA, `R-sq (%)` = 100 * s$r.squared,
                         `R-sq (adj) (%)` = if (dfe > 0) 100 * s$adj.r.squared else NA,
                         Runs = length(y), `Center points` = sum(is_ct), check.names = FALSE),
    intercept = unname(cf["(Intercept)"]),
    data = d, factors = factors, letters = letters_k, response = response,
    levels = data.frame(Factor = factors, Low = vapply(codes, function(c) as.character(c$low), ""),
                        High = vapply(codes, function(c) as.character(c$high), ""))
  )
}

doe_pareto_plot <- function(a) {
  t <- a$table[a$table$Code != "Ct", ]
  t <- t[order(abs(t$T)), ]
  t$Term <- factor(t$Term, levels = t$Term)
  ggplot2::ggplot(t, ggplot2::aes(abs(T), Term, fill = Significant)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_vline(xintercept = a$crit, linetype = 2, colour = "#c0392b") +
    ggplot2::annotate("text", x = a$crit, y = 0.6, label = sprintf(" %.3g", a$crit), hjust = 0, colour = "#c0392b") +
    ggplot2::scale_fill_manual(values = c(Yes = "#4a6fa5", No = "#b8c7da")) +
    ggplot2::labs(title = "Pareto chart of the standardized effects",
                  subtitle = sprintf("α = %g; dashed line = critical value", a$alpha),
                  x = "|Standardized effect|", y = NULL) +
    ggplot2::theme_minimal(base_size = 13) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

doe_normal_plot <- function(a) {
  t <- a$table[a$table$Code != "Ct", ]
  t <- t[order(t$T), ]
  t$q <- stats::qnorm(stats::ppoints(nrow(t)))
  ggplot2::ggplot(t, ggplot2::aes(T, q)) +
    ggplot2::geom_abline(slope = 1 / ref_sd(t$T[t$Significant == "No"], t$T), intercept = 0,
                         colour = "grey60", linetype = 2) +
    ggplot2::geom_point(ggplot2::aes(colour = Significant), size = 3) +
    ggplot2::geom_text(data = t[t$Significant == "Yes", ],
                       ggplot2::aes(label = Term, hjust = ifelse(T > 0, 1.08, -0.08)), vjust = -0.9, size = 3.6) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.12)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.12)) +
    ggplot2::scale_colour_manual(values = c(Yes = "#c0392b", No = "#4a6fa5")) +
    ggplot2::labs(title = "Normal plot of the standardized effects", x = "Standardized effect", y = "Normal score") +
    ggplot2::theme_minimal(base_size = 13) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

# Spread of the non-significant effects, for the reference line of the normal plot.
ref_sd <- function(x, fallback) stats::sd(if (length(x) >= 2) x else fallback)

doe_main_effects_plot <- function(a) {
  d <- a$data[if ("Ct" %in% names(a$data)) a$data$Ct == 0 else TRUE, ]
  grand <- mean(d$y)
  m <- do.call(rbind, lapply(seq_along(a$factors), function(i) {
    L <- a$letters[i]
    mm <- tapply(d$y, d[[L]], mean)
    data.frame(Factor = a$factors[i], Level = c("Low", "High"), x = c(-1, 1), Mean = as.numeric(mm[c("-1", "1")]),
               Lab = c(a$levels$Low[i], a$levels$High[i]))
  }))
  m$Factor <- factor(m$Factor, levels = a$factors)
  ggplot2::ggplot(m, ggplot2::aes(x, Mean)) +
    ggplot2::geom_hline(yintercept = grand, linetype = 3, colour = "grey50") +
    ggplot2::geom_line(colour = "#4a6fa5", linewidth = 1) + ggplot2::geom_point(colour = "#4a6fa5", size = 3) +
    ggplot2::geom_text(ggplot2::aes(label = Lab), vjust = -1, size = 3.3, colour = "grey30") +
    ggplot2::facet_wrap(~Factor, nrow = 1) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.08, 0.18))) +
    ggplot2::scale_x_continuous(breaks = c(-1, 1), labels = c("Low", "High"), expand = ggplot2::expansion(add = 0.4)) +
    ggplot2::labs(title = sprintf("Main effects plot for %s", a$response), x = NULL, y = "Mean") +
    ggplot2::theme_minimal(base_size = 13) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

doe_interaction_plot <- function(a) {
  d <- a$data[if ("Ct" %in% names(a$data)) a$data$Ct == 0 else TRUE, ]
  pairs <- utils::combn(seq_along(a$factors), 2)
  m <- do.call(rbind, lapply(seq_len(ncol(pairs)), function(j) {
    i1 <- pairs[1, j]; i2 <- pairs[2, j]
    mm <- stats::aggregate(list(Mean = d$y), list(x = d[[a$letters[i1]]], g = d[[a$letters[i2]]]), mean)
    data.frame(Panel = sprintf("%s * %s", a$factors[i1], a$factors[i2]), x = mm$x,
               Line = ifelse(mm$g < 0, "Second factor low", "Second factor high"), Mean = mm$Mean)
  }))
  m$Panel <- factor(m$Panel, levels = unique(m$Panel))
  ggplot2::ggplot(m, ggplot2::aes(x, Mean, colour = Line, group = Line)) +
    ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point(size = 2.5) +
    ggplot2::facet_wrap(~Panel) +
    ggplot2::scale_x_continuous(breaks = c(-1, 1), labels = c("Low", "High"), expand = ggplot2::expansion(add = 0.3)) +
    ggplot2::scale_colour_manual(values = c("Second factor low" = "#4a6fa5", "Second factor high" = "#c0392b")) +
    ggplot2::labs(title = sprintf("Interaction plot for %s", a$response),
                  subtitle = "Panel 'X * Z': X on the horizontal axis, one line per level of Z. Non-parallel lines suggest an interaction.",
                  x = NULL, y = "Mean", colour = NULL) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), legend.position = "top")
}

# Coded regression equation using the factor names.
doe_equation <- function(a) {
  t <- a$table
  parts <- sprintf("%s %s %s", ifelse(t$Coef < 0, "-", "+"), formatC(abs(t$Coef), digits = 4, format = "g"),
                   gsub("Ct Pt (curvature)", "CtPt", t$Term, fixed = TRUE))
  paste(a$response, "=", formatC(a$intercept, digits = 5, format = "g"), paste(parts, collapse = " "))
}
