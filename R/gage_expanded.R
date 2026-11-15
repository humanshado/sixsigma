# Expanded Gage R&R: extra random factors (gauge, fixture, location...),
# optional two-way interactions, and unbalanced data, using REML variance
# components. Reproducibility = every component except part and error.

gage_expanded <- function(df, meas, part, operator, extra = character(0), interactions = TRUE,
                          tolerance = NA, k = 6) {
  factors <- c(part, operator, extra)
  if (anyDuplicated(c(meas, factors))) stop("Choose different columns for measurement, part, operator and extra factors.")
  d <- df[stats::complete.cases(df[, c(meas, factors), drop = FALSE]), c(meas, factors), drop = FALSE]
  if (!is.numeric(d[[meas]])) stop("The measurement must be numeric.")
  for (f in factors) d[[f]] <- factor(d[[f]])
  if (nlevels(d[[part]]) < 2) stop("At least two parts are needed.")
  vn <- stats::setNames(make.names(factors, unique = TRUE), factors)
  dd <- d; names(dd) <- c(".y", vn)
  terms <- lapply(factors, function(f) list(f = f, label = f))
  if (interactions) {
    others <- c(operator, extra)
    for (o in others) terms[[length(terms) + 1]] <- list(f = c(part, o), label = paste(part, o, sep = "*"))
    if (length(extra)) for (e in extra) terms[[length(terms) + 1]] <- list(f = c(operator, e), label = paste(operator, e, sep = "*"))
  }
  # Skip interaction terms that have no replication (each cell measured once).
  reps <- vapply(terms, function(t) nrow(dd) / nrow(unique(dd[, vn[t$f], drop = FALSE])), numeric(1))
  terms <- terms[!(vapply(terms, function(t) length(t$f) > 1, logical(1)) & reps < 1.5)]
  vc <- reml_components(dd, terms, vn)
  comp <- stats::setNames(pmax(vc$`Variance component`, 0), vc$Source)
  comp[comp < 1e-8 * sum(comp)] <- 0   # REML returns ~1e-11 for components that are zero
  repeatability <- comp[["Error"]]
  part_var <- comp[[part]]
  repro_terms <- setdiff(names(comp), c(part, "Error"))
  repro <- sum(comp[repro_terms])
  grr <- repeatability + repro
  total <- grr + part_var
  rows <- c("Total Gage R&R", "  Repeatability", "  Reproducibility", paste0("    ", repro_terms), "Part-to-part", "Total variation")
  vars <- c(grr, repeatability, repro, comp[repro_terms], part_var, total)
  tab <- data.frame(Source = rows, `Variance component` = vars, `% Contribution` = 100 * vars / total,
                    `Std. dev.` = sqrt(vars), `Study var (k x SD)` = k * sqrt(vars), `% Study var` = 100 * sqrt(vars) / sqrt(total),
                    check.names = FALSE)
  if (!is.na(tolerance)) tab$`% Tolerance` <- 100 * k * sqrt(vars) / tolerance
  ndc <- max(1, floor(sqrt(2) * sqrt(part_var) / sqrt(grr)))
  pct <- 100 * sqrt(grr) / sqrt(total)
  list(table = tab, components = vc, ndc = ndc, pct_grr = pct,
       balanced = length(unique(table(do.call(paste, c(lapply(factors, function(f) dd[[vn[f]]]), sep = "\r"))))) == 1,
       verdict = if (pct < 10) "Acceptable: Gage R&R is under 10% of the study variation."
                 else if (pct <= 30) "Marginal: Gage R&R is 10-30% of the study variation; may be acceptable depending on the application."
                 else "Unacceptable: Gage R&R is over 30% of the study variation.",
       data = d, meas = meas, part = part, operator = operator)
}

gage_expanded_plot <- function(g) {
  t <- g$table[g$table$Source != "Total variation", ]
  t$Source <- factor(trimws(t$Source), levels = rev(trimws(t$Source)))
  long <- rbind(data.frame(Source = t$Source, Percent = t$`% Contribution`, Measure = "% Contribution"),
                data.frame(Source = t$Source, Percent = t$`% Study var`, Measure = "% Study variation"))
  p1 <- ggplot2::ggplot(long, ggplot2::aes(Percent, Source, fill = Measure)) + ggplot2::geom_col(position = "dodge") +
    ggplot2::geom_vline(xintercept = c(10, 30), linetype = 2, colour = "grey50") +
    ggplot2::scale_fill_manual(values = c("#9bb7d4", "#4a6fa5")) +
    ggplot2::labs(title = "Components of variation", x = "Percent", y = NULL, fill = NULL) +
    ggplot2::theme_bw(base_size = 12) + ggplot2::theme(legend.position = "top", plot.title = ggplot2::element_text(face = "bold"))
  d <- g$data
  p2 <- ggplot2::ggplot(d, ggplot2::aes(.data[[g$part]], .data[[g$meas]], colour = .data[[g$operator]], group = .data[[g$operator]])) +
    ggplot2::stat_summary(fun = mean, geom = "line", linewidth = 1) + ggplot2::geom_point(alpha = 0.5) +
    ggplot2::labs(title = sprintf("%s by part and %s", g$meas, g$operator), x = g$part, y = g$meas, colour = g$operator) +
    ggplot2::theme_bw(base_size = 12) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                                                       axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  list(p1, p2)
}
