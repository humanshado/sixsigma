# Attribute agreement analysis (measurement system analysis for attribute
# or ordinal judgements), following Minitab's layout:
#   within appraiser, each appraiser vs standard, between appraisers,
#   all appraisers vs standard; Fleiss' kappa; Kendall's W for ordinal data.

# Fleiss' kappa from a subjects x categories count matrix (each row sums to
# the number of ratings per subject). Per-category kappas and the standard
# errors under H0 (Fleiss, Nee & Landis, 1979).
fleiss_kappa <- function(counts) {
  counts <- as.matrix(counts)
  n <- rowSums(counts)
  if (length(unique(n)) != 1) stop("Each subject needs the same number of ratings for Fleiss' kappa.")
  n <- n[1]; N <- nrow(counts)
  if (n < 2) stop("Kappa needs at least two ratings per subject.")
  pj <- colSums(counts) / (N * n)
  Pi <- (rowSums(counts^2) - n) / (n * (n - 1))
  Pbar <- mean(Pi); Pe <- sum(pj^2)
  kappa <- if (Pe < 1) (Pbar - Pe) / (1 - Pe) else 1
  qj <- 1 - pj
  kj <- ifelse(pj > 0 & pj < 1, 1 - colSums(counts * (n - counts)) / (N * n * (n - 1) * pj * qj), NA)
  se_j <- rep(sqrt(2 / (N * n * (n - 1))), length(pj))
  spq <- sum(pj * qj)
  se <- if (spq > 0) sqrt(2) / (spq * sqrt(N * n * (n - 1))) * sqrt(spq^2 - sum(pj * qj * (qj - pj))) else NA
  tab <- data.frame(Response = c(colnames(counts), "Overall"), Kappa = c(kj, kappa), `SE kappa` = c(se_j, se),
                    check.names = FALSE)
  tab$Z <- tab$Kappa / tab$`SE kappa`
  tab$`p-value (vs > 0)` <- stats::pnorm(tab$Z, lower.tail = FALSE)
  list(kappa = kappa, table = tab)
}

# Kendall's coefficient of concordance W (with tie correction) for a
# subjects x raters matrix of ordinal scores.
kendall_w <- function(scores) {
  scores <- as.matrix(scores)
  N <- nrow(scores); m <- ncol(scores)
  R <- apply(scores, 2, rank)
  Ri <- rowSums(R)
  S <- sum((Ri - mean(Ri))^2)
  Tj <- sum(apply(scores, 2, function(v) { t <- table(v); sum(t^3 - t) }))
  W <- 12 * S / (m^2 * (N^3 - N) - m * Tj)
  chi <- m * (N - 1) * W
  list(W = W, chisq = chi, df = N - 1, p = stats::pchisq(chi, N - 1, lower.tail = FALSE))
}

exact_ci <- function(x, n, conf) {
  if (n == 0) return(c(NA, NA))
  stats::binom.test(x, n, conf.level = conf)$conf.int * 100
}

# Long data: one row per rating with sample, appraiser and response (and an
# optional known standard for the sample).
agreement_analysis <- function(df, sample, appraiser, response, standard = NULL, conf = 0.95, ordinal = FALSE) {
  cols <- c(sample, appraiser, response, standard)
  if (anyDuplicated(cols)) stop("Choose different columns for sample, appraiser, response and standard.")
  d <- df[stats::complete.cases(df[, cols, drop = FALSE]), cols, drop = FALSE]
  names(d) <- c("s", "a", "r", if (!is.null(standard)) "std")
  d$s <- as.character(d$s); d$a <- as.character(d$a)
  levs <- if (ordinal && is.numeric(d$r)) sort(unique(c(d$r, d$std))) else sort(unique(as.character(c(d$r, d$std))))
  d$r <- factor(as.character(d$r), levels = as.character(levs))
  if (!is.null(standard)) {
    d$std <- factor(as.character(d$std), levels = as.character(levs))
    chk <- tapply(d$std, d$s, function(v) length(unique(v)))
    if (any(chk > 1)) stop("The standard must be the same for every rating of a sample.")
  }
  apps <- sort(unique(d$a)); samples <- sort(unique(d$s))
  trials <- table(d$s, d$a)
  if (length(unique(as.vector(trials))) != 1) stop("Every appraiser must rate every sample the same number of times (a balanced study).")
  k <- trials[1, 1]
  stdv <- if (!is.null(standard)) tapply(as.character(d$std), d$s, `[`, 1)[samples] else NULL

  row_ci <- function(label, matched, n) {
    ci <- exact_ci(matched, n, conf)
    data.frame(Appraiser = label, Inspected = n, Matched = matched, Percent = 100 * matched / n,
               `CI lower` = ci[1], `CI upper` = ci[2], check.names = FALSE)
  }
  counts_for <- function(rows) {
    t(vapply(samples, function(s) as.numeric(table(rows$r[rows$s == s])), numeric(length(levs))))
  }

  within <- if (k >= 2) {
    do.call(rbind, lapply(apps, function(a) {
      x <- d[d$a == a, ]
      m <- sum(vapply(samples, function(s) length(unique(x$r[x$s == s])) == 1, logical(1)))
      row_ci(a, m, length(samples))
    }))
  }
  within_kappa <- if (k >= 2) lapply(stats::setNames(apps, apps), function(a) {
    cm <- counts_for(d[d$a == a, ]); colnames(cm) <- as.character(levs); fleiss_kappa(cm)$table
  })
  vs_std <- NULL; vs_std_kappa <- NULL; disagree <- NULL
  if (!is.null(standard)) {
    vs_std <- do.call(rbind, lapply(apps, function(a) {
      x <- d[d$a == a, ]
      m <- sum(vapply(samples, function(s) all(as.character(x$r[x$s == s]) == stdv[[s]]), logical(1)))
      row_ci(a, m, length(samples))
    }))
    vs_std_kappa <- lapply(stats::setNames(apps, apps), function(a) {
      x <- d[d$a == a, ]
      # Fleiss' kappa between the appraiser's trials and the standard, as in Minitab: each trial paired with the standard.
      do.call(rbind, lapply(seq_len(k), function(tr) {
        rr <- vapply(samples, function(s) as.character(x$r[x$s == s])[tr], "")
        cm <- t(vapply(seq_along(samples), function(i) as.numeric(table(factor(c(rr[i], stdv[[samples[i]]]), levels = levs))), numeric(length(levs))))
        colnames(cm) <- as.character(levs)
        cbind(Trial = tr, fleiss_kappa(cm)$table)
      }))
    })
    disagree <- do.call(rbind, lapply(apps, function(a) {
      x <- d[d$a == a, ]
      res <- vapply(samples, function(s) {
        r <- as.character(x$r[x$s == s])
        if (length(unique(r)) > 1) "mixed" else if (r[1] == stdv[[s]]) "ok" else "wrong"
      }, "")
      data.frame(Appraiser = a, `Consistently wrong` = sum(res == "wrong"), `Mixed (inconsistent)` = sum(res == "mixed"),
                 `Percent wrong` = 100 * mean(res == "wrong"), `Percent mixed` = 100 * mean(res == "mixed"), check.names = FALSE)
    }))
  }
  between_m <- sum(vapply(samples, function(s) length(unique(d$r[d$s == s])) == 1, logical(1)))
  between <- row_ci("All appraisers", between_m, length(samples))
  cmb <- counts_for(d); colnames(cmb) <- as.character(levs)
  between_kappa <- fleiss_kappa(cmb)$table
  all_std <- if (!is.null(standard)) {
    m <- sum(vapply(samples, function(s) all(as.character(d$r[d$s == s]) == stdv[[s]]), logical(1)))
    row_ci("All appraisers vs standard", m, length(samples))
  }
  kendall <- NULL
  if (ordinal) {
    num <- suppressWarnings(as.numeric(as.character(levs)))
    if (any(is.na(num))) stop("Ordinal analysis needs numeric response levels (e.g. 1 to 5).")
    sc <- function(rows) do.call(cbind, lapply(split(rows, rows$a_t), function(x) as.numeric(as.character(x$r[match(samples, x$s)]))))
    d$a_t <- paste(d$a, stats::ave(seq_len(nrow(d)), d$s, d$a, FUN = seq_along))
    kw_within <- if (k >= 2) do.call(rbind, lapply(apps, function(a) {
      w <- kendall_w(sc(d[d$a == a, ]))
      data.frame(Assessment = paste("Within", a), W = w$W, `Chi-square` = w$chisq, DF = w$df, `p-value` = w$p, check.names = FALSE)
    }))
    w <- kendall_w(sc(d))
    kendall <- rbind(kw_within, data.frame(Assessment = "Between appraisers", W = w$W, `Chi-square` = w$chisq, DF = w$df,
                                           `p-value` = w$p, check.names = FALSE))
  }
  list(within = within, within_kappa = within_kappa, vs_std = vs_std, vs_std_kappa = vs_std_kappa, disagree = disagree,
       between = between, between_kappa = between_kappa, all_std = all_std, kendall = kendall,
       trials = k, appraisers = apps, n_samples = length(samples), levels = as.character(levs), conf = conf)
}

agreement_plot <- function(a) {
  rows <- list()
  if (!is.null(a$within)) rows[[1]] <- cbind(a$within, Assessment = "Within appraisers")
  if (!is.null(a$vs_std)) rows[[length(rows) + 1]] <- cbind(a$vs_std, Assessment = "Appraiser vs standard")
  d <- do.call(rbind, rows)
  if (is.null(d)) return(NULL)
  d$Assessment <- factor(d$Assessment, levels = unique(d$Assessment))
  ggplot2::ggplot(d, ggplot2::aes(Appraiser, Percent)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = `CI lower`, ymax = `CI upper`), width = 0.2, colour = "#4a6fa5") +
    ggplot2::geom_point(colour = "#c0392b", size = 3) +
    ggplot2::facet_wrap(~Assessment) + ggplot2::coord_cartesian(ylim = c(0, 100)) +
    ggplot2::labs(title = "Assessment agreement", subtitle = sprintf("Points: percent matched; bars: %g%% exact confidence intervals", 100 * a$conf),
                  y = "Percent", x = NULL) +
    ggplot2::theme_bw(base_size = 12) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}
