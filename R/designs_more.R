# More designs: blocking for two-level designs, general full factorials,
# split-plot designs, definitive screening designs, D-optimal designs and
# extreme-vertices (constrained) mixture designs.

# ---- Blocking two-level designs -----------------------------------------------------
# Choose b block generators (products of the design's base columns) so that no
# block effect is confounded with a main effect and as few as possible with
# two-factor interactions. Returns the block number of each run.
doe_block_assign <- function(coded, blocks) {
  if (blocks == 1) return(rep(1L, nrow(coded)))
  b <- log2(blocks)
  if (b != round(b) || b < 1 || b > 3) stop("Use 1, 2, 4 or 8 blocks.")
  n <- nrow(coded); k <- ncol(coded)
  base <- round(log2(n))
  if (2^base != n) stop("Blocking needs a two-level factorial with a power-of-two number of runs.")
  if (blocks > n / 2) stop("Too many blocks for ", n, " runs.")
  # Base columns: the first `base` factors of a regular design span all effects.
  B <- coded[, seq_len(base), drop = FALSE]
  words <- unlist(lapply(2:base, function(m) utils::combn(base, m, simplify = FALSE)), recursive = FALSE)
  wcol <- lapply(words, function(w) apply(B[, w, drop = FALSE], 1, prod))
  me <- lapply(seq_len(k), function(j) coded[, j])
  tfi <- if (k > 1) lapply(utils::combn(k, 2, simplify = FALSE), function(p) coded[, p[1]] * coded[, p[2]]) else list()
  same <- function(a, b) abs(sum(a * b)) == n
  best <- NULL
  combos <- utils::combn(length(words), b, simplify = FALSE)
  if (length(combos) > 50000) combos <- combos[seq_len(50000)]
  for (cmb in combos) {
    gens <- wcol[cmb]
    # All block effects: products of non-empty subsets of the generators.
    eff <- unlist(lapply(seq_len(b), function(m) lapply(utils::combn(b, m, simplify = FALSE),
                                                       function(s) Reduce(`*`, gens[s]))), recursive = FALSE)
    if (any(vapply(eff, function(e) all(e == 1) || all(e == -1), logical(1)))) next  # generators not independent
    n_me <- sum(vapply(eff, function(e) any(vapply(me, same, logical(1), b = e)), logical(1)))
    n_2 <- sum(vapply(eff, function(e) if (length(tfi)) sum(vapply(tfi, same, logical(1), b = e)) else 0L, numeric(1)))
    score <- c(n_me, n_2, -sum(lengths(words[cmb])))
    if (is.null(best) || any(score != best$score) && (score[1] < best$score[1] ||
        (score[1] == best$score[1] && (score[2] < best$score[2] || (score[2] == best$score[2] && score[3] < best$score[3])))))
      best <- list(score = score, gens = gens, words = words[cmb])
  }
  if (is.null(best) || best$score[1] > 0) stop("Cannot block this design without confounding main effects; use fewer blocks or more runs.")
  bits <- vapply(best$gens, function(g) as.integer(g > 0), integer(n))
  if (is.null(dim(bits))) bits <- matrix(bits, ncol = 1)
  blk <- as.integer(bits %*% 2^(seq_len(ncol(bits)) - 1)) + 1L
  attr(blk, "confounded_2fi") <- best$score[2]
  attr(blk, "generators") <- vapply(best$words, function(w) paste(LETTERS[w], collapse = ""), "")
  blk
}

# ---- General full factorial ---------------------------------------------------------

gff_create <- function(factors, levels, replicates = 1, randomize = TRUE, seed = NA, response = "Response",
                       block_by_replicate = FALSE) {
  if (length(factors) < 1) stop("Enter at least one factor.")
  nm <- trimws(factors)
  if (any(!nzchar(nm)) || anyDuplicated(nm)) stop("Factor names must be filled in and different.")
  lv <- lapply(levels, function(s) { v <- trimws(strsplit(s, ",", fixed = TRUE)[[1]]); v[nzchar(v)] })
  if (any(lengths(lv) < 2)) stop("Every factor needs at least two levels, separated by commas.")
  g <- expand.grid(rev(lv), stringsAsFactors = FALSE)[, rev(seq_along(lv)), drop = FALSE]
  names(g) <- nm
  if (nrow(g) * replicates > 5000) stop("That design has more than 5,000 runs.")
  conv <- function(v) { x <- suppressWarnings(as.numeric(v)); if (all(!is.na(x))) x else v }
  for (j in seq_along(nm)) g[[j]] <- conv(g[[j]])
  out <- do.call(rbind, lapply(seq_len(replicates), function(r) cbind(Replicate = r, g)))
  out <- data.frame(StdOrder = seq_len(nrow(out)), out, check.names = FALSE)
  if (!is.na(seed)) set.seed(seed)
  if (randomize) {
    out$RunOrder <- if (block_by_replicate) unlist(lapply(split(seq_len(nrow(out)), out$Replicate), function(i) sample(i)))[order(unlist(split(seq_len(nrow(out)), out$Replicate)))]
                    else sample(nrow(out))
  } else out$RunOrder <- seq_len(nrow(out))
  if (block_by_replicate) out$Blocks <- out$Replicate
  out[[response]] <- NA_real_
  out <- out[order(out$RunOrder), c("StdOrder", "RunOrder", setdiff(names(out), c("StdOrder", "RunOrder"))), drop = FALSE]
  rownames(out) <- NULL
  out
}

# ---- Split-plot designs ---------------------------------------------------------------
# Hard-to-change (HTC) factors are set once per whole plot; easy-to-change
# (ETC) factors vary within each whole plot. Both parts are two-level full
# factorials; whole plots are replicated and randomized, and subplots are
# randomized within each whole plot.
splitplot_create <- function(factors, htc, wp_reps = 2, randomize = TRUE, seed = NA, response = "Response") {
  nm <- trimws(factors$name)
  if (any(!nzchar(nm)) || anyDuplicated(nm)) stop("Factor names must be filled in and different.")
  if (!length(htc) || all(htc)) stop("Mark at least one factor as hard to change and keep at least one easy to change.")
  if (any(factors$low >= factors$high)) stop("Each low level must be below its high level.")
  full <- function(k) as.matrix(expand.grid(rep(list(c(-1, 1)), k)))
  H <- full(sum(htc)); E <- full(sum(!htc))
  if (!is.na(seed)) set.seed(seed)
  wps <- do.call(rbind, rep(list(H), wp_reps))
  wp_order <- if (randomize) sample(nrow(wps)) else seq_len(nrow(wps))
  rows <- list(); run <- 0
  for (w in wp_order) {
    sub <- if (randomize) E[sample(nrow(E)), , drop = FALSE] else E
    for (i in seq_len(nrow(sub))) {
      run <- run + 1
      v <- numeric(length(nm)); v[htc] <- wps[w, ]; v[!htc] <- sub[i, ]
      rows[[run]] <- c(RunOrder = run, WholePlot = match(w, wp_order), v)
    }
  }
  m <- do.call(rbind, rows)
  out <- data.frame(RunOrder = m[, 1], WholePlot = m[, 2])
  for (j in seq_along(nm)) {
    lo <- factors$low[j]; hi <- factors$high[j]
    out[[nm[j]]] <- ifelse(m[, j + 2] < 0, lo, hi)
  }
  out[[response]] <- NA_real_
  out
}

# REML analysis: fixed factorial effects plus a random whole-plot effect.
splitplot_analyze <- function(df, response, factors, wholeplot, order = 2) {
  if (length(factors) < 2) stop("Choose at least two factors.")
  keep <- !is.na(df[[response]]) & stats::complete.cases(df[, c(factors, wholeplot), drop = FALSE])
  df <- df[keep, , drop = FALSE]
  if (nrow(df) < 6) stop("Enter response values for more runs first.")
  L <- LETTERS[seq_along(factors)]
  X <- as.data.frame(stats::setNames(lapply(factors, function(f) doe_code_column(df[[f]], f)$x), L))
  if (any(X == 0)) stop("Split-plot analysis expects two-level factors without center points.")
  d <- cbind(X, .y = df[[response]], .wp = factor(df[[wholeplot]]))
  order <- max(1, min(order, length(L)))
  fx <- stats::as.formula(paste0(".y ~ (", paste(L, collapse = " + "), ")", if (order > 1) paste0("^", order) else ""))
  fit <- tryCatch(nlme::lme(fx, random = ~ 1 | .wp, data = d, method = "REML"),
                  error = function(e) stop("The REML fit failed (check that whole plots are replicated): ", conditionMessage(e)))
  tt <- summary(fit)$tTable
  terms <- rownames(tt)[-1]
  lab <- vapply(terms, function(t) paste(factors[match(strsplit(t, ":", fixed = TRUE)[[1]], L)], collapse = "*"), "")
  wp_level <- vapply(terms, function(t) {
    f <- strsplit(t, ":", fixed = TRUE)[[1]]
    all(vapply(f, function(v) length(unique(d[[v]])) == 1 || all(tapply(d[[v]], d$.wp, function(z) length(unique(z)) == 1)), logical(1)))
  }, logical(1))
  vc <- nlme::VarCorr(fit)
  vwp <- as.numeric(vc[1, "Variance"]); vsp <- fit$sigma^2
  list(fit = fit,
       table = data.frame(Term = lab, Stratum = ifelse(wp_level, "Whole plot", "Subplot"), Effect = 2 * tt[-1, 1],
                          Coef = tt[-1, 1], `SE coef` = tt[-1, 2], DF = tt[-1, 3], T = tt[-1, 4], `p-value` = tt[-1, 5],
                          check.names = FALSE, row.names = NULL),
       vc = data.frame(Source = c("Whole plot", "Subplot (error)"), `Variance component` = c(vwp, vsp),
                       `% of total` = 100 * c(vwp, vsp) / (vwp + vsp), check.names = FALSE),
       factors = factors)
}

# ---- Safe formula evaluation (constraints, Monte Carlo transfer functions) ------------
safe_eval <- function(formula, vars) {
  expr <- tryCatch(parse(text = formula, keep.source = FALSE), error = function(e) stop("The formula '", formula, "' has a syntax error."))
  if (length(expr) != 1) stop("Enter a single formula.")
  dt_check_formula(expr[[1]], names(vars))
  fenv <- new.env(parent = baseenv())
  for (f in c("sd", "var", "median")) assign(f, get(f, envir = asNamespace("stats")), envir = fenv)
  eval(expr[[1]], list2env(as.list(vars), parent = fenv))
}

# ---- Definitive screening designs ------------------------------------------------------
# Conference matrices by the Paley construction over GF(q), q = n - 1.
gf_elements <- function(q) {
  if (q == 9) return(as.matrix(expand.grid(0:2, 0:2)))  # a + b*i with i^2 = -1 (mod 3)
  matrix(0:(q - 1), ncol = 1)
}
gf_sub <- function(x, y, q) (x - y) %% (if (q == 9) 3 else q)
gf_mul <- function(x, y, q) {
  if (q == 9) return(c((x[1] * y[1] - x[2] * y[2]) %% 3, (x[1] * y[2] + x[2] * y[1]) %% 3))
  (x * y) %% q
}
conference_matrix <- function(n) {
  q <- n - 1
  if (!q %in% c(3, 5, 7, 9, 11, 13, 17, 19, 23, 29)) stop("No conference matrix of order ", n, " is built in.")
  E <- gf_elements(q); nq <- nrow(E)
  key <- function(x) paste(x, collapse = ",")
  sq <- unique(vapply(seq_len(nq)[-1], function(i) key(gf_mul(E[i, ], E[i, ], q)), ""))
  chi <- function(x) if (all(x == 0)) 0 else if (key(x) %in% sq) 1 else -1
  Q <- matrix(0, nq, nq)
  for (i in seq_len(nq)) for (j in seq_len(nq)) Q[i, j] <- chi(gf_sub(E[i, ], E[j, ], q))
  first <- if (q %% 4 == 1) 1 else -1
  rbind(c(0, rep(1, q)), cbind(first * rep(1, q), Q))
}

dsd_create <- function(factors, extra_center = 0, randomize = TRUE, seed = NA, response = "Response") {
  m <- nrow(factors)
  if (m < 3) stop("Definitive screening designs need at least 3 factors.")
  if (m > 30) stop("Use at most 30 factors.")
  n <- if (m %% 2 == 0) m else m + 1
  if (n == 16 || n == 22 || n == 26 || n == 28) n <- n + 2  # orders without a Paley conference matrix
  C <- conference_matrix(n)[, seq_len(m), drop = FALSE]
  coded <- rbind(C, -C, matrix(0, 1 + extra_center, m))
  doe_decode_design(coded, factors, randomize, seed, response)
}

doe_decode_design <- function(coded, factors, randomize = TRUE, seed = NA, response = "Response") {
  nm <- trimws(factors$name)
  if (any(!nzchar(nm)) || anyDuplicated(nm)) stop("Factor names must be filled in and different.")
  if (any(factors$low >= factors$high)) stop("Each low level must be below its high level.")
  if (!is.na(seed)) set.seed(seed)
  n <- nrow(coded)
  run <- if (randomize) sample(n) else seq_len(n)
  out <- data.frame(StdOrder = seq_len(n), RunOrder = run)
  for (j in seq_along(nm)) {
    mid <- (factors$low[j] + factors$high[j]) / 2; half <- (factors$high[j] - factors$low[j]) / 2
    out[[nm[j]]] <- signif(mid + half * coded[, j], 10)
  }
  out[[response]] <- NA_real_
  attr(out, "coded") <- coded
  out <- out[order(out$RunOrder), , drop = FALSE]; rownames(out) <- NULL
  out
}

# ---- D-optimal designs -------------------------------------------------------------
doe_model_matrix <- function(coded, model = c("linear", "interaction", "quadratic")) {
  model <- match.arg(model)
  X <- cbind(1, coded)
  k <- ncol(coded)
  if (model != "linear" && k > 1)
    for (p in utils::combn(k, 2, simplify = FALSE)) X <- cbind(X, coded[, p[1]] * coded[, p[2]])
  if (model == "quadratic") X <- cbind(X, coded^2)
  X
}

# Fedorov-style exchange on a candidate grid, keeping only candidates that meet
# the linear constraints (formulas in actual units, e.g. "Temp + 10*Time <= 400").
doptimal_create <- function(factors, runs, model = "quadratic", constraints = character(0), starts = 5,
                            randomize = TRUE, seed = NA, response = "Response") {
  k <- nrow(factors)
  if (k < 1 || k > 8) stop("Use 1 to 8 factors.")
  lv <- if (model == "quadratic") c(-1, -0.5, 0, 0.5, 1) else c(-1, 0, 1)
  if (k > 5) lv <- if (model == "quadratic") c(-1, 0, 1) else c(-1, 1)
  cand <- as.matrix(expand.grid(rep(list(lv), k)))
  nm <- trimws(factors$name)
  actual <- sweep(sweep(cand, 2, (factors$high - factors$low) / 2, `*`), 2, (factors$high + factors$low) / 2, `+`)
  colnames(actual) <- nm
  constraints <- trimws(constraints); constraints <- constraints[nzchar(constraints)]
  ok <- rep(TRUE, nrow(cand))
  for (cn in constraints) {
    v <- safe_eval(cn, as.data.frame(actual))
    if (!is.logical(v) || length(v) != nrow(cand)) stop("The constraint '", cn, "' must be a comparison such as  A + B <= 10.")
    ok <- ok & !is.na(v) & v
  }
  cand <- cand[ok, , drop = FALSE]
  X <- doe_model_matrix(cand, model)
  p <- ncol(X)
  if (runs < p) stop(sprintf("The %s model has %d terms: use at least %d runs.", model, p, p))
  if (nrow(cand) < p) stop("Too few candidate points satisfy the constraints for this model.")
  if (!is.na(seed)) set.seed(seed)
  logdet <- function(M) { d <- determinant(M, logarithm = TRUE); if (d$sign <= 0) -Inf else as.numeric(d$modulus) }
  best <- NULL
  for (s in seq_len(starts)) {
    idx <- sample(nrow(cand), runs, replace = nrow(cand) < runs)
    M <- crossprod(X[idx, , drop = FALSE]) + diag(1e-8, p)
    cur <- logdet(M)
    for (iter in 1:100) {
      Minv <- tryCatch(solve(M), error = function(e) NULL)
      if (is.null(Minv)) { idx <- sample(nrow(cand), runs, replace = TRUE); M <- crossprod(X[idx, , drop = FALSE]) + diag(1e-8, p); next }
      dc <- rowSums((X %*% Minv) * X)                 # prediction variance of candidates
      dd <- dc[idx]
      improved <- FALSE
      # Fedorov delta for swapping design point i with candidate j
      for (i in order(dd)) {
        xi <- X[idx[i], ]
        dij <- X %*% (Minv %*% xi)
        delta <- dc - dd[i] - (dc * dd[i] - dij^2)
        j <- which.max(delta)
        if (delta[j] > 1e-6) {
          idx[i] <- j
          M <- crossprod(X[idx, , drop = FALSE]) + diag(1e-8, p)
          improved <- TRUE
          break
        }
      }
      if (!improved) break
      cur <- logdet(M)
    }
    cur <- logdet(crossprod(X[idx, , drop = FALSE]))
    if (is.null(best) || cur > best$ld) best <- list(ld = cur, idx = idx)
  }
  if (!is.finite(best$ld)) stop("Could not find a design that estimates every model term; add runs or relax the constraints.")
  D <- cand[best$idx, , drop = FALSE]
  out <- doe_decode_design(D, factors, randomize, seed, response)
  Xd <- doe_model_matrix(D, model)
  attr(out, "efficiency") <- c(`D-efficiency (%)` = 100 * exp(best$ld / p) / runs,
                               `log det(X'X)` = best$ld, Terms = p, Candidates = nrow(cand))
  out
}

# ---- Constrained mixtures: extreme vertices (McLean & Anderson) ----------------------
mixture_vertices <- function(lower, upper) {
  q <- length(lower)
  if (any(lower < 0) || any(upper > 1) || any(lower > upper)) stop("Bounds must satisfy 0 <= lower <= upper <= 1.")
  if (sum(lower) > 1 + 1e-9 || sum(upper) < 1 - 1e-9) stop("No blend satisfies these bounds: the lower bounds add to more than 1 or the upper bounds to less than 1.")
  V <- list()
  for (free in seq_len(q)) {
    others <- setdiff(seq_len(q), free)
    g <- as.matrix(expand.grid(lapply(others, function(i) c(lower[i], upper[i]))))
    for (r in seq_len(nrow(g))) {
      x <- numeric(q); x[others] <- g[r, ]; x[free] <- 1 - sum(g[r, ])
      if (x[free] >= lower[free] - 1e-9 && x[free] <= upper[free] + 1e-9) V[[length(V) + 1]] <- round(x, 10)
    }
  }
  V <- unique(do.call(rbind, V))
  V
}

mixture_constrained <- function(components, lower, upper, edges = TRUE, axial = TRUE, centroid = TRUE,
                                replicates = 1, randomize = TRUE, seed = NA, response = "Response") {
  q <- length(components)
  if (q < 2) stop("Enter at least two components.")
  V <- mixture_vertices(lower, upper)
  pts <- list(V); type <- rep("Vertex", nrow(V))
  if (edges && q > 2) {
    E <- list()
    for (p in utils::combn(nrow(V), 2, simplify = FALSE)) {
      # An edge joins two vertices that share q-2 active bounds.
      a <- V[p[1], ]; b <- V[p[2], ]
      same_bound <- sum(abs(a - b) < 1e-9 & (abs(a - lower) < 1e-9 | abs(a - upper) < 1e-9))
      if (same_bound >= q - 2) E[[length(E) + 1]] <- (a + b) / 2
    }
    if (length(E)) { E <- unique(round(do.call(rbind, E), 10)); pts <- c(pts, list(E)); type <- c(type, rep("Edge midpoint", nrow(E))) }
  }
  ctr <- colMeans(V)
  if (axial) { A <- sweep(V, 2, ctr, function(v, c) (v + c) / 2); pts <- c(pts, list(A)); type <- c(type, rep("Axial", nrow(A))) }
  if (centroid) { pts <- c(pts, list(matrix(ctr, 1))); type <- c(type, "Centroid") }
  M <- do.call(rbind, pts)
  base <- data.frame(PtType = type, round(M, 6)); names(base)[-1] <- components
  out <- do.call(rbind, rep(list(base), replicates))
  if (!is.na(seed)) set.seed(seed)
  out <- data.frame(StdOrder = seq_len(nrow(out)), RunOrder = if (randomize) sample(nrow(out)) else seq_len(nrow(out)), out,
                    check.names = FALSE)
  out[[response]] <- NA_real_
  out <- out[order(out$RunOrder), , drop = FALSE]; rownames(out) <- NULL
  out
}
