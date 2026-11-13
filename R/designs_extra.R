# Additional experimental designs: Plackett-Burman screening designs,
# response surface designs (central composite, Box-Behnken), mixture
# designs (simplex lattice, simplex centroid) and Taguchi orthogonal arrays.
#
# Feature set modelled on r6qualitytools (Encarnacion, Barahona, Flores et al.),
# implemented here from the published constructions.

# ---- Plackett-Burman -----------------------------------------------------------
# First rows of the cyclic Plackett & Burman (1946) designs; the remaining rows
# are cyclic shifts, plus a final row of all minus signs.
pb_generators <- list(
  `12` = "++-+++---+-",
  `20` = "++--++++-+-+----++-",
  `24` = "+++++-+-++--++--+-+----"
)

pb_matrix <- function(runs) {
  g <- pb_generators[[as.character(runs)]]
  if (is.null(g)) stop("Plackett-Burman designs are available with 12, 20 or 24 runs.")
  v <- ifelse(strsplit(g, "")[[1]] == "+", 1, -1)
  n <- length(v)
  m <- t(vapply(0:(n - 1), function(s) v[((seq_len(n) - 1 - s) %% n) + 1], numeric(n)))
  rbind(m, rep(-1, n))
}

# ---- Taguchi orthogonal arrays ------------------------------------------------
# Regular arrays are generated: two-level columns are XORs of the base
# columns (column j uses the base factors in the bits of j); three- and
# five-level columns are linear combinations of the base columns modulo p, in
# Taguchi's standard column order. L12 and L18 are the published tables.

oa_two_level <- function(k) {
  runs <- 2^k
  base <- vapply(1:k, function(b) rep(rep(0:1, each = runs / 2^b), 2^(b - 1)), numeric(runs))
  vapply(1:(runs - 1), function(j) {
    bits <- which(bitwAnd(j, 2^(0:(k - 1))) > 0)
    (rowSums(base[, bits, drop = FALSE]) %% 2) + 1
  }, numeric(runs))
}

oa_prime_level <- function(p, k, coefs) {
  runs <- p^k
  base <- vapply(1:k, function(b) rep(rep(0:(p - 1), each = runs / p^b), p^(b - 1)), numeric(runs))
  vapply(coefs, function(cf) ((base %*% cf) %% p)[, 1] + 1, numeric(runs))
}

oa_from_rows <- function(rows) do.call(rbind, lapply(strsplit(rows, ""), as.numeric))

oa_tables <- list(
  L4 = list(levels = "3 factors at 2 levels", make = function() oa_two_level(2)),
  L8 = list(levels = "7 factors at 2 levels", make = function() oa_two_level(3)),
  L9 = list(levels = "4 factors at 3 levels",
            make = function() oa_prime_level(3, 2, list(c(1, 0), c(0, 1), c(1, 1), c(2, 1)))),
  L12 = list(levels = "11 factors at 2 levels", make = function() oa_from_rows(c(
    "11111111111", "11111222222", "11222111222", "12122122112", "12212212121", "12221221211",
    "21221122121", "21212221112", "21122212211", "22211112212", "22121211122", "22112121221"))),
  L16 = list(levels = "15 factors at 2 levels", make = function() oa_two_level(4)),
  L18 = list(levels = "1 factor at 2 levels and 7 at 3 levels", make = function() oa_from_rows(c(
    "11111111", "11222222", "11333333", "12112233", "12223311", "12331122", "13121323", "13232131",
    "13313212", "21133221", "21211332", "21322113", "22123132", "22231213", "22312321", "23132312",
    "23213123", "23321231"))),
  L25 = list(levels = "6 factors at 5 levels",
             make = function() oa_prime_level(5, 2, list(c(1, 0), c(0, 1), c(1, 1), c(2, 1), c(3, 1), c(4, 1)))),
  L27 = list(levels = "13 factors at 3 levels", make = function() oa_prime_level(3, 3, list(
    c(1, 0, 0), c(0, 1, 0), c(1, 1, 0), c(2, 1, 0), c(0, 0, 1), c(1, 0, 1), c(2, 0, 1), c(0, 1, 1),
    c(1, 1, 1), c(2, 1, 1), c(0, 2, 1), c(1, 2, 1), c(2, 2, 1)))),
  L32 = list(levels = "31 factors at 2 levels", make = function() oa_two_level(5))
)

taguchi_array <- function(name) {
  t <- oa_tables[[name]]
  if (is.null(t)) stop("Unknown orthogonal array ", name, ".")
  m <- t$make()
  colnames(m) <- paste0("C", seq_len(ncol(m)))
  m
}

# Create a Taguchi worksheet: factors assigned to the first columns (or to
# chosen columns), with one response column per noise replicate.
taguchi_create <- function(array, factors, columns = NULL, replicates = 1, response = "Y") {
  m <- taguchi_array(array)
  k <- length(factors)
  if (k < 1) stop("Enter at least one factor.")
  if (is.null(columns)) columns <- seq_len(k)
  if (length(columns) != k || any(columns > ncol(m)) || anyDuplicated(columns))
    stop(array, " has ", ncol(m), " columns; choose a different column for each factor.")
  nm <- trimws(factors)
  if (any(!nzchar(nm)) || anyDuplicated(nm)) stop("Factor names must be filled in and different.")
  out <- data.frame(Run = seq_len(nrow(m)))
  for (i in seq_len(k)) out[[nm[i]]] <- m[, columns[i]]
  for (r in seq_len(replicates)) out[[if (replicates == 1) response else paste0(response, r)]] <- NA_real_
  out
}

# ---- Response surface designs ----------------------------------------------------

ccd_alpha <- function(k, type = "rotatable", runs_factorial = 2^k) {
  switch(type, rotatable = runs_factorial^(1 / 4), face = 1, stop("Unknown axial distance type."))
}

# Central composite design in coded units. The factorial part is a full 2^k
# (or the resolution V fraction for 5+ factors when `fraction` is TRUE).
rsm_ccd <- function(k, alpha = "rotatable", center = 6, fraction = FALSE) {
  if (k < 2 || k > 6) stop("Central composite designs are offered for 2 to 6 factors.")
  runs_f <- if (fraction && k >= 5) 2^(k - 1) else 2^k
  fac <- doe_coded(k, runs_f)$matrix
  a <- if (is.numeric(alpha)) alpha else ccd_alpha(k, alpha, runs_f)
  ax <- do.call(rbind, lapply(seq_len(k), function(j) {
    m <- matrix(0, 2, k); m[1, j] <- -a; m[2, j] <- a; m
  }))
  ctr <- matrix(0, center, k)
  m <- rbind(unname(fac), ax, ctr)
  colnames(m) <- LETTERS[seq_len(k)]
  list(matrix = m, type = c(rep("Factorial", nrow(fac)), rep("Axial", nrow(ax)), rep("Center", center)), alpha = a)
}

# Box-Behnken design for 3 to 5 factors: every pair of factors at +/-1 with
# the others at 0, plus center points.
rsm_bbd <- function(k, center = 3) {
  if (k < 3 || k > 5) stop("Box-Behnken designs are offered for 3 to 5 factors.")
  pairs <- utils::combn(k, 2)
  sq <- as.matrix(expand.grid(c(-1, 1), c(-1, 1)))
  edge <- do.call(rbind, lapply(seq_len(ncol(pairs)), function(j) {
    m <- matrix(0, 4, k); m[, pairs[, j]] <- sq; m
  }))
  m <- rbind(edge, matrix(0, center, k))
  colnames(m) <- LETTERS[seq_len(k)]
  list(matrix = m, type = c(rep("Edge", nrow(edge)), rep("Center", center)), alpha = 1)
}

# Turn a coded RSM matrix into a worksheet. Factor levels: low/high map to
# coded -1/+1; axial points fall outside unless the design is face-centred.
rsm_worksheet <- function(design, factors, replicates = 1, randomize = TRUE, seed = NA, response = "Response") {
  k <- ncol(design$matrix)
  if (nrow(factors) != k) stop("Enter one row of levels per factor.")
  nm <- trimws(factors$name)
  if (any(!nzchar(nm)) || anyDuplicated(nm)) stop("Factor names must be filled in and different.")
  if (any(factors$low >= factors$high)) stop("Each low level must be below its high level.")
  m <- do.call(rbind, rep(list(design$matrix), replicates))
  type <- rep(design$type, replicates)
  std <- seq_len(nrow(m))
  if (!is.na(seed)) set.seed(seed)
  run <- if (randomize) sample(std) else std
  out <- data.frame(StdOrder = std, RunOrder = run, PtType = type)
  for (j in seq_len(k)) {
    mid <- (factors$low[j] + factors$high[j]) / 2; half <- (factors$high[j] - factors$low[j]) / 2
    out[[nm[j]]] <- mid + half * m[, j]
  }
  out[[response]] <- NA_real_
  out <- out[order(out$RunOrder), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# ---- Mixture designs ---------------------------------------------------------------

# All blends with proportions in {0, 1/m, ..., 1} summing to 1.
mixture_lattice <- function(q, m) {
  g <- as.matrix(expand.grid(rep(list(0:m), q)))
  g <- g[rowSums(g) == m, , drop = FALSE] / m
  g[order(-apply(g, 1, max), -rowSums(g > 0)), , drop = FALSE]
}

# Pure components, then every 2-, 3-, ..., q-component equal blend.
mixture_centroid <- function(q) {
  rows <- unlist(lapply(seq_len(q), function(s) {
    lapply(seq_len(ncol(utils::combn(q, s))), function(j) {
      v <- numeric(q); v[utils::combn(q, s)[, j]] <- 1 / s; v
    })
  }), recursive = FALSE)
  do.call(rbind, rows)
}

mixture_create <- function(components, type = "centroid", degree = 2, axial = FALSE, replicates = 1,
                            randomize = TRUE, seed = NA, response = "Response") {
  q <- length(components)
  nm <- trimws(components)
  if (q < 3 || q > 6) stop("Mixture designs are offered for 3 to 6 components.")
  if (any(!nzchar(nm)) || anyDuplicated(nm)) stop("Component names must be filled in and different.")
  m <- if (type == "centroid") mixture_centroid(q) else mixture_lattice(q, degree)
  if (axial) {
    # Axial check blends halfway between each vertex and the overall centroid.
    ax <- t(vapply(seq_len(q), function(i) { v <- rep(1 / (2 * q), q); v[i] <- v[i] + 0.5; v }, numeric(q)))
    m <- rbind(m, ax)
    if (type != "centroid") m <- rbind(m, rep(1 / q, q))
  }
  m <- unique(round(m, 10))
  m <- do.call(rbind, rep(list(m), replicates))
  std <- seq_len(nrow(m))
  if (!is.na(seed)) set.seed(seed)
  run <- if (randomize) sample(std) else std
  out <- data.frame(StdOrder = std, RunOrder = run)
  for (j in seq_len(q)) out[[nm[j]]] <- round(m[, j], 6)
  out[[response]] <- NA_real_
  out <- out[order(out$RunOrder), , drop = FALSE]
  rownames(out) <- NULL
  out
}
