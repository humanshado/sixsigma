# Worksheet transformations. All functions take and return a data frame.

dt_stack <- function(df, cols, value = "Value", label = "Source", keep = NULL) {
  if (length(cols) < 2) stop("Choose two or more columns to stack.")
  if (value %in% c(label, keep) || label %in% keep) stop("Choose new names that differ from the kept columns.")
  out <- do.call(rbind, lapply(cols, function(cn) {
    d <- data.frame(df[, keep, drop = FALSE], check.names = FALSE)
    d[[label]] <- cn; d[[value]] <- df[[cn]]
    d
  }))
  rownames(out) <- NULL
  out
}

dt_unstack <- function(df, value, by) {
  if (value == by) stop("Choose different value and group columns.")
  g <- as.character(df[[by]])
  sp <- split(df[[value]], factor(g, levels = unique(g[!is.na(g)])))
  n <- max(lengths(sp))
  out <- as.data.frame(lapply(sp, function(v) c(v, rep(NA, n - length(v)))), check.names = FALSE)
  names(out) <- make.unique(paste(value, names(sp), sep = "_"))
  out
}

dt_subset_ops <- c("equals" = "==", "does not equal" = "!=", "is less than" = "<", "is at most" = "<=",
                   "is greater than" = ">", "is at least" = ">=", "contains" = "contains",
                   "is missing" = "missing", "is not missing" = "present")

dt_subset <- function(df, col, op, value = "") {
  v <- df[[col]]
  if (is.null(v)) stop("Unknown column.")
  cmp <- if (is.numeric(v) && op %in% c("==", "!=", "<", "<=", ">", ">=")) {
    x <- suppressWarnings(as.numeric(value))
    if (is.na(x)) stop("'", value, "' is not a number, but ", col, " is numeric.")
    x
  } else value
  keep <- switch(op,
    `==` = as.character(v) == as.character(cmp) & !is.na(v), `!=` = as.character(v) != as.character(cmp) & !is.na(v),
    `<` = v < cmp, `<=` = v <= cmp, `>` = v > cmp, `>=` = v >= cmp,
    contains = grepl(value, as.character(v), fixed = TRUE),
    missing = is.na(v), present = !is.na(v), stop("Unknown condition."))
  if (is.numeric(v) && op %in% c("==", "!=")) keep <- switch(op, `==` = v == cmp, `!=` = v != cmp)
  keep[is.na(keep)] <- FALSE
  out <- df[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}

dt_sort <- function(df, col, descending = FALSE) {
  o <- order(df[[col]], decreasing = descending, na.last = TRUE)
  out <- df[o, , drop = FALSE]; rownames(out) <- NULL; out
}

# Calculated columns. Only arithmetic, comparisons and a whitelist of
# functions are allowed, so a formula cannot run arbitrary code.
dt_allowed_functions <- c(
  "+", "-", "*", "/", "^", "%%", "%/%", "(", "==", "!=", "<", ">", "<=", ">=", "&", "|", "!",
  "abs", "sqrt", "exp", "log", "log10", "log2", "round", "signif", "floor", "ceiling", "sin", "cos", "tan",
  "mean", "median", "sd", "var", "min", "max", "sum", "pmin", "pmax", "cumsum", "rank", "length",
  "ifelse", "is.na", "nchar", "toupper", "tolower", "paste", "paste0", "substr", "as.numeric", "as.character",
  "seq_along", "rev", "c")

dt_check_formula <- function(expr, columns) {
  walk <- function(e) {
    if (is.call(e)) {
      fn <- e[[1]]
      if (!is.name(fn) || !(as.character(fn) %in% dt_allowed_functions))
        stop("'", deparse(fn)[1], "' is not allowed in formulas (see the allowed functions under the formula box).")
      for (a in as.list(e)[-1]) if (!missing(a)) walk(a)
    } else if (is.name(e)) {
      nm <- as.character(e)
      if (!(nm %in% c(columns, "pi", "TRUE", "FALSE", "NA", "T", "F")))
        stop("Unknown column '", nm, "'. Use backticks for names with spaces, e.g. `my col`.")
    }
  }
  walk(expr)
  invisible(TRUE)
}

dt_calc <- function(df, name, formula) {
  name <- trimws(name)
  if (!nzchar(name)) stop("Enter a name for the new column.")
  expr <- tryCatch(parse(text = formula, keep.source = FALSE), error = function(e) stop("The formula has a syntax error."))
  if (length(expr) != 1) stop("Enter a single formula.")
  dt_check_formula(expr[[1]], names(df))
  fenv <- new.env(parent = baseenv())
  for (f in c("sd", "var", "median")) assign(f, get(f, envir = asNamespace("stats")), envir = fenv)
  env <- list2env(as.list(df), parent = fenv)
  v <- eval(expr[[1]], env)
  if (!(length(v) %in% c(1, nrow(df)))) stop("The formula gives ", length(v), " values; the worksheet has ", nrow(df), " rows.")
  df[[name]] <- rep_len(v, nrow(df))
  df
}

# Recode with lines of "old = new".
dt_recode <- function(df, col, rules, new_name = "") {
  lines <- trimws(strsplit(rules, "\n", fixed = TRUE)[[1]]); lines <- lines[nzchar(lines)]
  if (!length(lines) || !all(grepl("=", lines, fixed = TRUE))) stop("Write one rule per line as  old = new.")
  from <- trimws(sub("=.*$", "", lines)); to <- trimws(sub("^[^=]*=", "", lines))
  v <- as.character(df[[col]])
  out <- v
  for (i in seq_along(from)) out[!is.na(v) & v == from[i]] <- to[i]
  num <- suppressWarnings(as.numeric(out))
  if (all(is.na(num) == is.na(out))) out <- num
  df[[if (nzchar(trimws(new_name))) trimws(new_name) else col]] <- out
  df
}

dt_convert <- function(df, col, type) {
  v <- df[[col]]
  df[[col]] <- switch(type,
    numeric = {
      x <- suppressWarnings(as.numeric(as.character(v)))
      bad <- sum(is.na(x) & !is.na(v))
      if (bad) stop(bad, " value(s) in ", col, " are not numbers (for example '", as.character(v[is.na(x) & !is.na(v)][1]), "').")
      x
    },
    text = as.character(v),
    category = factor(v),
    stop("Unknown type."))
  df
}

dt_rename <- function(df, col, new) {
  new <- trimws(new)
  if (!nzchar(new)) stop("Enter the new name.")
  if (new %in% setdiff(names(df), col)) stop("A column called '", new, "' already exists.")
  names(df)[names(df) == col] <- new
  df
}

dt_delete <- function(df, cols) {
  if (!length(cols)) stop("Choose the columns to delete.")
  if (length(cols) >= ncol(df)) stop("Keep at least one column.")
  df[, setdiff(names(df), cols), drop = FALSE]
}

dt_drop_missing <- function(df, cols = names(df)) {
  out <- df[stats::complete.cases(df[, cols, drop = FALSE]), , drop = FALSE]
  rownames(out) <- NULL
  out
}
