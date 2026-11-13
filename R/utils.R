# Shared helpers for the app modules.

# Sample datasets: those in the SixSigma package plus the CSV files in
# data/qcr (from the qcr package). Returned as option groups for selectInput.
qcr_data_dir <- function() file.path("data", "qcr")

sample_datasets <- function() {
  items <- utils::data(package = "SixSigma")$results[, "Item"]
  env <- new.env()
  utils::data(list = items, package = "SixSigma", envir = env)
  keep <- vapply(items, function(i) is.data.frame(env[[i]]), logical(1))
  qcr <- sub("\\.csv$", "", list.files(qcr_data_dir(), pattern = "\\.csv$"))
  out <- list(`SixSigma package` = unname(items[keep]))
  if (length(qcr)) out$`qcr package` <- stats::setNames(paste0("qcr:", qcr), qcr)
  out
}

load_sample <- function(name) {
  if (startsWith(name, "qcr:")) {
    f <- file.path(qcr_data_dir(), paste0(sub("^qcr:", "", name), ".csv"))
    return(utils::read.csv(f, stringsAsFactors = TRUE))
  }
  env <- new.env()
  utils::data(list = name, package = "SixSigma", envir = env)
  as.data.frame(env[[name]])
}

numeric_cols <- function(df) names(df)[vapply(df, is.numeric, logical(1))]

# Run an analysis and turn R errors into a readable message for the user.
validate_run <- function(expr) {
  tryCatch(expr, error = function(e) {
    shiny::validate(shiny::need(FALSE, conditionMessage(e)))
  })
}

# Capture console output of a call (several SixSigma functions print
# their results rather than returning them).
capture_text <- function(expr) {
  paste(utils::capture.output(expr), collapse = "\n")
}

# Evaluate a call that also draws a plot, discarding the drawing. Used to get
# the printed results of SixSigma functions that always plot.
without_plot <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
}

# A numeric input that starts empty instead of showing "NA".
blank_numeric <- function(id, label, ...) {
  htmltools::tagQuery(shiny::numericInput(id, label, NA, ...))$find("input")$removeAttrs("value")$allTags()
}

# A numeric input that may be left blank. Returns NA when empty.
num_or_na <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) NA_real_ else as.numeric(x)

# PNG download for a plot-drawing function.
plot_download <- function(id_prefix, draw, width = 10, height = 7) {
  shiny::downloadHandler(
    filename = function() paste0(id_prefix, "-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".png"),
    content = function(file) {
      grDevices::png(file, width = width, height = height, units = "in", res = 150)
      on.exit(grDevices::dev.off())
      res <- draw()
      if (inherits(res, "ggplot")) print(res)
    }
  )
}

# Standard two-column layout: controls in a sidebar, results on the right.
analysis_layout <- function(title, controls, results) {
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(title = title, width = 320, open = "always", controls),
    results
  )
}

column_picker <- function(id, label, choices = NULL, selected = NULL, multiple = FALSE) {
  shiny::selectInput(id, label, choices = choices, selected = selected, multiple = multiple)
}

# Fill a set of column pickers whenever the active dataset changes.
# `spec` maps input id -> list(type = "numeric" | "any", default = <name or index>,
# optional = TRUE/FALSE).
update_pickers <- function(session, df, spec) {
  nums <- numeric_cols(df)
  all <- names(df)
  for (id in names(spec)) {
    s <- spec[[id]]
    choices <- if (identical(s$type, "numeric")) nums else all
    if (isTRUE(s$optional)) choices <- c("(none)" = "", choices)
    # Keep what the user picked if that column still exists (e.g. after a cell edit).
    prev <- shiny::isolate(session$input[[id]])
    sel <- if (!is.null(prev) && length(prev) == 1 && prev %in% choices) prev else s$default
    if (is.numeric(sel)) sel <- choices[min(sel, length(choices))]
    if (is.null(sel) || !(sel %in% choices)) sel <- if (isTRUE(s$optional)) "" else choices[1]
    shiny::updateSelectInput(session, id, choices = choices, selected = sel)
  }
}

empty_to_null <- function(x) if (is.null(x) || identical(x, "")) NULL else x

# Show numeric columns that hold only whole numbers without decimals.
int_cols <- function(df) {
  if (is.null(df)) return(df)
  for (i in seq_along(df)) {
    v <- df[[i]]
    if (is.numeric(v) && all(is.na(v) | (is.finite(v) & v == round(v) & abs(v) < 1e9))) df[[i]] <- as.integer(v)
  }
  df
}

# Keep a multi-select's current choices that still exist, else use the default.
keep_selected <- function(session, id, choices, default) {
  prev <- intersect(shiny::isolate(session$input[[id]]), choices)
  if (length(prev)) prev else default
}

# Clear numeric inputs (e.g. spec limits) so they show as blank.
clear_numeric <- function(session, ids) for (id in ids) session$sendInputMessage(id, list(value = ""))

# Spec limits typed one at a time pass through odd combinations (new LSL with
# the old USL). Wait until typing pauses before recalculating.
debounced_inputs <- function(input, ids, ms = 600) {
  stats::setNames(lapply(ids, function(id) shiny::debounce(shiny::reactive(input[[id]]), ms)), ids)
}

# TRUE when specification limits lie far outside the data, which usually
# means they belong to another dataset or were mistyped.
specs_far_from_data <- function(x, ...) {
  x <- x[!is.na(x)]; lim <- stats::na.omit(c(...))
  span <- diff(range(x)); if (!length(lim) || span <= 0) return(FALSE)
  diff(range(c(x, lim))) > 25 * span
}
