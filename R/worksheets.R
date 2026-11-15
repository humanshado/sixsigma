# Several worksheets with their own undo/redo history. The store is a plain
# list so the logic can be tested without Shiny:
#   list(sheets = list(<id> = list(id, name, df, undo, redo, pristine)), active = <id>, next_id = n)

WS_MAX_SHEETS <- 30
WS_MAX_UNDO <- 30
WS_MAX_UNDO_BYTES <- 300 * 1024^2   # large worksheets keep fewer undo steps

# Keep the newest history entries that fit both limits (at least one).
ws_trim <- function(h) {
  h <- utils::head(h, WS_MAX_UNDO)
  if (length(h) <= 1) return(h)
  sz <- cumsum(vapply(h, function(d) as.numeric(utils::object.size(d)), numeric(1)))
  h[seq_len(max(1, sum(sz <= WS_MAX_UNDO_BYTES)))]
}

ws_store <- function() list(sheets = list(), active = NULL, next_id = 1L)

ws_unique_name <- function(store, name) {
  taken <- vapply(store$sheets, `[[`, "", "name")
  name <- trimws(name); if (!nzchar(name)) name <- "Worksheet"
  if (!(name %in% taken)) return(name)
  i <- 2; while (sprintf("%s (%d)", name, i) %in% taken) i <- i + 1
  sprintf("%s (%d)", name, i)
}

ws_add <- function(store, df, name, pristine = FALSE) {
  if (length(store$sheets) >= WS_MAX_SHEETS) stop("At most ", WS_MAX_SHEETS, " worksheets can be open; close some first.")
  id <- paste0("ws", store$next_id)
  store$sheets[[id]] <- list(id = id, name = ws_unique_name(store, name), df = df, undo = list(), redo = list(), pristine = pristine)
  store$active <- id; store$next_id <- store$next_id + 1L
  store
}

ws_active <- function(store) if (is.null(store$active)) NULL else store$sheets[[store$active]]

# Data loaded from a source replaces the active sheet if nothing has been done
# to it yet; otherwise it opens in a new sheet, so work is never lost.
ws_load <- function(store, df, name) {
  a <- ws_active(store)
  if (!is.null(a) && isTRUE(a$pristine)) {
    others <- store; others$sheets[[a$id]] <- NULL
    store$sheets[[a$id]] <- list(id = a$id, name = ws_unique_name(others, name), df = df, undo = list(), redo = list(), pristine = TRUE)
    return(store)
  }
  ws_add(store, df, name, pristine = TRUE)
}

ws_change <- function(store, df) {
  a <- ws_active(store); if (is.null(a)) stop("No worksheet is open.")
  a$undo <- ws_trim(c(list(a$df), a$undo))
  a$redo <- list(); a$df <- df; a$pristine <- FALSE
  store$sheets[[a$id]] <- a
  store
}

ws_undo <- function(store) {
  a <- ws_active(store); if (is.null(a) || !length(a$undo)) stop("Nothing to undo.")
  a$redo <- ws_trim(c(list(a$df), a$redo))
  a$df <- a$undo[[1]]; a$undo <- a$undo[-1]
  store$sheets[[a$id]] <- a
  store
}

ws_redo <- function(store) {
  a <- ws_active(store); if (is.null(a) || !length(a$redo)) stop("Nothing to redo.")
  a$undo <- ws_trim(c(list(a$df), a$undo))
  a$df <- a$redo[[1]]; a$redo <- a$redo[-1]
  store$sheets[[a$id]] <- a
  store
}

ws_select <- function(store, id) { if (!(id %in% names(store$sheets))) stop("No such worksheet."); store$active <- id; store }

ws_rename <- function(store, name) {
  a <- ws_active(store); name <- trimws(name)
  if (!nzchar(name)) stop("Enter a name.")
  others <- store; others$sheets[[a$id]] <- NULL
  if (name %in% vapply(others$sheets, `[[`, "", "name")) stop("Another worksheet already has that name.")
  store$sheets[[a$id]]$name <- name
  store
}

ws_duplicate <- function(store) {
  a <- ws_active(store)
  ws_add(store, a$df, paste(a$name, "copy"))
}

ws_close <- function(store) {
  if (length(store$sheets) <= 1) stop("The last worksheet cannot be closed.")
  ids <- names(store$sheets); k <- match(store$active, ids)
  store$sheets[[store$active]] <- NULL
  store$active <- names(store$sheets)[min(k, length(store$sheets))]
  store
}

ws_choices <- function(store) stats::setNames(names(store$sheets), vapply(store$sheets, function(s)
  sprintf("%s (%d x %d)", s$name, nrow(s$df), ncol(s$df)), ""))

# For project files: names and data only (no undo history).
ws_export <- function(store) list(active = ws_active(store)$name,
                                  sheets = lapply(unname(store$sheets), function(s) list(name = s$name, df = s$df)))
ws_import <- function(x) {
  st <- ws_store()
  for (s in x$sheets) st <- ws_add(st, s$df, s$name)
  act <- which(vapply(st$sheets, `[[`, "", "name") == (x$active %||% ""))
  if (length(act)) st$active <- names(st$sheets)[act[1]]
  st
}

# ---- Combining worksheets ----------------------------------------------------------
dt_append <- function(df, other) {
  for (n in setdiff(names(other), names(df))) df[[n]] <- rep(NA, nrow(df))
  for (n in setdiff(names(df), names(other))) other[[n]] <- rep(NA, nrow(other))
  other <- other[, names(df), drop = FALSE]
  for (n in names(df)) if (is.numeric(df[[n]]) != is.numeric(other[[n]]) && !all(is.na(other[[n]])) && !all(is.na(df[[n]]))) {
    df[[n]] <- as.character(df[[n]]); other[[n]] <- as.character(other[[n]])
  }
  out <- rbind(df, other); rownames(out) <- NULL
  out
}

dt_join <- function(df, other, key, other_key = key, type = c("left", "inner", "full")) {
  type <- match.arg(type)
  if (!(key %in% names(df))) stop("Column '", key, "' is not in this worksheet.")
  if (!(other_key %in% names(other))) stop("Column '", other_key, "' is not in the other worksheet.")
  if (anyDuplicated(stats::na.omit(other[[other_key]]))) stop("The key column in the other worksheet has repeated values; each key must appear once.")
  df$.order <- seq_len(nrow(df))
  out <- merge(df, other, by.x = key, by.y = other_key, all.x = type %in% c("left", "full"), all.y = type == "full",
               sort = FALSE, suffixes = c("", ".2"))
  out <- out[order(out$.order, na.last = TRUE), setdiff(names(out), ".order"), drop = FALSE]
  rownames(out) <- NULL
  out
}
