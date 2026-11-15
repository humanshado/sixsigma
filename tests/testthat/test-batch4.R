library(ggplot2)
`%||%` <- function(a, b) if (is.null(a)) b else a
for (f in c("worksheets.R", "templates.R")) source(file.path("..", "..", "R", f), local = TRUE)

test_that("loading data replaces an untouched worksheet but never overwrites work", {
  s <- ws_load(ws_store(), mtcars[1:3, 1:3], "cars")
  s <- ws_load(s, iris[1:2, ], "iris")
  expect_length(s$sheets, 1)
  expect_equal(ws_active(s)$name, "iris")
  s <- ws_change(s, iris[1, ])
  s <- ws_load(s, mtcars, "cars")
  expect_length(s$sheets, 2)
  expect_equal(nrow(s$sheets$ws1$df), 1)       # edited sheet kept
  expect_equal(ws_active(s)$name, "cars")
})

test_that("undo and redo walk the history; a new change clears redo", {
  s <- ws_add(ws_store(), data.frame(x = 1), "A")
  s <- ws_change(s, data.frame(x = 2)); s <- ws_change(s, data.frame(x = 3))
  s <- ws_undo(s); expect_equal(ws_active(s)$df$x, 2)
  s <- ws_undo(s); expect_equal(ws_active(s)$df$x, 1)
  expect_error(ws_undo(s), "Nothing to undo")
  s <- ws_redo(s); expect_equal(ws_active(s)$df$x, 2)
  s <- ws_change(s, data.frame(x = 9))
  expect_error(ws_redo(s), "Nothing to redo")
  for (i in 1:40) s <- ws_change(s, data.frame(x = i))
  expect_length(ws_active(s)$undo, WS_MAX_UNDO)
})

test_that("histories are kept per worksheet", {
  s <- ws_add(ws_store(), data.frame(x = 1), "A"); s <- ws_change(s, data.frame(x = 2))
  s <- ws_add(s, data.frame(y = 1), "B")
  expect_error(ws_undo(s), "Nothing to undo")
  s <- ws_select(s, "ws1"); s <- ws_undo(s)
  expect_equal(ws_active(s)$df$x, 1)
})

test_that("names stay unique; rename, duplicate and close behave", {
  s <- ws_add(ws_store(), data.frame(x = 1), "Data")
  s <- ws_add(s, data.frame(x = 2), "Data")
  expect_equal(ws_active(s)$name, "Data (2)")
  expect_error(ws_rename(s, "Data"), "already has that name")
  s <- ws_rename(s, "Second"); s <- ws_duplicate(s)
  expect_equal(ws_active(s)$name, "Second copy")
  s <- ws_close(s); expect_equal(ws_active(s)$name, "Second")
  s <- ws_close(s); expect_error(ws_close(s), "last worksheet")
  expect_equal(names(ws_choices(s)), "Data (1 x 1)")
})

test_that("project export/import keeps names, data and the active sheet", {
  s <- ws_add(ws_store(), data.frame(x = 1:3), "A"); s <- ws_add(s, data.frame(y = "q"), "B"); s <- ws_select(s, "ws1")
  s2 <- ws_import(ws_export(s))
  expect_equal(unname(vapply(s2$sheets, `[[`, "", "name")), c("A", "B"))
  expect_equal(ws_active(s2)$name, "A")
  expect_equal(s2$sheets[[2]]$df, data.frame(y = "q"))
})

test_that("append matches columns by name; join keeps row order and checks keys", {
  a <- data.frame(id = c(1, 2, 3), x = c(5, 6, 7))
  ap <- dt_append(a, data.frame(x = 8, z = "new"))
  expect_equal(dim(ap), c(4, 3)); expect_true(is.na(ap$id[4])); expect_equal(ap$z[4], "new")
  b <- data.frame(id = c(3, 1), y = c("c", "a"))
  j <- dt_join(a, b, "id")
  expect_equal(j$y, c("a", NA, "c"))
  expect_equal(nrow(dt_join(a, b, "id", type = "inner")), 2)
  expect_equal(nrow(dt_join(a, data.frame(key = c(4, 1), w = 1:2), "id", "key", "full")), 4)
  expect_error(dt_join(a, data.frame(id = c(1, 1), y = 1:2), "id"), "repeated values")
})

test_that("value stream metrics: takt, lead time and process cycle efficiency", {
  m <- vsm_metrics(vsm_example(), demand = 460, avail = 27600)
  expect_equal(m$takt, 60)
  expect_equal(m$va, 242)
  expect_equal(m$lead_days, 5600 / 460 + 242 / 27600)
  expect_equal(m$pce, 242 / (m$lead_days * 27600))
  expect_equal(m$table$Bottleneck[4], "Bottleneck")
  expect_equal(m$table$`Meets takt`, c("Yes", "No", "Yes", "No", "Yes"))
  expect_error(vsm_metrics(vsm_example(), 0, 100), "demand")
  expect_s3_class(vsm_plot(m), "ggplot"); expect_s3_class(vsm_balance_plot(m), "ggplot")
})

test_that("template diagrams build and report empty input clearly", {
  expect_s3_class(sipoc_plot(sipoc_example()), "ggplot")
  expect_error(sipoc_plot(tpl_blank(sipoc_cols, 2)), "Fill in")
  expect_s3_class(whys_plot("P", c("a", "", "b"), "root"), "ggplot")
  expect_error(whys_plot("", "a"), "problem")
  expect_s3_class(charter_timeline_plot(charter_milestones_example()), "ggplot")
  expect_error(charter_timeline_plot(data.frame(Phase = "Define", Planned = "soon", Actual = "")), "YYYY-MM-DD")
})

test_that("control plan completeness check lists missing items", {
  expect_equal(nrow(control_plan_check(control_plan_example())), 0)
  cp <- control_plan_example(); cp$`Reaction plan`[3] <- ""; cp$Owner[3] <- NA
  chk <- control_plan_check(cp)
  expect_equal(chk$Row, 3); expect_equal(chk$Missing, "Reaction plan, Owner")
  expect_equal(nrow(control_plan_check(rbind(control_plan_example(), tpl_blank(control_plan_cols, 1)))), 0)  # blank rows ignored
})

test_that("undo history is capped by memory for large worksheets", {
  big <- data.frame(x = runif(5e6))   # about 40 MB
  s <- ws_add(ws_store(), big, "big")
  for (i in 1:12) s <- ws_change(s, big)
  n <- length(ws_active(s)$undo)
  expect_lt(n, 12); expect_gte(n, 1)
  expect_lte(sum(vapply(ws_active(s)$undo, function(d) as.numeric(object.size(d)), 0)), WS_MAX_UNDO_BYTES)
})
