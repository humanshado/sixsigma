# DMAIC templates: project charter, SIPOC, 5 Whys, control plan, value
# stream map and A3 report. Tables are plain data frames; the plots use ggplot2.

tpl_blank <- function(cols, n = 1, numeric = character(0)) {
  as.data.frame(stats::setNames(lapply(cols, function(c) if (c %in% numeric) rep(NA_real_, n) else rep("", n)), cols),
                check.names = FALSE, stringsAsFactors = FALSE)
}

# ---- Project charter ----------------------------------------------------------------
charter_text_example <- list(
  title = "Reduce shaft diameter rework on Line 3", leader = "A. Rahman (Black Belt)", sponsor = "Plant manager",
  start = "2026-01-12",
  business = "Rework on Line 3 shafts cost about $310,000 last year and delays customer shipments.",
  problem = "Since March, 6.2% of shafts from Line 3 are reworked because the diameter is out of specification (10.00 ± 0.05 mm), against a 1% target.",
  goal = "Reduce shaft rework on Line 3 from 6.2% to below 1% by 30 June, without adding inspection.",
  scope_in = "Line 3 turning, grinding and final inspection; all shaft part numbers.",
  scope_out = "Other lines; supplier bar stock quality (handled by purchasing).",
  benefits = "About $260,000 per year in rework and overtime; better on-time delivery.")
charter_team_example <- function() data.frame(Role = c("Sponsor", "Black Belt", "Process owner", "Team member", "Team member"),
  Name = c("Plant manager", "A. Rahman", "Line 3 supervisor", "Setter, shift A", "Quality engineer"),
  Responsibility = c("Remove barriers, approve resources", "Lead the project", "Implement and sustain changes",
                     "Process knowledge, trials", "Measurement system, data"), stringsAsFactors = FALSE)
charter_metrics_example <- function() data.frame(Metric = c("Rework rate (%)", "Diameter Ppk", "Cost of rework ($/month)"),
  Baseline = c(6.2, 0.62, 26000), Target = c(1, 1.33, 4000), stringsAsFactors = FALSE)
# Example dates are relative to today so the timeline shows a project under way.
charter_milestones_example <- function(today = Sys.Date()) data.frame(Phase = c("Define", "Measure", "Analyze", "Improve", "Control"),
  Planned = format(today + c(-50, -20, 15, 60, 110)), Actual = c(format(today + c(-52, -16)), "", "", ""), stringsAsFactors = FALSE)

parse_date_safe <- function(x) { d <- suppressWarnings(as.Date(as.character(x), optional = TRUE)); d }

charter_timeline_plot <- function(ms, start = NA) {
  ms <- ms[nzchar(trimws(ms$Phase %||% "")), , drop = FALSE]
  pl <- parse_date_safe(ms$Planned); ac <- parse_date_safe(ms$Actual)
  if (!nrow(ms) || all(is.na(pl))) stop("Enter planned dates as YYYY-MM-DD to see the timeline.")
  st <- parse_date_safe(start)
  begin <- c(if (!is.na(st)) st else min(pl, na.rm = TRUE) - 28, utils::head(pl, -1))
  d <- data.frame(Phase = factor(ms$Phase, levels = rev(ms$Phase)), from = begin, to = pl,
                  status = ifelse(!is.na(ac), ifelse(ac <= pl, "Done on time", "Done late"),
                                  ifelse(pl < Sys.Date(), "Overdue", "Open")))
  ggplot2::ggplot(d) +
    ggplot2::geom_segment(ggplot2::aes(x = from, xend = to, y = Phase, yend = Phase, colour = status), linewidth = 9) +
    ggplot2::geom_vline(xintercept = Sys.Date(), linetype = "dashed", colour = "grey40") +
    ggplot2::scale_colour_manual(values = c("Done on time" = "#18BC9C", "Done late" = "#F39C12", "Overdue" = "#E74C3C", "Open" = "#95A5A6"),
                                 drop = FALSE) +
    ggplot2::labs(title = "Project timeline (dashed line = today)", x = NULL, y = NULL, colour = NULL) +
    ggplot2::theme_minimal(base_size = 13) + ggplot2::theme(legend.position = "bottom")
}

# ---- SIPOC -------------------------------------------------------------------------
sipoc_cols <- c("Suppliers", "Inputs", "Process", "Outputs", "Customers")
sipoc_example <- function() data.frame(
  Suppliers = c("Steel mill", "Tooling vendor", "Maintenance", "Production planning", ""),
  Inputs = c("Bar stock", "Inserts and wheels", "Calibrated machines", "Work orders", "Setup sheet"),
  Process = c("Cut bar to length", "Rough turn", "Heat treat", "Grind diameter", "Inspect and pack"),
  Outputs = c("Finished shafts", "Inspection records", "Scrap and rework", "", ""),
  Customers = c("Assembly line", "Quality department", "Finance", "", ""), stringsAsFactors = FALSE)

sipoc_plot <- function(d) {
  items <- do.call(rbind, lapply(seq_along(sipoc_cols), function(j) {
    v <- trimws(as.character(d[[sipoc_cols[j]]] %||% character(0))); v <- v[!is.na(v) & nzchar(v)]
    if (!length(v)) return(NULL)
    data.frame(col = j, row = seq_along(v), label = v)
  }))
  if (is.null(items)) stop("Fill in at least one cell of the SIPOC table.")
  wrap <- function(s) vapply(s, function(x) paste(strwrap(x, 18), collapse = "\n"), "")
  items$label <- wrap(items$label)
  heads <- data.frame(col = seq_along(sipoc_cols), label = sipoc_cols)
  proc <- items[items$col == 3, ]
  p <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = heads, ggplot2::aes(col, 0, fill = I("#2C3E50")), width = 0.92, height = 0.7) +
    ggplot2::geom_text(data = heads, ggplot2::aes(col, 0, label = label), colour = "white", fontface = "bold", size = 4.5) +
    ggplot2::geom_tile(data = items, ggplot2::aes(col, -row, fill = I(ifelse(col == 3, "#D1F2EB", "#EBF5FB"))),
                       colour = "#7F8C8D", width = 0.9, height = 0.8) +
    ggplot2::geom_text(data = items, ggplot2::aes(col, -row, label = label), size = 3.4, lineheight = 0.9)
  if (nrow(proc) > 1) p <- p + ggplot2::geom_segment(data = proc[-nrow(proc), ], ggplot2::aes(x = 3, xend = 3, y = -row - 0.4, yend = -row - 0.6),
                                                    arrow = grid::arrow(length = grid::unit(0.15, "cm")), colour = "#16A085")
  p + ggplot2::scale_x_continuous(limits = c(0.5, 5.5)) + ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(title = "SIPOC") + ggplot2::theme_void(base_size = 13)
}

# ---- 5 Whys -------------------------------------------------------------------------
whys_example <- list(problem = "Shaft diameters drift above the upper specification limit during the afternoon shift.",
  whys = c("The grinding wheel cuts less material as the shift goes on.",
           "The wheel is dressed only at the start of each shift.",
           "The dressing interval was set for the old, softer steel grade.",
           "The setup sheet was not updated when the steel grade changed.",
           "Material changes do not trigger a review of machine settings."),
  root = "No change-control link between material specification changes and machine setup sheets.",
  action = "Add setup-sheet review to the material change procedure; dress the wheel every 2 hours; add an X-bar chart at grinding.")

whys_plot <- function(problem, whys, root = "") {
  whys <- trimws(whys); whys <- whys[nzchar(whys)]
  if (!nzchar(trimws(problem)) || !length(whys)) stop("Enter the problem and at least one answer.")
  lab <- c(paste("Problem:", problem), sprintf("Why %d: %s", seq_along(whys), whys), if (nzchar(trimws(root))) paste("Root cause:", root))
  type <- c("Problem", rep("Why", length(whys)), if (nzchar(trimws(root))) "Root cause")
  d <- data.frame(y = -seq_along(lab), label = vapply(lab, function(x) paste(strwrap(x, 70), collapse = "\n"), ""), type = type)
  ggplot2::ggplot(d, ggplot2::aes(0, y)) +
    ggplot2::geom_segment(data = d[-nrow(d), ], ggplot2::aes(x = 0, xend = 0, y = y - 0.32, yend = y - 0.68),
                          arrow = grid::arrow(length = grid::unit(0.18, "cm")), colour = "#7F8C8D") +
    ggplot2::geom_label(ggplot2::aes(label = label, fill = type), size = 3.6, label.padding = grid::unit(0.45, "lines"),
                        lineheight = 0.95, hjust = 0.5) +
    ggplot2::scale_fill_manual(values = c(Problem = "#FADBD8", Why = "#EBF5FB", `Root cause` = "#D5F5E3"), guide = "none") +
    ggplot2::scale_x_continuous(limits = c(-1, 1)) + ggplot2::labs(title = "5 Whys") + ggplot2::theme_void(base_size = 13)
}

# ---- Control plan ---------------------------------------------------------------------
control_plan_cols <- c("Process step", "Characteristic", "Type", "Specification", "Measurement method",
                       "Sample size", "Frequency", "Control method", "Reaction plan", "Owner")
control_plan_example <- function() data.frame(
  `Process step` = c("Grind diameter", "Grind diameter", "Heat treat", "Final inspection"),
  Characteristic = c("Shaft diameter", "Wheel dressing interval", "Surface hardness", "Visual defects"),
  Type = c("Product", "Process", "Product", "Product"),
  Specification = c("10.00 ± 0.05 mm", "Every 2 h", "58-62 HRC", "No burrs or scratches"),
  `Measurement method` = c("Air gauge (GR&R 8%)", "Machine log", "Rockwell tester", "Visual, 5x lens"),
  `Sample size` = c("5", "1", "3", "100%"), Frequency = c("Every hour", "Every 2 h", "Each batch", "Every part"),
  `Control method` = c("X-bar and R chart", "Checklist", "I-MR chart", "Attribute check sheet"),
  `Reaction plan` = c("Stop, adjust offset, check last 20 parts", "Dress wheel, record", "Quarantine batch, notify QE", "Segregate, rework"),
  Owner = c("Operator", "Setter", "Heat treat operator", "Inspector"), check.names = FALSE, stringsAsFactors = FALSE)

control_plan_check <- function(d) {
  d <- d[apply(d, 1, function(r) any(nzchar(trimws(r)) & !is.na(r))), , drop = FALSE]
  need <- c("Specification", "Measurement method", "Frequency", "Control method", "Reaction plan", "Owner")
  miss <- lapply(seq_len(nrow(d)), function(i) need[!nzchar(trimws(ifelse(is.na(unlist(d[i, need])), "", unlist(d[i, need]))))])
  bad <- which(lengths(miss) > 0)
  data.frame(Row = bad, Characteristic = d$Characteristic[bad], Missing = vapply(miss[bad], paste, "", collapse = ", "))
}

# ---- Value stream map ----------------------------------------------------------------
vsm_example <- function() data.frame(Process = c("Cutting", "Turning", "Heat treat", "Grinding", "Inspection"),
  `Cycle time (s)` = c(35, 62, 40, 75, 30), `Changeover (min)` = c(10, 45, 0, 30, 0), `Uptime (%)` = c(95, 85, 98, 80, 100),
  Operators = c(1, 1, 1, 2, 1), `Inventory before (units)` = c(2400, 900, 1200, 800, 300),
  check.names = FALSE, stringsAsFactors = FALSE)

vsm_metrics <- function(steps, demand, avail) {
  if (!isTRUE(demand > 0) || !isTRUE(avail > 0)) stop("Enter the customer demand and the available working time per day.")
  ct <- as.numeric(steps$`Cycle time (s)`); inv <- as.numeric(steps$`Inventory before (units)`)
  ok <- nzchar(trimws(steps$Process)) & !is.na(ct)
  if (!any(ok)) stop("Enter at least one process with a cycle time.")
  steps <- steps[ok, , drop = FALSE]; ct <- ct[ok]; inv <- ifelse(is.na(inv[ok]), 0, inv[ok])
  takt <- avail / demand
  inv_days <- inv / demand
  va <- sum(ct)
  lead_days <- sum(inv_days) + va / avail
  tab <- data.frame(Process = steps$Process, `Cycle time (s)` = ct, `Takt time (s)` = takt,
                    `Load vs takt (%)` = 100 * ct / takt, `Inventory (days)` = inv_days, check.names = FALSE)
  tab$Bottleneck <- ifelse(ct == max(ct), "Bottleneck", "")
  tab$`Meets takt` <- ifelse(ct <= takt, "Yes", "No")
  summary <- data.frame(Measure = c("Takt time (s)", "Value-added time (s)", "Production lead time (days)",
                                    "Process cycle efficiency (%)", "Total inventory (units)", "Bottleneck step"),
                        Value = c(format(signif(takt, 4)), format(va), format(signif(lead_days, 4)),
                                  format(signif(100 * va / (lead_days * avail), 3)), format(sum(inv)),
                                  steps$Process[which.max(ct)]))
  list(takt = takt, va = va, lead_days = lead_days, pce = va / (lead_days * avail), table = tab, summary = summary,
       steps = steps, inv_days = inv_days)
}

vsm_plot <- function(m, supplier = "Supplier", customer = "Customer", demand = NA) {
  s <- m$steps; n <- nrow(s); x <- seq_len(n) * 2
  box <- data.frame(x = x, label = s$Process)
  info <- data.frame(x = x, label = sprintf("C/T %gs\nC/O %smin\nUptime %s%%\nOps %s", s$`Cycle time (s)`,
                     ifelse(is.na(s$`Changeover (min)`), "-", s$`Changeover (min)`),
                     ifelse(is.na(s$`Uptime (%)`), "-", s$`Uptime (%)`), ifelse(is.na(s$Operators), "-", s$Operators)))
  tri <- data.frame(x = x - 1, label = sprintf("%g\n%.1f d", as.numeric(ifelse(is.na(s$`Inventory before (units)`), 0, s$`Inventory before (units)`)), m$inv_days))
  # Timeline ladder: inventory days high, cycle times low.
  lad <- do.call(rbind, lapply(seq_len(n), function(i) rbind(
    data.frame(x1 = x[i] - 1.5, x2 = x[i] - 0.5, y = -4.2, label = sprintf("%.1f d", m$inv_days[i]), lvl = "hi"),
    data.frame(x1 = x[i] - 0.5, x2 = x[i] + 0.5, y = -4.8, label = sprintf("%gs", s$`Cycle time (s)`[i]), lvl = "lo"))))
  ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = 0.2, xmax = 1.6, ymin = 2, ymax = 2.8, fill = "#FDEBD0", colour = "#7F8C8D") +
    ggplot2::annotate("text", x = 0.9, y = 2.4, label = supplier, size = 3.6) +
    ggplot2::annotate("rect", xmin = 2 * n + 0.4, xmax = 2 * n + 1.8, ymin = 2, ymax = 2.8, fill = "#FDEBD0", colour = "#7F8C8D") +
    ggplot2::annotate("text", x = 2 * n + 1.1, y = 2.4, label = sprintf("%s\n%s/day", customer, if (is.na(demand)) "?" else format(demand)), size = 3.4) +
    ggplot2::geom_tile(data = box, ggplot2::aes(x, 0.5), width = 1.3, height = 0.8, fill = "#D6EAF8", colour = "#2C3E50") +
    ggplot2::geom_text(data = box, ggplot2::aes(x, 0.5, label = label), fontface = "bold", size = 3.6) +
    ggplot2::geom_tile(data = info, ggplot2::aes(x, -1.3), width = 1.3, height = 2, fill = "white", colour = "#7F8C8D") +
    ggplot2::geom_text(data = info, ggplot2::aes(x, -1.3, label = label), size = 3, lineheight = 0.95) +
    ggplot2::geom_point(data = tri, ggplot2::aes(x, 0.6), shape = 24, size = 9, fill = "#F9E79F", colour = "#B7950B") +
    ggplot2::geom_text(data = tri, ggplot2::aes(x, -0.25, label = label), size = 2.9, lineheight = 0.9) +
    ggplot2::geom_segment(data = lad, ggplot2::aes(x = x1, xend = x2, y = y, yend = y), linewidth = 1, colour = "#2C3E50") +
    ggplot2::geom_text(data = lad, ggplot2::aes(x = (x1 + x2) / 2, y = y + ifelse(lvl == "hi", 0.25, -0.25), label = label), size = 3) +
    ggplot2::annotate("label", x = 2 * n + 1.1, y = -4.5, size = 3.3,
                      label = sprintf("Lead time %.1f d\nVA time %gs\nPCE %.3f%%\nTakt %.0fs", m$lead_days, m$va, 100 * m$pce, m$takt)) +
    ggplot2::coord_cartesian(xlim = c(0, 2 * n + 2), ylim = c(-5.4, 3), clip = "off") +
    ggplot2::labs(title = "Current-state value stream map") + ggplot2::theme_void(base_size = 13)
}

vsm_balance_plot <- function(m) {
  d <- m$table; d$Process <- factor(d$Process, levels = d$Process)
  ggplot2::ggplot(d, ggplot2::aes(Process, `Cycle time (s)`, fill = `Meets takt`)) + ggplot2::geom_col() +
    ggplot2::geom_hline(yintercept = m$takt, linetype = "dashed", colour = "#C0392B", linewidth = 1) +
    ggplot2::annotate("text", x = 0.6, y = m$takt, label = sprintf("Takt %.0fs", m$takt), vjust = -0.5, hjust = 0, colour = "#C0392B") +
    ggplot2::scale_fill_manual(values = c(Yes = "#4C78A8", No = "#E74C3C")) +
    ggplot2::labs(title = "Cycle time vs takt time (line balance)", x = NULL) + ggplot2::theme_minimal(base_size = 13)
}

# ---- A3 report ---------------------------------------------------------------------------
a3_plan_example <- function() data.frame(Action = c("Update material change procedure", "Dress wheel every 2 hours", "X-bar chart at grinding", "Train setters"),
  Owner = c("Quality engineer", "Setter", "Black Belt", "Line 3 supervisor"), Due = c("2026-04-15", "2026-04-01", "2026-04-08", "2026-04-30"),
  Status = c("Done", "Done", "In progress", "Open"), stringsAsFactors = FALSE)
