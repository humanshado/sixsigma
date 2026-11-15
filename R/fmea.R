# Failure mode and effects analysis (FMEA) worksheet.
#   RPN = Severity x Occurrence x Detection (each rated 1-10),
#   Criticality = Severity x Occurrence. "New" ratings are after the action.

fmea_columns <- c("Item", "Function", "Failure mode", "Effect", "S", "Cause", "O", "Current prevention controls",
                  "Current detection controls", "D", "Recommended action", "Responsibility", "Target date",
                  "Action taken", "New S", "New O", "New D")
fmea_ratings <- c("S", "O", "D", "New S", "New O", "New D")

fmea_blank <- function(n = 1) {
  d <- as.data.frame(stats::setNames(replicate(length(fmea_columns), rep(NA_character_, n), simplify = FALSE), fmea_columns),
                     check.names = FALSE, stringsAsFactors = FALSE)
  for (r in fmea_ratings) d[[r]] <- NA_real_
  d
}

fmea_example <- function() {
  d <- fmea_blank(6)
  d$Item <- c("Drill spindle", "Drill spindle", "Coolant system", "Fixture", "Fixture", "Final inspection")
  d$Function <- c("Drill 6 mm hole", "Drill 6 mm hole", "Cool cutting zone", "Locate part", "Clamp part", "Detect oversize holes")
  d$`Failure mode` <- c("Hole oversize", "Hole position off", "Coolant flow low", "Part mislocated", "Clamp loose", "Oversize hole missed")
  d$Effect <- c("Loose fit at assembly", "Assembly impossible", "Tool wear, burrs", "Scrap", "Part moves, scrap", "Customer complaint")
  d$S <- c(7, 8, 5, 8, 8, 9)
  d$Cause <- c("Worn drill", "Fixture wear", "Clogged filter", "Chips on locator", "Low air pressure", "Gauge not calibrated")
  d$O <- c(5, 3, 6, 4, 2, 3)
  d$`Current prevention controls` <- c("Tool change every 500 parts", "Fixture PM quarterly", "Weekly filter check",
                                       "Operator blows chips", "Pressure switch", "Annual calibration")
  d$`Current detection controls` <- c("Go/no-go gauge 1 in 50", "CMM 1 per shift", "Visual", "None", "Pressure gauge", "Audit")
  d$D <- c(6, 4, 7, 8, 3, 6)
  d$`Recommended action` <- c("Tool-life counter with automatic stop", "", "Flow sensor with alarm", "Poka-yoke locator pin",
                              "", "Calibration every 3 months")
  d$Responsibility <- c("Maintenance", "", "Engineering", "Tooling", "", "Quality")
  d$`Target date` <- c("2026-12-15", "", "2027-01-31", "2026-12-31", "", "2026-12-01")
  d$`New S` <- c(7, NA, 5, 8, NA, 9); d$`New O` <- c(2, NA, 3, 2, NA, 3); d$`New D` <- c(3, NA, 3, 2, NA, 3)
  d
}

fmea_clean <- function(d) {
  for (r in fmea_ratings) {
    v <- suppressWarnings(as.numeric(d[[r]]))
    v[!is.na(v) & (v < 1 | v > 10 | v != round(v))] <- NA
    d[[r]] <- v
  }
  d
}

fmea_compute <- function(d, threshold = 100, sev_flag = 9) {
  d <- fmea_clean(d)
  rpn <- d$S * d$O * d$D
  new_rpn <- d$`New S` * d$`New O` * d$`New D`
  pr <- ifelse(is.na(rpn), "Incomplete",
               ifelse(d$S >= sev_flag, "High (severity)", ifelse(rpn >= threshold, "High (RPN)", "Normal")))
  data.frame(Row = seq_len(nrow(d)), Item = d$Item, `Failure mode` = d$`Failure mode`, Cause = d$Cause,
             S = d$S, O = d$O, D = d$D, RPN = rpn, Criticality = d$S * d$O, Priority = pr,
             `Recommended action` = d$`Recommended action`, `New RPN` = new_rpn,
             `RPN reduction (%)` = ifelse(is.na(new_rpn) | is.na(rpn), NA, 100 * (rpn - new_rpn) / rpn),
             check.names = FALSE, stringsAsFactors = FALSE)
}

fmea_label <- function(r) {
  lab <- ifelse(is.na(r$`Failure mode`) | !nzchar(r$`Failure mode`), paste("Row", r$Row), r$`Failure mode`)
  make.unique(ifelse(is.na(r$Cause) | !nzchar(r$Cause), lab, paste0(lab, " (", r$Cause, ")")), sep = " ")
}

fmea_pareto <- function(r, threshold = 100, top = 15) {
  r <- r[!is.na(r$RPN), ]
  if (!nrow(r)) stop("Fill in S, O and D (1-10) for at least one row.")
  r$Label <- fmea_label(r)
  r <- utils::head(r[order(-r$RPN), ], top)
  r$Label <- factor(r$Label, levels = rev(r$Label))
  long <- rbind(data.frame(Label = r$Label, RPN = r$RPN, When = "Current"),
                data.frame(Label = r$Label, RPN = r$`New RPN`, When = "After action"))
  long <- long[!is.na(long$RPN), ]
  long$When <- factor(long$When, levels = c("Current", "After action"))
  ggplot2::ggplot(long, ggplot2::aes(RPN, Label, fill = When)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75) +
    ggplot2::geom_vline(xintercept = threshold, linetype = 2, colour = "#c0392b") +
    ggplot2::scale_fill_manual(values = c(Current = "#4a6fa5", `After action` = "#9fc5a0"), name = NULL) +
    ggplot2::labs(title = "Risk priority numbers (highest first)", subtitle = sprintf("Dashed line: RPN action threshold %g", threshold),
                  x = "RPN = S x O x D", y = NULL) +
    ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), legend.position = "top")
}

fmea_matrix <- function(r) {
  r <- r[!is.na(r$S) & !is.na(r$O), ]
  if (!nrow(r)) stop("Fill in S and O for at least one row.")
  grid <- expand.grid(S = 1:10, O = 1:10)
  grid$zone <- cut(grid$S * grid$O, c(0, 20, 50, 100), labels = c("Low", "Medium", "High"))
  agg <- stats::aggregate(Row ~ S + O, data = r, FUN = length)
  ggplot2::ggplot() +
    ggplot2::geom_tile(data = grid, ggplot2::aes(O, S, fill = zone), colour = "white") +
    ggplot2::scale_fill_manual(values = c(Low = "#d8ecd8", Medium = "#fbe8c0", High = "#f4c7c3"), name = "S x O") +
    ggplot2::geom_point(data = agg, ggplot2::aes(O, S, size = Row), colour = "#2c3e50") +
    ggplot2::geom_text(data = agg, ggplot2::aes(O, S, label = Row), colour = "white", size = 3) +
    ggplot2::scale_size_continuous(range = c(5, 11), guide = "none") +
    ggplot2::scale_x_continuous(breaks = 1:10, expand = c(0, 0)) + ggplot2::scale_y_continuous(breaks = 1:10, expand = c(0, 0)) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "Risk matrix (number of failure modes)", x = "Occurrence", y = "Severity") +
    ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}
