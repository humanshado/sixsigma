# Quality Tools > Loss Function, Cause-and-Effect Diagram, Yield.

# ---- Taguchi loss function (SixSigma::ss.lfa) --------------------------------

lfaUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Loss function analysis",
    shiny::tagList(
      column_picker(ns("var"), "Characteristic (CTQ)"),
      shiny::radioButtons(ns("kind"), "Type",
                          c("Nominal is best" = "nominal", "Smaller is better" = "smaller",
                            "Larger is better" = "larger")),
      shiny::conditionalPanel("input.kind == 'nominal'", ns = ns,
                              blank_numeric(ns("target"), "Target (Y0)")),
      blank_numeric(ns("delta"), "Tolerance (distance from target to the limit)", min = 0),
      blank_numeric(ns("l0"), "Cost of poor quality at the tolerance limit (L0)", min = 0),
      blank_numeric(ns("size"), "Production size for total loss (optional)", min = 0),
      shiny::textInput(ns("subtitle"), "Report subtitle", "Six Sigma Project"),
      shiny::downloadButton(ns("download"), "Download report image")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Results"), shiny::tableOutput(ns("results"))),
      bslib::card(shiny::plotOutput(ns("plot"), height = 560))
    )
  )
}

lfaServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      update_pickers(session, data(), list(var = list(type = "numeric", default = 1)))
      if (identical(names(data()), "diameter")) {
        shiny::updateNumericInput(session, "target", value = 10)
        shiny::updateNumericInput(session, "delta", value = 0.5)
        shiny::updateNumericInput(session, "l0", value = 0.001)
        shiny::updateNumericInput(session, "size", value = 1e5)
      }
    })

    call_lfa <- function(output_type) {
      df <- data()
      shiny::req(input$var %in% names(df))
      y0 <- switch(input$kind, nominal = num_or_na(input$target), smaller = 0, larger = Inf)
      delta <- num_or_na(input$delta); l0 <- num_or_na(input$l0)
      shiny::validate(
        shiny::need(!is.na(y0), "Enter the target value."),
        shiny::need(!is.na(delta) && delta > 0, "Enter a positive tolerance."),
        shiny::need(!is.na(l0) && l0 >= 0, "Enter the cost at the tolerance limit.")
      )
      d <- df[!is.na(df[[input$var]]), , drop = FALSE]
      SixSigma::ss.lfa(d, input$var, delta, y0, l0, lfa.size = num_or_na(input$size),
                       lfa.output = output_type, lfa.sub = input$subtitle)
    }

    output$results <- shiny::renderTable({
      r <- validate_run(call_lfa("text"))
      data.frame(Quantity = c("Loss coefficient k", "Mean squared deviation (MSD)",
                              "Average loss per unit", "Total loss for production size"),
                 Value = c(r$lfa.k, r$lfa.MSD, r$lfa.avLoss, r$lfa.Loss))
    }, digits = 6, na = "")
    output$plot <- shiny::renderPlot(validate_run(invisible(capture_text(call_lfa("plot")))))
    output$download <- plot_download("loss-function", function() invisible(capture_text(call_lfa("plot"))))
  })
}

# ---- Cause-and-effect (Ishikawa) diagram (SixSigma::ss.ceDiag) ---------------

ceUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Cause-and-effect diagram",
    shiny::tagList(
      shiny::textInput(ns("effect"), "Effect (problem)", "Flight Time"),
      shiny::textAreaInput(
        ns("causes"), "Causes: one group per line as  Group: cause, cause, ...", rows = 10,
        value = paste(
          "Operator: operator #1, operator #2, operator #3",
          "Environment: height, cleaning",
          "Tools: scissors, tape",
          "Design: rotor length, rotor width, paperclip",
          "Raw material: thickness, marks",
          "Measure tool: calibrate, model",
          sep = "\n")
      ),
      shiny::textInput(ns("subtitle"), "Subtitle", "Paper Helicopter Project"),
      shiny::downloadButton(ns("download"), "Download diagram")
    ),
    shiny::tagList(
      bslib::card(shiny::plotOutput(ns("plot"), height = 560))
    )
  )
}

parse_causes <- function(txt) {
  lines <- trimws(strsplit(txt, "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines)]
  if (!length(lines)) stop("Enter at least one group of causes.")
  if (!all(grepl(":", lines, fixed = TRUE))) stop("Each line needs a group name followed by a colon.")
  groups <- trimws(sub(":.*$", "", lines))
  causes <- lapply(sub("^[^:]*:", "", lines), function(s) {
    v <- trimws(strsplit(s, ",", fixed = TRUE)[[1]])
    v[nzchar(v)]
  })
  if (any(lengths(causes) == 0)) stop("Every group needs at least one cause.")
  list(groups = groups, causes = causes)
}

ceServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    draw <- function() {
      p <- parse_causes(input$causes)
      shiny::validate(shiny::need(nzchar(input$effect), "Enter the effect."))
      SixSigma::ss.ceDiag(input$effect, p$groups, p$causes, sub = input$subtitle)
    }
    output$plot <- shiny::renderPlot(validate_run(invisible(draw())))
    output$download <- plot_download("cause-effect", draw)
  })
}

# ---- Yield calculator (SixSigma::ss.ca.yield) --------------------------------

yieldUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Process yield",
    shiny::tagList(
      shiny::textInput(ns("defects"), "Defects per process step (comma separated)", "3, 5, 12"),
      shiny::textInput(ns("rework"), "Rework per process step (comma separated)", "1, 2, 4"),
      shiny::numericInput(ns("opps"), "Units (opportunities) processed", 1915, min = 1)
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Yield metrics"), shiny::tableOutput(ns("yield")),
                  shiny::helpText("Yield ignores rework; FTY (first time yield) counts rework as a loss; ",
                                  "RTY (rolled throughput yield) multiplies the yield of each step."))
    )
  )
}

parse_numbers <- function(s) {
  v <- suppressWarnings(as.numeric(trimws(strsplit(s, ",", fixed = TRUE)[[1]])))
  if (!length(v) || any(is.na(v)) || any(v < 0)) stop("Enter non-negative numbers separated by commas.")
  v
}

yieldServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    output$yield <- shiny::renderTable({
      d <- validate_run(parse_numbers(input$defects))
      r <- validate_run(parse_numbers(input$rework))
      shiny::validate(
        shiny::need(length(d) == length(r), "Enter the same number of defect and rework values."),
        shiny::need(isTRUE(input$opps >= 1), "Enter the number of units processed."),
        shiny::need(sum(d) + sum(r) <= input$opps, "Defects plus rework exceed the units processed.")
      )
      y <- SixSigma::ss.ca.yield(d, r, input$opps)
      data.frame(Metric = c("Yield", "First time yield (FTY)", "Rolled throughput yield (RTY)",
                            "Total defects", "Defects per million opportunities (DPMO)",
                            "Approx. sigma level (with 1.5 shift)"),
                 Value = c(sprintf("%.2f%%", 100 * y$Yield), sprintf("%.2f%%", 100 * y$FTY),
                           sprintf("%.2f%%", 100 * y$RTY), format(y$DPU),
                           format(round(y$DPMO, 1), big.mark = ","),
                           if (y$DPMO > 0) sprintf("%.2f", stats::qnorm(1 - y$DPMO / 1e6) + 1.5) else "> 6"))
    })
  })
}
