# Quality Tools > Capability Analysis (SixSigma::ss.study.ca and indices).

capabilityUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Process capability",
    shiny::tagList(
      column_picker(ns("var"), "Short-term data"),
      column_picker(ns("lt"), "Long-term data (optional)"),
      blank_numeric(ns("lsl"), "Lower spec limit (LSL)"),
      blank_numeric(ns("usl"), "Upper spec limit (USL)"),
      blank_numeric(ns("target"), "Target (blank = midpoint of specs)"),
      shiny::numericInput(ns("conf"), "Confidence level (%)", 95, min = 50, max = 99.9, step = 0.5),
      shiny::textInput(ns("subtitle"), "Report subtitle", ""),
      shiny::downloadButton(ns("download"), "Download report image"),
      shiny::helpText("At least one specification limit is required.")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Capability indices"), shiny::tableOutput(ns("indices")),
                  shiny::helpText("Cp/Cpk use the overall standard deviation, as in SixSigma. ",
                                  "Values of 1.33 or more are commonly considered capable.")),
      bslib::card(bslib::card_header("Six Sigma capability study (SixSigma::ss.study.ca)"),
                  shiny::plotOutput(ns("study"), height = 620))
    )
  )
}

capability_indices <- function(x, lsl, usl, alpha) {
  x <- x[!is.na(x)]
  cp <- if (!is.na(lsl) && !is.na(usl)) SixSigma::ss.ca.cp(x, lsl, usl) else NA
  cp_ci <- if (!is.na(cp)) SixSigma::ss.ca.cp(x, lsl, usl, ci = TRUE, alpha = alpha) else c(NA, NA)
  cpk <- SixSigma::ss.ca.cpk(x, lsl, usl)
  cpk_ci <- SixSigma::ss.ca.cpk(x, lsl, usl, ci = TRUE, alpha = alpha)
  z <- SixSigma::ss.ca.z(x, lsl, usl)
  m <- mean(x); s <- stats::sd(x)
  below <- if (is.na(lsl)) 0 else stats::pnorm(lsl, m, s)
  above <- if (is.na(usl)) 0 else stats::pnorm(usl, m, s, lower.tail = FALSE)
  data.frame(
    Statistic = c("N", "Mean", "Std. dev.", "Cp", "Cpk", "Z (sigma score)",
                  "Expected PPM below LSL", "Expected PPM above USL", "Expected PPM total",
                  "Observed PPM out of spec"),
    Value = c(length(x), m, s, cp, cpk, z, below * 1e6, above * 1e6, (below + above) * 1e6,
              1e6 * mean((!is.na(lsl) & x < lsl) | (!is.na(usl) & x > usl))),
    `Lower CI` = c(NA, NA, NA, cp_ci[1], cpk_ci[1], NA, NA, NA, NA, NA),
    `Upper CI` = c(NA, NA, NA, cp_ci[2], cpk_ci[2], NA, NA, NA, NA, NA),
    check.names = FALSE
  )
}

capabilityServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      update_pickers(session, data(), list(
        var = list(type = "numeric", default = 1),
        lt = list(type = "numeric", optional = TRUE)
      ))
      # Suggest spec limits for the sample capability dataset from the book.
      if (identical(names(data()), "Volume")) {
        shiny::updateNumericInput(session, "lsl", value = 740)
        shiny::updateNumericInput(session, "usl", value = 760)
        shiny::updateNumericInput(session, "target", value = 750)
      }
    })

    params <- shiny::reactive({
      df <- data()
      shiny::req(input$var %in% names(df))
      lsl <- num_or_na(input$lsl); usl <- num_or_na(input$usl)
      shiny::validate(
        shiny::need(!(is.na(lsl) && is.na(usl)), "Enter at least one specification limit."),
        shiny::need(is.na(lsl) || is.na(usl) || lsl < usl, "LSL must be below USL.")
      )
      target <- num_or_na(input$target)
      if (is.na(target)) {
        shiny::validate(shiny::need(!is.na(lsl) && !is.na(usl),
                                    "With a one-sided specification, enter a target value."))
        target <- (lsl + usl) / 2
      }
      lt_col <- empty_to_null(input$lt)
      x <- df[[input$var]]
      shiny::validate(shiny::need(sum(!is.na(x)) >= 3, "At least three values are needed."))
      list(x = x, lt = if (is.null(lt_col)) NA else df[[lt_col]],
           lsl = lsl, usl = usl, target = target, alpha = 1 - input$conf / 100)
    })

    output$indices <- shiny::renderTable({
      p <- params()
      validate_run(capability_indices(p$x, p$lsl, p$usl, p$alpha))
    }, digits = 4, na = "")

    draw <- function() {
      p <- params()
      SixSigma::ss.study.ca(p$x, xLT = p$lt, LSL = p$lsl, USL = p$usl, Target = p$target,
                            alpha = p$alpha, f.sub = input$subtitle)
    }
    output$study <- shiny::renderPlot(validate_run(invisible(capture_text(draw()))))
    output$download <- plot_download("capability", function() invisible(capture_text(draw())))
  })
}
