# Quality Tools > Non-normal capability (percentile method with a fitted
# distribution or a Box-Cox transformation; as in qcr::qcs.pcr).

nonnormalUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Non-normal capability",
    shiny::tagList(
      column_picker(ns("var"), "Data"),
      blank_numeric(ns("lsl"), "Lower spec limit (LSL)"),
      blank_numeric(ns("usl"), "Upper spec limit (USL)"),
      blank_numeric(ns("target"), "Target (optional)"),
      shiny::selectInput(ns("dist"), "Distribution", c("Best fit (lowest AIC)" = "best", nn_distributions)),
      shiny::conditionalPanel("input.dist == 'boxcox' || input.dist == 'best'", ns = ns,
        shiny::checkboxInput(ns("round_lambda"), "Round Box-Cox λ to the nearest 0.5", FALSE)),
      shiny::numericInput(ns("bins"), "Histogram bins", 20, min = 5, max = 100),
      shiny::numericInput(ns("B"), "Simulations for goodness-of-fit p-values", 200, min = 50, max = 2000, step = 50),
      shiny::actionButton(ns("pvals"), "Compute p-values", icon = shiny::icon("calculator")),
      shiny::downloadButton(ns("download"), "Download charts"),
      shiny::helpText("Use this when the data are skewed or otherwise not normal (check the normality tests in ",
                      "Stat > Basic statistics). Capability is computed from the fitted distribution's ",
                      "0.135%, 50% and 99.865% percentiles.")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Distribution fits (best first)"), shiny::tableOutput(ns("compare")),
                  shiny::helpText("Lower AIC and lower Anderson-Darling (AD) statistic mean a better fit. ",
                                  "Distributions that need positive data are skipped when values are zero or negative. ",
                                  "'Compute p-values' simulates the AD test for each fit (parametric bootstrap); ",
                                  "p < 0.05 means the distribution does not fit.")),
      bslib::layout_columns(
        col_widths = c(7, 5),
        bslib::card(shiny::plotOutput(ns("hist"), height = 380)),
        bslib::card(shiny::plotOutput(ns("prob"), height = 380))
      ),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        bslib::card(bslib::card_header(shiny::textOutput(ns("used"), inline = TRUE)), shiny::tableOutput(ns("indices"))),
        bslib::card(bslib::card_header("Fitted percentiles"), shiny::tableOutput(ns("quant"))),
        bslib::card(bslib::card_header("Parts per million out of spec"), shiny::tableOutput(ns("ppm")))
      )
    )
  )
}

nonnormalServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    prev_cols <- shiny::reactiveVal(NULL)
    shiny::observeEvent(data(), {
      update_pickers(session, data(), list(var = list(type = "numeric", default = 1)))
      if (!identical(names(data()), prev_cols())) {
        prev_cols(names(data()))
        clear_numeric(session, c("lsl", "usl", "target"))
      }
    })
    spec <- debounced_inputs(input, c("lsl", "usl", "target"))

    x <- shiny::reactive({
      df <- data()
      shiny::req(input$var %in% names(df))
      v <- df[[input$var]]
      shiny::validate(shiny::need(sum(!is.na(v)) >= 8, "At least 8 values are needed."))
      v[!is.na(v)]
    })
    comparison <- shiny::reactive(validate_run(nn_compare(x(), isTRUE(input$round_lambda))))
    fit <- shiny::reactive({
      d <- if (input$dist == "best") comparison()$Code[1] else input$dist
      validate_run(nn_fit(x(), d, isTRUE(input$round_lambda)))
    })
    specs <- shiny::reactive({
      lsl <- num_or_na(spec$lsl()); usl <- num_or_na(spec$usl())
      shiny::validate(shiny::need(!(is.na(lsl) && is.na(usl)), "Enter at least one specification limit."),
                      shiny::need(is.na(lsl) || is.na(usl) || lsl < usl, "LSL must be below USL."))
      shiny::validate(shiny::need(!specs_far_from_data(x(), lsl, usl), "The specification limits are far outside the data. Check that they belong to this dataset."))
      list(lsl = lsl, usl = usl, target = num_or_na(spec$target()))
    })
    cap <- shiny::reactive({ s <- specs(); validate_run(nn_capability(fit(), s$lsl, s$usl, s$target)) })

    pv <- shiny::reactiveVal(NULL)
    shiny::observeEvent(list(x(), input$round_lambda), pv(NULL))
    shiny::observeEvent(input$pvals, {
      cmp <- comparison(); B <- max(50, min(2000, input$B %||% 200))
      p <- shiny::withProgress(message = "Simulating goodness-of-fit p-values", value = 0, {
        vapply(seq_len(nrow(cmp)), function(i) {
          shiny::incProgress(1 / nrow(cmp), detail = cmp$Distribution[i])
          f <- tryCatch(nn_fit(x(), cmp$Code[i], isTRUE(input$round_lambda)), error = function(e) NULL)
          if (is.null(f)) NA_real_ else nn_ad_pvalue(f, B, isTRUE(input$round_lambda))
        }, numeric(1))
      })
      pv(stats::setNames(p, cmp$Code))
    })
    output$compare <- shiny::renderTable({
      cmp <- comparison()
      out <- cmp[, c("Rank", "Distribution", "AD statistic", "AIC", "Parameters")]
      if (!is.null(pv())) out <- data.frame(out[, 1:3], `AD p-value` = unname(pv()[cmp$Code]), out[, 4:5], check.names = FALSE)
      out
    }, digits = 4)
    output$used <- shiny::renderText(sprintf("Capability (%s)", fit()$label))
    output$indices <- shiny::renderTable(cap()$indices, digits = 4, na = "")
    output$quant <- shiny::renderTable(cap()$quantiles, digits = 5)
    output$ppm <- shiny::renderTable(cap()$ppm, digits = 1)
    plot_hist <- function() { s <- specs(); nn_plot(fit(), s$lsl, s$usl, s$target, input$bins) }
    output$hist <- shiny::renderPlot(plot_hist())
    output$prob <- shiny::renderPlot(nn_probability_plot(fit()))
    output$download <- plot_download("nonnormal-capability", function() draw_grid(list(plot_hist(), nn_probability_plot(fit()))),
                                     width = 14, height = 6)
  })
}
