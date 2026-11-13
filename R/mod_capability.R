# Quality Tools > Capability Analysis: SixSigma's study report plus the
# extended indices from R/capability.R (within/overall sigma, Cpm, Cpmk,
# nonparametric indices and Vannman's capability plot, as in qcr).

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
      shiny::selectInput(ns("within"), "Within-subgroup sigma",
                         c("Moving range (individual values)" = "mr",
                           "Pooled std. dev., subgroups of fixed size" = "size",
                           "Pooled std. dev., subgroups from a column" = "col",
                           "Do not estimate (overall only)" = "none")),
      shiny::conditionalPanel("input.within == 'size'", ns = ns,
        shiny::numericInput(ns("sg_size"), "Subgroup size", 5, min = 2)),
      shiny::conditionalPanel("input.within == 'col'", ns = ns,
        column_picker(ns("sg_col"), "Subgroup column")),
      shiny::numericInput(ns("conf"), "Confidence level (%)", 95, min = 50, max = 99.9, step = 0.5),
      shiny::numericInput(ns("k"), "Required capability for the plot (k)", 1.33, min = 0.5, max = 3, step = 0.01),
      shiny::textInput(ns("subtitle"), "Report subtitle", ""),
      shiny::downloadButton(ns("download"), "Download report image"),
      shiny::helpText("At least one specification limit is required.")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Capability indices"), shiny::tableOutput(ns("indices")),
                  shiny::helpText("Within = short-term (potential) capability from the within-subgroup sigma; ",
                                  "Overall = long-term performance from the overall standard deviation. ",
                                  "Cpm and Cpmk also penalise distance from the target. ",
                                  "Values of 1.33 or more are commonly considered capable.")),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(bslib::card_header("Summary and PPM"), shiny::tableOutput(ns("stats")), shiny::tableOutput(ns("ppm"))),
        bslib::card(bslib::card_header("Nonparametric indices (Tong & Chen)"), shiny::tableOutput(ns("np")),
                    shiny::helpText("For data that are not normal: sigma is replaced by the 0.135%-99.865% ",
                                    "percentile range / 6 and the mean by the median. Needs a large sample ",
                                    "(several hundred values) to estimate the extreme percentiles well."))
      ),
      bslib::card(bslib::card_header("Capability plot (Vannman)"), shiny::plotOutput(ns("vannman"), height = 420)),
      bslib::card(bslib::card_header("Six Sigma capability study (SixSigma::ss.study.ca)"),
                  shiny::plotOutput(ns("study"), height = 620))
    )
  )
}

capabilityServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    prev_cols <- shiny::reactiveVal(NULL)
    shiny::observeEvent(data(), {
      update_pickers(session, data(), list(
        var = list(type = "numeric", default = 1),
        lt = list(type = "numeric", optional = TRUE),
        sg_col = list(type = "any", default = 1)
      ))
      # A different dataset: drop the old spec limits (they belong to the old
      # data). Suggest limits for the book's capability example.
      if (!identical(names(data()), prev_cols())) {
        prev_cols(names(data()))
        if (identical(names(data()), "Volume")) {
          shiny::updateNumericInput(session, "lsl", value = 740)
          shiny::updateNumericInput(session, "usl", value = 760)
          shiny::updateNumericInput(session, "target", value = 750)
        } else {
          clear_numeric(session, c("lsl", "usl", "target"))
        }
      }
    })
    spec <- debounced_inputs(input, c("lsl", "usl", "target"))

    params <- shiny::reactive({
      df <- data()
      shiny::req(input$var %in% names(df))
      lsl <- num_or_na(spec$lsl()); usl <- num_or_na(spec$usl())
      shiny::validate(
        shiny::need(!(is.na(lsl) && is.na(usl)), "Enter at least one specification limit."),
        shiny::need(is.na(lsl) || is.na(usl) || lsl < usl, "LSL must be below USL.")
      )
      target <- num_or_na(spec$target())
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

    sigma_w <- shiny::reactive({
      p <- params(); df <- data()
      switch(input$within,
        none = NA_real_,
        mr = validate_run(cap_within_sigma(p$x, "mr")),
        size = {
          shiny::validate(shiny::need(isTRUE(input$sg_size >= 2), "Subgroup size must be at least 2."))
          validate_run(cap_within_sigma(p$x, "pooled", ceiling(seq_along(p$x) / input$sg_size)))
        },
        col = {
          shiny::req(input$sg_col %in% names(df))
          validate_run(cap_within_sigma(p$x, "pooled", df[[input$sg_col]]))
        })
    })

    output$indices <- shiny::renderTable({
      p <- params()
      validate_run(cap_table(p$x, p$lsl, p$usl, p$target, sigma_w(), 1 - p$alpha))
    }, digits = 4, na = "")
    output$stats <- shiny::renderTable({
      p <- params(); x <- p$x[!is.na(p$x)]
      data.frame(Statistic = c("N", "Mean", "Std. dev. (overall)", "Std. dev. (within)", "Target", "Z (sigma score)"),
                 Value = c(length(x), mean(x), stats::sd(x), sigma_w(), p$target,
                           validate_run(SixSigma::ss.ca.z(x, p$lsl, p$usl))))
    }, digits = 5, na = "")
    output$ppm <- shiny::renderTable({
      p <- params()
      cap_ppm(p$x, p$lsl, p$usl, sigma_w())
    }, digits = 1)
    output$np <- shiny::renderTable({
      p <- params()
      validate_run(cap_nonparametric(p$x, p$lsl, p$usl, p$target))
    }, digits = 4)
    output$vannman <- shiny::renderPlot({
      p <- params(); x <- p$x[!is.na(p$x)]
      s <- if (is.na(sigma_w())) stats::sd(x) else sigma_w()
      shiny::validate(shiny::need(isTRUE(input$k > 0), "Enter the required capability k."))
      validate_run(vannman_plot(mean(x), s, p$lsl, p$usl, p$target, input$k,
                                if (is.na(sigma_w())) "Process (overall sigma)" else "Process (within sigma)"))
    })

    draw <- function() {
      p <- params()
      shiny::validate(shiny::need(!specs_far_from_data(p$x, p$lsl, p$usl, p$target),
        "The specification limits are far outside the data. Check that they belong to this dataset."))
      SixSigma::ss.study.ca(p$x, xLT = p$lt, LSL = p$lsl, USL = p$usl, Target = p$target,
                            alpha = p$alpha, f.sub = input$subtitle)
    }
    output$study <- shiny::renderPlot(validate_run(invisible(capture_text(draw()))))
    output$download <- plot_download("capability", function() invisible(capture_text(draw())))
  })
}
