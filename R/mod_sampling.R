# Quality Tools > Acceptance sampling.

samplingUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Acceptance sampling",
    shiny::tagList(
      shiny::selectInput(ns("mode"), "Task", c("Design an attribute plan from risks" = "design",
                                               "Evaluate a single sampling plan" = "single",
                                               "Evaluate a double sampling plan" = "double",
                                               "Variables plan (k-method)" = "variables")),
      shiny::conditionalPanel("input.mode == 'design' || input.mode == 'variables'", ns = ns,
        shiny::fluidRow(
          shiny::column(6, shiny::numericInput(ns("aql"), "AQL (%)", 1, min = 0.01, max = 50, step = 0.1)),
          shiny::column(6, shiny::numericInput(ns("alpha"), "Producer's risk α (%)", 5, min = 0.1, max = 49))),
        shiny::fluidRow(
          shiny::column(6, shiny::numericInput(ns("ltpd"), "LTPD / RQL (%)", 6, min = 0.02, max = 90, step = 0.5)),
          shiny::column(6, shiny::numericInput(ns("beta"), "Consumer's risk β (%)", 10, min = 0.1, max = 49)))),
      shiny::conditionalPanel("input.mode == 'single'", ns = ns,
        shiny::fluidRow(
          shiny::column(6, shiny::numericInput(ns("n"), "Sample size n", 89, min = 1)),
          shiny::column(6, shiny::numericInput(ns("c"), "Acceptance number c", 2, min = 0)))),
      shiny::conditionalPanel("input.mode == 'double'", ns = ns,
        shiny::fluidRow(
          shiny::column(4, shiny::numericInput(ns("n1"), "n1", 50, min = 1)),
          shiny::column(4, shiny::numericInput(ns("c1"), "Accept ≤", 1, min = 0)),
          shiny::column(4, shiny::numericInput(ns("r1"), "Reject ≥", 4, min = 1))),
        shiny::fluidRow(
          shiny::column(4, shiny::numericInput(ns("n2"), "n2", 100, min = 1)),
          shiny::column(8, shiny::numericInput(ns("c2"), "Accept if total ≤", 3, min = 0)))),
      shiny::conditionalPanel("input.mode != 'variables'", ns = ns,
        shiny::selectInput(ns("model"), "Probability model", c("Binomial" = "binomial", "Hypergeometric (finite lot)" = "hypergeometric",
                                                               "Poisson" = "poisson"))),
      blank_numeric(ns("N"), "Lot size N (optional; for AOQ, ATI and the hypergeometric model)"),
      shiny::conditionalPanel("input.mode == 'variables'", ns = ns,
        shiny::checkboxInput(ns("sigma_known"), "Process standard deviation is known", FALSE),
        shiny::tags$hr(), shiny::tags$b("Judge a lot (optional)"),
        column_picker(ns("x"), "Measurements from the lot"),
        blank_numeric(ns("lsl"), "LSL"), blank_numeric(ns("usl"), "USL"),
        blank_numeric(ns("sigma"), "Known sigma (if checked above)")),
      shiny::textInput(ns("ps"), "Report Pa at these % defective", "0.5, 1, 2, 5, 10"),
      shiny::downloadButton(ns("download"), "Download OC curve")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Plan"), shiny::uiOutput(ns("plan"))),
      shiny::uiOutput(ns("lot_card")),
      bslib::card(shiny::plotOutput(ns("curves"), height = 380)),
      bslib::card(bslib::card_header("Probability of acceptance"), shiny::tableOutput(ns("tab")),
                  shiny::helpText("AQL: quality level that should usually be accepted (producer's risk α of rejecting it). ",
                                  "LTPD: quality level that should usually be rejected (consumer's risk β of accepting it)."))
    )
  )
}

samplingServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), update_pickers(session, data(), list(x = list(type = "numeric", default = 1))))
    pct <- function(v) num_or_na(v) / 100
    N <- shiny::reactive({
      v <- num_or_na(input$N)
      shiny::validate(shiny::need(is.na(v) || v >= 2, "The lot size must be at least 2."))
      v
    })
    plan <- shiny::reactive({
      model <- if (input$mode == "variables") "binomial" else input$model
      if (model == "hypergeometric") shiny::validate(shiny::need(!is.na(N()), "Enter the lot size for the hypergeometric model."))
      validate_run(switch(input$mode,
        design = { d <- design_attribute(pct(input$aql), pct(input$alpha), pct(input$ltpd), pct(input$beta), model, N())
                   list(type = "single", n = d$n, c = d$c, model = model,
                        pa = function(p) pa_single(p, d$n, d$c, N(), model)) },
        single = { shiny::validate(shiny::need(isTRUE(input$n >= 1 && input$c >= 0 && input$c < input$n), "Need 0 ≤ c < n."))
                   if (!is.na(N())) shiny::validate(shiny::need(input$n <= N(), "n cannot exceed the lot size."))
                   list(type = "single", n = input$n, c = input$c, model = model,
                        pa = function(p) pa_single(p, input$n, input$c, N(), model)) },
        double = { shiny::validate(shiny::need(isTRUE(input$c1 < input$r1 && input$c2 >= input$c1), "Need c1 < r1 and c2 ≥ c1."))
                   list(type = "double", n = input$n1, model = "binomial",
                        pa = function(p) pa_double(p, input$n1, input$c1, input$r1, input$n2, input$c2),
                        asn = function(p) asn_double(p, input$n1, input$c1, input$r1, input$n2)) },
        variables = { v <- design_variables(pct(input$aql), pct(input$alpha), pct(input$ltpd), pct(input$beta), isTRUE(input$sigma_known))
                      list(type = "variables", n = v$n, k = v$k, model = "normal",
                           pa = function(p) pa_variables(p, v$n, v$k, isTRUE(input$sigma_known))) }))
    })
    pmax_plot <- shiny::reactive({
      hi <- max(c(pct(input$ltpd), 0.02), na.rm = TRUE) * 2.5
      if (input$mode %in% c("single", "double")) hi <- max(0.05, min(1, 8 / max(plan()$n, 1)))
      seq(0, min(hi, 1), length.out = 301)[-1]
    })
    curves <- shiny::reactive({
      p <- pmax_plot(); pl <- plan()
      sampling_curves(p, pl$pa(p), pl$n, N(),
                      if (input$mode %in% c("design", "variables")) pct(input$aql) else NA,
                      if (input$mode %in% c("design", "variables")) pct(input$ltpd) else NA,
                      if (!is.null(pl$asn)) pl$asn(p))
    })
    output$plan <- shiny::renderUI({
      pl <- plan(); cv <- curves()
      txt <- switch(pl$type,
        single = sprintf("Inspect n = %d items; accept the lot if %d or fewer are defective, otherwise reject.", pl$n, pl$c),
        double = sprintf("Inspect %d; accept if ≤ %d defective, reject if ≥ %d; otherwise inspect %d more and accept if the total is ≤ %d.",
                         input$n1, input$c1, input$r1, input$n2, input$c2),
        variables = sprintf("Measure n = %d items; compute Q = (USL - mean)/s or (mean - LSL)/s and accept the lot if Q ≥ k = %.3f.",
                            pl$n, pl$k))
      extra <- if (input$mode %in% c("design", "variables"))
        sprintf("At AQL %.2f%%: Pa = %.3f (target ≥ %.3f). At LTPD %.2f%%: Pa = %.3f (target ≤ %.3f).",
                input$aql, pl$pa(pct(input$aql)), 1 - pct(input$alpha), input$ltpd, pl$pa(pct(input$ltpd)), pct(input$beta))
      shiny::tagList(shiny::div(class = "alert alert-info mb-2", txt),
                     if (!is.null(extra)) shiny::tags$p(extra),
                     if (pl$type != "variables") shiny::tags$p(sprintf("AOQL = %.3f%% at %.2f%% incoming defective%s.",
                                                                       100 * cv$aoql, 100 * cv$aoql_p,
                                                                       if (is.na(N())) " (infinite lot)" else "")))
    })
    output$lot_card <- shiny::renderUI({
      if (input$mode != "variables") return(NULL)
      bslib::card(bslib::card_header("Lot decision"), shiny::uiOutput(session$ns("lot")))
    })
    output$lot <- shiny::renderUI({
      df <- data(); pl <- plan()
      lsl <- num_or_na(input$lsl); usl <- num_or_na(input$usl)
      if (is.na(lsl) && is.na(usl)) return(shiny::helpText("Enter a specification limit to judge a lot from the selected column."))
      shiny::req(input$x %in% names(df))
      sg <- if (isTRUE(input$sigma_known)) num_or_na(input$sigma) else NA
      r <- validate_run(variables_decision(df[[input$x]], pl$n, pl$k, lsl, usl, sg))
      shiny::tagList(
        shiny::div(class = paste("alert mb-2", if (r$accept) "alert-success" else "alert-danger"),
                   if (r$accept) "Accept the lot (Q ≥ k)." else "Reject the lot (Q < k)."),
        if (!is.null(r$note)) shiny::helpText(r$note),
        shiny::renderTable(r$table, digits = 4))
    })
    output$curves <- shiny::renderPlot(draw_grid(curves()$plots, ncol = length(curves()$plots)))
    output$tab <- shiny::renderTable({
      p <- parse_num_list(input$ps) / 100
      shiny::validate(shiny::need(length(p) && all(p > 0 & p < 1), "Enter percentages between 0 and 100."))
      pl <- plan(); pa <- pl$pa(p)
      out <- data.frame(`% defective` = 100 * p, Pa = pa, `Pr(reject)` = 1 - pa, check.names = FALSE)
      if (pl$type != "variables") out$`AOQ (%)` <- 100 * pa * p * (if (is.na(N())) 1 else (N() - pl$n) / N())
      if (!is.null(pl$asn)) out$ASN <- pl$asn(p)
      out
    }, digits = 4)
    output$download <- plot_download("oc-curve", function() curves()$plots[[1]], height = 5)
  })
}
