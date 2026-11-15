# Reliability > Accelerated life testing, Warranty analysis, Demonstration test planning.

status_pickers <- function(session, df, extra = list()) {
  cats <- names(df)[!vapply(df, is.numeric, logical(1))]
  st <- c(cats[grepl("status|cens|fail|state", cats, ignore.case = TRUE)], cats)[1]
  update_pickers(session, df, c(extra, list(
    status = list(type = "any", optional = TRUE, default = if (is.na(st)) "" else st),
    freq = list(type = "numeric", optional = TRUE, default = ""))))
}

fail_value_observer <- function(input, session, data) {
  shiny::observeEvent(list(data(), input$status), {
    df <- data(); s <- empty_to_null(input$status)
    if (is.null(s) || !(s %in% names(df))) return()
    u <- sort(unique(as.character(stats::na.omit(df[[s]]))))
    guess <- u[grepl("^(f|fail|failed|failure|1|true|yes)$", u, ignore.case = TRUE)]
    shiny::updateSelectInput(session, "fail_value", choices = u, selected = if (length(guess)) guess[1] else u[1])
  })
}

altUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Accelerated life testing",
    shiny::tagList(
      column_picker(ns("time"), "Time to failure (or to censoring)"),
      column_picker(ns("stress"), "Stress (accelerating variable)"),
      column_picker(ns("status"), "Status column (blank = all failed)"),
      shiny::conditionalPanel("input.status != '' && input.status != '(none)'", ns = ns,
        shiny::selectInput(ns("fail_value"), "Value that means 'failed'", NULL)),
      column_picker(ns("freq"), "Frequency column (optional)"),
      shiny::selectInput(ns("rel"), "Life-stress relationship", alt_relationships),
      shiny::selectInput(ns("dist"), "Distribution", rel_dists[rel_dists != "normal"]),
      shiny::numericInput(ns("use"), "Use (normal operating) stress", 130),
      shiny::textInput(ns("pcts"), "Percentiles at use stress (%)", "1, 10, 50"),
      blank_numeric(ns("t"), "Time for reliability at use stress (optional)"),
      shiny::numericInput(ns("conf"), "Confidence level (%)", 95, min = 50, max = 99.9),
      shiny::downloadButton(ns("download"), "Download life-stress plot"),
      shiny::helpText("Example: Examples > insulation_alt with the Arrhenius relationship and lognormal distribution.")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Model parameters"), shiny::tableOutput(ns("params")), shiny::uiOutput(ns("note"))),
      bslib::layout_columns(col_widths = c(6, 6),
        bslib::card(bslib::card_header(shiny::textOutput(ns("pct_title"), inline = TRUE)), shiny::tableOutput(ns("pct")), shiny::tableOutput(ns("rel_t"))),
        bslib::card(bslib::card_header("Test stresses"), shiny::tableOutput(ns("acc")))),
      bslib::card(shiny::plotOutput(ns("plot"), height = 440))
    )
  )
}

altServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      df <- data(); nums <- numeric_cols(df)
      s_guess <- c(nums[grepl("temp|stress|volt|load|^t?c$", nums, ignore.case = TRUE)], utils::tail(nums, 1))[1]
      status_pickers(session, df, list(time = list(type = "numeric", default = utils::head(setdiff(nums, s_guess), 1)),
                                       stress = list(type = "numeric", default = s_guess)))
      if (!is.na(s_guess) && s_guess %in% names(df)) {
        lv <- sort(unique(df[[s_guess]]))
        if (length(lv)) shiny::updateNumericInput(session, "use", value = signif(min(lv) - 0.5 * (stats::median(lv) - min(lv) + 1e-9), 3))
        if (grepl("temp|c$", s_guess, ignore.case = TRUE)) shiny::updateSelectInput(session, "rel", selected = "arrhenius")
      }
    })
    fail_value_observer(input, session, data)
    conf <- shiny::reactive({ a <- input$conf / 100; shiny::validate(shiny::need(isTRUE(a > 0 && a < 1), "Confidence must be between 0 and 100.")); a })
    fit <- shiny::reactive({
      df <- data(); shiny::req(input$time %in% names(df), input$stress %in% names(df))
      s <- empty_to_null(input$status); fr <- empty_to_null(input$freq)
      failed <- if (is.null(s)) NULL else { shiny::req(input$fail_value); as.character(df[[s]]) == input$fail_value }
      validate_run(alt_fit(df[[input$time]], failed, df[[input$stress]], if (is.null(fr)) NULL else df[[fr]], input$dist, input$rel))
    })
    use <- shiny::reactive({ shiny::validate(shiny::need(is.finite(input$use), "Enter the use stress.")); input$use })
    summ <- shiny::reactive(validate_run(alt_summary(fit(), use(), conf())))
    output$params <- shiny::renderTable(summ()$parameters, digits = 4)
    output$note <- shiny::renderUI(shiny::helpText(sprintf("Log-likelihood %.3f. Location (log time) = b0 + b1 × %s.",
      summ()$loglik, switch(fit()$rel, arrhenius = "11604.5 / (T + 273.15)", power = "log(stress)", "stress"))))
    output$pct <- shiny::renderTable({
      p <- parse_num_list(input$pcts) / 100; p <- p[p > 0 & p < 1]
      shiny::validate(shiny::need(length(p) > 0, "Enter percentiles between 0 and 100."))
      r <- validate_run(alt_percentile(fit(), use(), p, conf()))
      fmt <- function(v) format(signif(v, 5), big.mark = ",", scientific = FALSE, trim = TRUE)
      data.frame(Percentile = sprintf("B%g", r$Percent), Estimate = fmt(r$Estimate), Lower = fmt(r$Lower), Upper = fmt(r$Upper))
    }, align = "lrrr")
    output$pct_title <- shiny::renderText(sprintf("Life at use stress %g", use()))
    output$rel_t <- shiny::renderTable({
      t <- num_or_na(input$t); if (is.na(t) || t <= 0) return(NULL)
      alt_reliability(fit(), use(), t, conf())
    }, digits = 5)
    output$acc <- shiny::renderTable(int_cols(summ()$acceleration), digits = 4)
    output$plot <- shiny::renderPlot(alt_plot(fit(), use(), conf()))
    output$download <- plot_download("alt-life-stress", function() alt_plot(fit(), use(), conf()))
  })
}

# ---- Warranty ----------------------------------------------------------------------
warrantyUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Warranty analysis",
    shiny::tagList(
      column_picker(ns("shipped"), "Units shipped (one row per shipment period)"),
      column_picker(ns("returns"), "Return columns, in order of months in service", multiple = TRUE),
      shiny::selectInput(ns("dist"), "Distribution", rel_dists[rel_dists != "normal"]),
      shiny::numericInput(ns("horizon"), "Periods to forecast", 6, min = 1, max = 60),
      blank_numeric(ns("limit"), "Warranty length in periods (optional)"),
      shiny::helpText("Lay the data out as a Nevada chart: each row is a shipment period; column j holds returns in ",
                      "the j-th period of service; leave cells blank for ages not reached yet. ",
                      "Example: Examples > warranty_returns.")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Fitted life distribution"), shiny::tableOutput(ns("params"))),
      bslib::layout_columns(col_widths = c(6, 6),
        bslib::card(bslib::card_header("Forecast of returns (all shipments)"), shiny::tableOutput(ns("forecast"))),
        bslib::card(bslib::card_header("Shipments"), shiny::tableOutput(ns("cohorts")))),
      bslib::card(shiny::plotOutput(ns("plot"), height = 380))
    )
  )
}

warrantyServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      df <- data(); nums <- numeric_cols(df)
      ship <- c(nums[grepl("ship|qty|quant|sold|units", nums, ignore.case = TRUE)], nums)[1]
      update_pickers(session, df, list(shipped = list(type = "numeric", default = ship)))
      rc <- setdiff(nums, ship)
      shiny::updateSelectInput(session, "returns", choices = nums, selected = keep_selected(session, "returns", nums, rc))
    })
    res <- shiny::reactive({
      df <- data(); r <- intersect(input$returns, names(df)); shiny::req(input$shipped %in% names(df))
      shiny::validate(shiny::need(length(r) >= 2, "Choose at least two return columns."),
                      shiny::need(!(input$shipped %in% r), "The shipped column cannot also be a return column."),
                      shiny::need(isTRUE(input$horizon >= 1), "Forecast at least one period."))
      validate_run(warranty_analyze(df[[input$shipped]], df[, r, drop = FALSE], input$dist, input$horizon, num_or_na(input$limit)))
    })
    output$params <- shiny::renderTable(rel_parameters(res()$fit), digits = 4)
    output$forecast <- shiny::renderTable(res()$forecast, digits = 2)
    output$cohorts <- shiny::renderTable(int_cols(res()$cohorts), digits = 3)
    output$plot <- shiny::renderPlot({
      f <- res()$forecast
      ggplot2::ggplot(f, ggplot2::aes(`Future period`, Cumulative)) + ggplot2::geom_col(fill = "#4C78A8") +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = Cumulative, ymax = `Approx. 95% upper (Poisson)`), width = 0.3) +
        ggplot2::labs(title = "Cumulative expected returns", x = "Periods from now", y = "Returns") + ggplot2::theme_minimal(base_size = 13)
    })
  })
}

# ---- Demonstration test planning -------------------------------------------------------
demoUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Demonstration test planning",
    shiny::tagList(
      shiny::selectInput(ns("type"), "Goal", c("Demonstrate reliability at a mission time (Weibull / binomial)" = "rel",
                                                "Demonstrate MTBF (exponential, total test time)" = "mtbf")),
      shiny::conditionalPanel("input.type == 'rel'", ns = ns,
        shiny::numericInput(ns("R"), "Reliability to demonstrate", 0.9, min = 0.5, max = 0.99999, step = 0.01),
        shiny::numericInput(ns("mission"), "Mission time", 1000, min = 0),
        shiny::numericInput(ns("beta"), "Weibull shape β (assumed; 1 = constant failure rate)", 2, min = 0.1, step = 0.1),
        shiny::selectInput(ns("solve"), "Solve for", c("Number of units" = "n", "Test time per unit" = "time")),
        shiny::conditionalPanel("input.solve == 'n'", ns = ns,
          shiny::numericInput(ns("ttime"), "Test time per unit", 1000, min = 0)),
        shiny::conditionalPanel("input.solve == 'time'", ns = ns,
          shiny::numericInput(ns("n"), "Units on test", 10, min = 1))),
      shiny::conditionalPanel("input.type == 'mtbf'", ns = ns,
        shiny::numericInput(ns("mtbf"), "MTBF to demonstrate", 5000, min = 0)),
      shiny::numericInput(ns("conf"), "Confidence (%)", 90, min = 50, max = 99.9),
      shiny::numericInput(ns("fails"), "Failures allowed", 0, min = 0, max = 20)
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Test plan"), shiny::uiOutput(ns("plan"))),
      bslib::card(bslib::card_header("Units needed by failures allowed and test length"), shiny::tableOutput(ns("table"))),
      bslib::card(shiny::plotOutput(ns("oc"), height = 360),
                  shiny::helpText("Operating characteristic: chance of passing the test for different true reliabilities."))
    )
  )
}

demoServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    conf <- shiny::reactive({ a <- input$conf / 100; shiny::validate(shiny::need(isTRUE(a > 0 && a < 1), "Confidence must be between 0 and 100.")); a })
    fails <- shiny::reactive({ shiny::validate(shiny::need(isTRUE(input$fails >= 0), "Enter the failures allowed.")); as.integer(input$fails) })
    plan <- shiny::reactive({
      if (input$type == "mtbf") {
        shiny::validate(shiny::need(isTRUE(input$mtbf > 0), "Enter the MTBF."))
        return(list(type = "mtbf", total = demo_mtbf_time(input$mtbf, conf(), fails())))
      }
      shiny::validate(shiny::need(isTRUE(input$R > 0 && input$R < 1), "Reliability must be between 0 and 1."),
                      shiny::need(isTRUE(input$mission > 0 && input$beta > 0), "Enter a positive mission time and shape."))
      if (input$solve == "n") {
        shiny::validate(shiny::need(isTRUE(input$ttime > 0), "Enter the test time per unit."))
        m <- input$ttime / input$mission
        list(type = "rel", n = validate_run(demo_sample_size(input$R, conf(), fails(), m, input$beta)), m = m)
      } else {
        shiny::validate(shiny::need(isTRUE(input$n > fails()), "Test more units than failures allowed."))
        m <- validate_run(demo_test_time(input$R, conf(), input$n, fails(), input$beta))
        list(type = "rel", n = input$n, m = m)
      }
    })
    output$plan <- shiny::renderUI({
      p <- plan()
      if (p$type == "mtbf") return(shiny::tags$p(shiny::tags$b(sprintf("Total test time: %s unit-hours", format(signif(p$total, 5), big.mark = ","))),
        sprintf(" (spread over any number of units). The test passes with %d or fewer failures, demonstrating MTBF ≥ %g at %g%% confidence.",
                fails(), input$mtbf, input$conf)))
      shiny::tags$p(shiny::tags$b(sprintf("Test %d units for %s each (%.3g × the mission time).", p$n, format(signif(p$m * input$mission, 5), big.mark = ","), p$m)),
        if (fails() == 0) " Pass if no unit fails." else sprintf(" Pass if %d or fewer fail.", fails()),
        sprintf(" This demonstrates R(%g) ≥ %g at %g%% confidence, assuming a Weibull shape of %g.",
                input$mission, input$R, input$conf, input$beta))
    })
    output$table <- shiny::renderTable({
      if (input$type == "mtbf") {
        f <- 0:5
        return(data.frame(`Failures allowed` = f, `Total test time` = demo_mtbf_time(input$mtbf, conf(), f),
                          `Multiple of MTBF` = demo_mtbf_time(1, conf(), f), check.names = FALSE))
      }
      validate_run(demo_table(input$R, conf(), input$beta, failures = 0:3))
    }, digits = 3)
    output$oc <- shiny::renderPlot({
      p <- plan()
      if (p$type == "mtbf") {
        th <- seq(0.2, 5, length.out = 200) * input$mtbf
        d <- data.frame(x = th, Pass = stats::ppois(fails(), p$total / th))
        lab <- "True MTBF"; ref <- input$mtbf
      } else {
        d <- demo_oc(p$n, p$m, input$beta, fails()); names(d)[1] <- "x"; lab <- "True reliability at mission time"; ref <- input$R
      }
      ggplot2::ggplot(d, ggplot2::aes(x, Pass)) + ggplot2::geom_line(linewidth = 1, colour = "#4C78A8") +
        ggplot2::geom_vline(xintercept = ref, linetype = "dashed") +
        ggplot2::labs(title = "Operating characteristic", x = lab, y = "Probability of passing") + ggplot2::theme_minimal(base_size = 13)
    })
  })
}
