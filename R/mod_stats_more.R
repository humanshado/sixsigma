# Stat > Tolerance intervals, Equivalence tests, Correlation, Outlier tests, Time series.

toleranceUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Tolerance intervals",
    shiny::tagList(
      column_picker(ns("x"), "Data"),
      shiny::numericInput(ns("cov"), "Minimum percentage of population covered", 95, min = 50, max = 99.99),
      shiny::numericInput(ns("conf"), "Confidence level (%)", 95, min = 50, max = 99.99),
      shiny::selectInput(ns("sides"), "Interval", c("Two-sided" = "two", "Lower bound only" = "lower", "Upper bound only" = "upper")),
      shiny::helpText("A tolerance interval covers a stated percentage of individual values (not the mean) with the ",
                      "stated confidence, e.g. 'with 95% confidence, at least 95% of parts lie between ...'.")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Tolerance intervals"), shiny::tableOutput(ns("tab"))),
      bslib::card(shiny::plotOutput(ns("plot"), height = 360))
    )
  )
}

toleranceServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), update_pickers(session, data(), list(x = list(type = "numeric", default = 1))))
    res <- shiny::reactive({
      df <- data(); shiny::req(input$x %in% names(df))
      validate_run(tolerance_interval(df[[input$x]], input$cov / 100, input$conf / 100, input$sides))
    })
    output$tab <- shiny::renderTable(res(), digits = 5, na = "-")
    output$plot <- shiny::renderPlot({
      df <- data(); x <- df[[input$x]]; x <- x[!is.na(x)]; r <- res()
      lines <- data.frame(v = c(r$Lower, r$Upper), m = rep(r$Method, 2))
      lines <- lines[!is.na(lines$v), ]
      ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x)) +
        ggplot2::geom_histogram(bins = max(8, min(30, length(x) %/% 4)), fill = "#c9d8ea", colour = "white") +
        ggplot2::geom_vline(data = lines, ggplot2::aes(xintercept = v, colour = m, linetype = m), linewidth = 1) +
        ggplot2::scale_colour_manual(values = c("#c0392b", "#2e7d32"), name = NULL) + ggplot2::scale_linetype_manual(values = c(1, 2), name = NULL) +
        ggplot2::labs(title = "Data with tolerance limits", x = input$x, y = "Count") +
        ggplot2::theme_bw(base_size = 12) + ggplot2::theme(legend.position = "top", plot.title = ggplot2::element_text(face = "bold"))
    })
  })
}

equivalenceUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Equivalence tests",
    shiny::tagList(
      shiny::selectInput(ns("type"), "Test", c("1-sample (mean vs target)" = "one", "2-sample (two means)" = "two", "Paired" = "paired")),
      column_picker(ns("x"), "Sample (or first sample)"),
      shiny::conditionalPanel("input.type != 'one'", ns = ns, column_picker(ns("y"), "Second sample")),
      shiny::conditionalPanel("input.type == 'one'", ns = ns, blank_numeric(ns("target"), "Target")),
      shiny::fluidRow(shiny::column(6, blank_numeric(ns("lower"), "Lower limit")), shiny::column(6, blank_numeric(ns("upper"), "Upper limit"))),
      shiny::numericInput(ns("alpha"), "α", 0.05, min = 0.001, max = 0.25, step = 0.01),
      shiny::helpText("Shows that a difference is small enough not to matter (e.g. a new supplier is equivalent to ",
                      "the old one within ±0.5). Limits are on the difference (mean - target, or first - second).")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Result"), shiny::uiOutput(ns("concl")), shiny::tableOutput(ns("tab"))),
      bslib::card(shiny::plotOutput(ns("plot"), height = 260))
    )
  )
}

equivalenceServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), update_pickers(session, data(), list(x = list(type = "numeric", default = 1), y = list(type = "numeric", default = 2))))
    res <- shiny::reactive({
      df <- data(); shiny::req(input$x %in% names(df))
      lo <- num_or_na(input$lower); hi <- num_or_na(input$upper)
      shiny::validate(shiny::need(!is.na(lo) && !is.na(hi), "Enter the lower and upper equivalence limits (e.g. -0.5 and 0.5)."))
      y <- if (input$type != "one") { shiny::req(input$y %in% names(df)); df[[input$y]] }
      if (input$type == "one") shiny::validate(shiny::need(!is.na(num_or_na(input$target)), "Enter the target."))
      validate_run(tost(df[[input$x]], y, lo, hi, input$type, num_or_na(input$target) %||% 0, input$alpha))
    })
    output$concl <- shiny::renderUI(shiny::div(class = paste("alert mb-2", if (res()$equivalent) "alert-success" else "alert-warning"), res()$conclusion))
    output$tab <- shiny::renderTable(res()$table, digits = 5)
    output$plot <- shiny::renderPlot(tost_plot(res()))
  })
}

correlationUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Correlation",
    shiny::tagList(
      column_picker(ns("vars"), "Variables", multiple = TRUE),
      shiny::selectInput(ns("method"), "Method", c("Pearson (linear)" = "pearson", "Spearman (rank, monotonic)" = "spearman")),
      shiny::helpText("Correlation measures association, not cause and effect.")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Pairwise correlations"), shiny::tableOutput(ns("tab"))),
      bslib::layout_columns(col_widths = c(5, 7),
        bslib::card(shiny::plotOutput(ns("heat"), height = 420)),
        bslib::card(shiny::plotOutput(ns("scatter"), height = 420)))
    )
  )
}

correlationServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      nums <- numeric_cols(data())
      shiny::updateSelectInput(session, "vars", choices = nums, selected = keep_selected(session, "vars", nums, utils::head(nums, 4)))
    })
    vars <- shiny::reactive({ v <- intersect(input$vars, names(data())); shiny::validate(shiny::need(length(v) >= 2, "Choose at least two numeric variables.")); v })
    output$tab <- shiny::renderTable(validate_run(correlation_table(data(), vars(), input$method)), digits = 4)
    output$heat <- shiny::renderPlot(correlation_plot(data(), vars(), input$method))
    output$scatter <- shiny::renderPlot({ shiny::validate(shiny::need(length(vars()) <= 6, "Scatterplots are shown for up to 6 variables.")); scatter_matrix(data(), vars()) })
  })
}

outlierUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Outlier tests",
    shiny::tagList(
      column_picker(ns("x"), "Data"),
      shiny::selectInput(ns("side"), "Grubbs' test", c("Largest or smallest (two-sided)" = "two", "Largest value" = "max", "Smallest value" = "min")),
      shiny::numericInput(ns("r"), "Maximum number of outliers (generalized ESD)", 3, min = 1, max = 20),
      shiny::numericInput(ns("alpha"), "α", 0.05, min = 0.001, max = 0.2, step = 0.01),
      shiny::helpText("Both tests assume the remaining data are normal. Investigate an outlier before removing it: ",
                      "it may be a recording error, or a real signal.")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Grubbs' test (one outlier)"), shiny::tableOutput(ns("grubbs"))),
      bslib::card(bslib::card_header("Generalized ESD test (Rosner, several outliers)"), shiny::tableOutput(ns("esd"))),
      bslib::card(shiny::plotOutput(ns("plot"), height = 320))
    )
  )
}

outlierServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), update_pickers(session, data(), list(x = list(type = "numeric", default = 1))))
    x <- shiny::reactive({ df <- data(); shiny::req(input$x %in% names(df)); df[[input$x]] })
    output$grubbs <- shiny::renderTable(validate_run(grubbs_test(x(), input$side, input$alpha)), digits = 5)
    esd <- shiny::reactive(validate_run(esd_test(x(), input$r, input$alpha)))
    output$esd <- shiny::renderTable(esd(), digits = 4)
    output$plot <- shiny::renderPlot({
      v <- x(); out <- esd(); flag <- seq_along(v) %in% out$Row[out$Outlier == "Yes"]
      ggplot2::ggplot(data.frame(i = seq_along(v), v = v, Outlier = ifelse(flag, "Outlier", "Normal")), ggplot2::aes(i, v, colour = Outlier)) +
        ggplot2::geom_point(size = 2.5, na.rm = TRUE) + ggplot2::scale_colour_manual(values = c(Normal = "#4a6fa5", Outlier = "#c0392b")) +
        ggplot2::labs(title = "Data in row order", x = "Row", y = input$x, colour = NULL) +
        ggplot2::theme_bw(base_size = 12) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
    })
  })
}

timeseriesUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Time series",
    shiny::tagList(
      column_picker(ns("y"), "Series (in time order)"),
      shiny::selectInput(ns("method"), "Method", c("Linear trend" = "linear", "Quadratic trend" = "quadratic",
                                                   "Exponential growth" = "exponential", "Moving average" = "moving",
                                                   "Single exponential smoothing" = "single", "Double exponential smoothing (Holt)" = "double",
                                                   "Decomposition (trend + seasonal)" = "decomposition")),
      shiny::conditionalPanel("input.method == 'moving'", ns = ns, shiny::numericInput(ns("window"), "Moving average length", 3, min = 2)),
      shiny::conditionalPanel("input.method == 'single'", ns = ns, blank_numeric(ns("alpha"), "Smoothing weight alpha (blank = optimal)")),
      shiny::conditionalPanel("input.method == 'decomposition'", ns = ns,
        shiny::numericInput(ns("period"), "Seasonal period (e.g. 12 for months)", 12, min = 2),
        shiny::radioButtons(ns("decomp"), NULL, c("Additive" = "additive", "Multiplicative" = "multiplicative"), inline = TRUE)),
      shiny::numericInput(ns("h"), "Periods to forecast", 6, min = 0, max = 100),
      shiny::numericInput(ns("lags"), "Lags for autocorrelation", 20, min = 2, max = 100)
    ),
    shiny::tagList(
      bslib::card(shiny::plotOutput(ns("plot"), height = 400)),
      bslib::layout_columns(col_widths = c(4, 8),
        bslib::card(bslib::card_header("Accuracy (lower is better)"), shiny::tableOutput(ns("acc"))),
        bslib::card(bslib::card_header("Forecasts"), shiny::tableOutput(ns("fc")), shiny::tableOutput(ns("seas")))),
      bslib::card(shiny::plotOutput(ns("acf"), height = 320))
    )
  )
}

timeseriesServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), update_pickers(session, data(), list(y = list(type = "numeric", default = 1))))
    y <- shiny::reactive({ df <- data(); shiny::req(input$y %in% names(df)); df[[input$y]] })
    res <- shiny::reactive({
      a <- num_or_na(input$alpha)
      if (!is.na(a)) shiny::validate(shiny::need(a > 0 && a <= 1, "Alpha must be between 0 and 1."))
      validate_run(ts_analyze(y(), input$method, max(0, input$h %||% 0), input$window, a, input$period, input$decomp))
    })
    output$plot <- shiny::renderPlot(ts_plot(res()))
    output$acc <- shiny::renderTable(res()$accuracy, digits = 4)
    output$fc <- shiny::renderTable(int_cols(res()$forecast), digits = 4)
    output$seas <- shiny::renderTable(if (!is.null(res()$seasonal)) int_cols(res()$seasonal), digits = 4)
    output$acf <- shiny::renderPlot(validate_run(acf_plot(y(), input$lags)))
  })
}
