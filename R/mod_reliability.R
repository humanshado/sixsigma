# Reliability > Life data analysis (right-censored).

reliabilityUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Life data analysis",
    shiny::tagList(
      column_picker(ns("time"), "Time to failure (or to censoring)"),
      column_picker(ns("status"), "Status column (blank = all failed)"),
      shiny::conditionalPanel("input.status != '' && input.status != '(none)'", ns = ns,
        shiny::selectInput(ns("fail_value"), "Value that means 'failed'", NULL)),
      column_picker(ns("freq"), "Frequency column (optional)"),
      shiny::selectInput(ns("dist"), "Distribution", c("Best fit (lowest AIC)" = "best", rel_dists)),
      shiny::numericInput(ns("conf"), "Confidence level (%)", 95, min = 50, max = 99.9),
      shiny::textInput(ns("pcts"), "Percentiles to estimate (B-lives, %)", "1, 5, 10, 50"),
      shiny::textInput(ns("times"), "Times for reliability (blank = automatic)", ""),
      shiny::downloadButton(ns("download"), "Download probability plot"),
      shiny::helpText("Right-censored units are those still running (or removed) without failing; they still ",
                      "carry information. Try the Examples datasets ball_bearings and pump_field_failures.")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Distribution fits (best first)"), shiny::tableOutput(ns("compare")),
                  shiny::uiOutput(ns("data_note"))),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(bslib::card_header(shiny::textOutput(ns("ptitle"), inline = TRUE)), shiny::tableOutput(ns("params")),
                    shiny::tableOutput(ns("mttf"))),
        bslib::card(bslib::card_header("Percentiles (B-lives)"), shiny::tableOutput(ns("pct")),
                    shiny::helpText("B10 = time by which 10% of units are expected to fail."), shiny::uiOutput(ns("neg_note")))
      ),
      bslib::card(shiny::plotOutput(ns("prob"), height = 460)),
      bslib::card(bslib::card_header("Reliability at chosen times"), shiny::tableOutput(ns("rel"))),
      bslib::card(shiny::plotOutput(ns("curves"), height = 380)),
      bslib::card(bslib::card_header("Kaplan-Meier (nonparametric) estimates"), shiny::tableOutput(ns("km")))
    )
  )
}

parse_num_list <- function(s) {
  v <- suppressWarnings(as.numeric(trimws(strsplit(s, ",", fixed = TRUE)[[1]])))
  v[!is.na(v)]
}

reliabilityServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      df <- data()
      cats <- names(df)[!vapply(df, is.numeric, logical(1))]
      st <- c(cats[grepl("status|cens|fail|state", cats, ignore.case = TRUE)], cats)[1]
      update_pickers(session, df, list(
        time = list(type = "numeric", default = utils::head(setdiff(numeric_cols(df), "Unit"), 1)),
        status = list(type = "any", optional = TRUE, default = if (is.na(st)) "" else st),
        freq = list(type = "numeric", optional = TRUE, default = "")))
    })
    shiny::observeEvent(list(data(), input$status), {
      df <- data(); s <- empty_to_null(input$status)
      if (is.null(s) || !(s %in% names(df))) return()
      u <- sort(unique(as.character(stats::na.omit(df[[s]]))))
      guess <- u[grepl("^(f|fail|failed|failure|1|true|yes)$", u, ignore.case = TRUE)]
      shiny::updateSelectInput(session, "fail_value", choices = u, selected = if (length(guess)) guess[1] else u[1])
    })

    conf <- shiny::reactive({
      a <- input$conf / 100
      shiny::validate(shiny::need(isTRUE(a > 0 && a < 1), "Confidence level must be between 0 and 100."))
      a
    })
    life <- shiny::reactive({
      df <- data(); shiny::req(input$time %in% names(df))
      s <- empty_to_null(input$status); fr <- empty_to_null(input$freq)
      failed <- if (is.null(s)) NULL else {
        shiny::req(input$fail_value)
        as.character(df[[s]]) == input$fail_value
      }
      validate_run(rel_data(df[[input$time]], failed, if (is.null(fr)) NULL else df[[fr]]))
    })
    comparison <- shiny::reactive(validate_run(rel_compare(life())))
    fit <- shiny::reactive({
      d <- if (input$dist == "best") comparison()$Code[1] else input$dist
      validate_run(rel_fit(life(), d))
    })

    output$compare <- shiny::renderTable(comparison()[, c("Distribution", "Log-likelihood", "AIC")], digits = 3)
    output$data_note <- shiny::renderUI({
      d <- life()
      shiny::helpText(sprintf("%g failures and %g censored units. Lower AIC = better fit.",
                              sum(d$w[d$f]), sum(d$w[!d$f])))
    })
    output$ptitle <- shiny::renderText(sprintf("%s parameters", fit()$label))
    output$params <- shiny::renderTable(rel_parameters(fit(), conf()), digits = 5)
    output$mttf <- shiny::renderTable(rel_mttf(fit(), conf()), digits = 5, na = "-")
    output$pct <- shiny::renderTable({
      p <- parse_num_list(input$pcts) / 100
      shiny::validate(shiny::need(length(p) && all(p > 0 & p < 1), "Enter percentiles between 0 and 100."))
      rel_percentiles(fit(), sort(p), conf())
    }, digits = 5)
    output$neg_note <- shiny::renderUI({
      if (fit()$dist != "normal") return(NULL)
      p <- parse_num_list(input$pcts) / 100; p <- p[p > 0 & p < 1]
      if (!length(p) || all(rel_percentiles(fit(), p, conf())$Life >= 0)) return(NULL)
      shiny::div(class = "alert alert-warning mt-2", "The normal distribution allows negative lifetimes, so some ",
                 "percentiles are below zero. A distribution for positive times (Weibull, lognormal, log-logistic) is usually more suitable.")
    })
    output$rel <- shiny::renderTable({
      t <- parse_num_list(input$times)
      if (!length(t)) t <- signif(stats::quantile(life()$t, c(0.1, 0.25, 0.5, 0.75, 0.9), names = FALSE), 3)
      if (fit()$log) t <- t[t > 0]
      rel_reliability(fit(), sort(unique(t)), conf())
    }, digits = 5)
    output$prob <- shiny::renderPlot(rel_probability_plot(fit(), conf()))
    output$curves <- shiny::renderPlot({
      g <- rel_curves_plot(fit(), conf())
      grid::grid.newpage(); grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 3)))
      for (i in 1:3) print(g[[i]], vp = grid::viewport(layout.pos.row = 1, layout.pos.col = i))
    })
    output$km <- shiny::renderTable(int_cols(rel_kaplan_meier(life(), conf())), digits = 5)
    output$download <- plot_download("probability-plot", function() rel_probability_plot(fit(), conf()), height = 6)
  })
}
