# Stat > Sample size and power.

power_tests <- c("1-sample t" = "one_t", "2-sample t" = "two_t", "Paired t" = "paired",
                 "1 proportion" = "one_prop", "2 proportions" = "two_prop", "One-way ANOVA" = "anova",
                 "2-level factorial design" = "factorial")

powerUI <- function(id) {
  ns <- shiny::NS(id)
  is_type <- function(...) sprintf("[%s].indexOf(input.test) >= 0", paste0("'", c(...), "'", collapse = ","))
  analysis_layout(
    "Sample size and power",
    shiny::tagList(
      shiny::selectInput(ns("test"), "Test", power_tests, selected = "two_t"),
      shiny::helpText("Fill in two of the three boxes below; the blank one is calculated."),
      blank_numeric(ns("n"), "Sample size (blank = calculate)"),
      shiny::numericInput(ns("power"), "Power (blank = calculate)", 0.8, min = 0.01, max = 0.999, step = 0.05),
      shiny::numericInput(ns("delta"), "Difference to detect (blank = calculate)", 1),
      shiny::conditionalPanel(is_type("one_t", "two_t", "paired", "anova", "factorial"), ns = ns,
        shiny::numericInput(ns("sd"), "Standard deviation", 1, min = 0)),
      shiny::conditionalPanel(is_type("one_prop", "two_prop"), ns = ns,
        shiny::numericInput(ns("p0"), "Baseline proportion (hypothesised / group 1)", 0.5, min = 0.001, max = 0.999, step = 0.05)),
      shiny::conditionalPanel(is_type("anova"), ns = ns,
        shiny::numericInput(ns("groups"), "Number of groups", 3, min = 2)),
      shiny::conditionalPanel(is_type("factorial"), ns = ns,
        shiny::numericInput(ns("k"), "Number of factors", 3, min = 2, max = 8),
        shiny::numericInput(ns("runs"), "Runs per replicate (2^k or a fraction)", 8, min = 4),
        shiny::numericInput(ns("center"), "Center points", 0, min = 0),
        shiny::helpText("Model: main effects and two-factor interactions. Sample size = number of replicates.")),
      shiny::selectInput(ns("alt"), "Alternative", c("Not equal" = "two.sided", "One-sided" = "one.sided")),
      shiny::numericInput(ns("alpha"), "Significance level α", 0.05, min = 0.001, max = 0.5, step = 0.01)
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Result"), shiny::uiOutput(ns("result")), shiny::tableOutput(ns("tab"))),
      bslib::card(shiny::plotOutput(ns("curve"), height = 420),
                  shiny::helpText("Power = chance of detecting the difference if it really exists. 80-90% is usual."))
    )
  )
}

powerServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Proportion tests need a small difference; others default to 1 SD.
    shiny::observeEvent(input$test, {
      d <- num_or_na(input$delta)
      if (input$test %in% c("one_prop", "two_prop") && !is.na(d) && abs(d) >= 0.5) shiny::updateNumericInput(session, "delta", value = 0.1)
      if (!(input$test %in% c("one_prop", "two_prop")) && !is.na(d) && abs(d) < 0.5 && isTRUE(input$sd == 1)) shiny::updateNumericInput(session, "delta", value = 1)
    }, ignoreInit = TRUE)
    args <- shiny::reactive(list(sd = num_or_na(input$sd) %||% 1, alpha = input$alpha, alternative = input$alt,
                                 p0 = input$p0, groups = input$groups, k = input$k, runs = input$runs,
                                 center = input$center %||% 0))
    res <- shiny::reactive({
      a <- args()
      shiny::validate(shiny::need(isTRUE(input$alpha > 0 && input$alpha < 0.5), "α must be between 0 and 0.5."))
      if (input$test %in% c("one_t", "two_t", "paired", "anova", "factorial"))
        shiny::validate(shiny::need(isTRUE(a$sd > 0), "Enter the standard deviation."))
      blanks <- c(n = is.na(num_or_na(input$n)), power = is.na(num_or_na(input$power)), delta = is.na(num_or_na(input$delta)))
      shiny::validate(shiny::need(sum(blanks) == 1, "Leave exactly one of sample size, power and difference blank."))
      validate_run(do.call(power_solve, c(list(test = input$test, n = num_or_na(input$n), power = num_or_na(input$power),
                                               delta = num_or_na(input$delta)), a)))
    })
    output$result <- shiny::renderUI({
      r <- res(); lab <- power_n_label(input$test)
      what <- if (is.na(num_or_na(input$n))) sprintf("%s needed: %d", lab, r$n)
              else if (is.na(num_or_na(input$power))) sprintf("Power: %.1f%%", 100 * r$power)
              else sprintf("Smallest detectable difference: %.4g", r$delta)
      extra <- if (input$test == "factorial") sprintf(" (%d runs in total)", r$n * input$runs + (input$center %||% 0))
               else if (input$test %in% c("two_t", "two_prop", "anova")) sprintf(" (%d in total)", r$n * (if (input$test == "anova") input$groups else 2))
               else ""
      shiny::div(class = "alert alert-info mb-2", shiny::tags$b(what), extra)
    })
    output$tab <- shiny::renderTable({
      r <- res()
      data.frame(Quantity = c(power_n_label(input$test), "Difference", "Power", "α"),
                 Value = c(format(r$n), signif(r$delta, 5), sprintf("%.4f", r$power), input$alpha))
    })
    output$curve <- shiny::renderPlot({
      r <- res(); a <- args()
      ns <- unique(pmax(if (input$test == "factorial") 1 else 2, round(r$n * c(0.5, 1, 2))))
      top <- if (input$test %in% c("one_prop", "two_prop")) min(abs(r$delta) * 2.5, 0.999 - input$p0) else abs(r$delta) * 2.5
      deltas <- seq(0, top, length.out = 120) * sign(r$delta %||% 1)
      do.call(power_curve_plot, c(list(test = input$test, ns = ns, deltas = deltas, target = r$power), a))
    })
  })
}
