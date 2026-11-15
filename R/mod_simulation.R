# Quality Tools > Tolerance stack-up, Monte Carlo simulation.

stack_defaults <- data.frame(name = c("Housing", "Shaft", "Washer", "Clip", "Spacer", "Cover"),
                             nominal = c(50, 30, 10, 9.8, 5, 2), tol = c(0.1, 0.05, 0.05, 0.05, 0.05, 0.05),
                             sign = c(1, -1, -1, -1, -1, -1), stringsAsFactors = FALSE)

stackupUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Tolerance stack-up",
    shiny::tagList(
      shiny::numericInput(ns("k"), "Number of dimensions in the loop", 4, min = 1, max = 20),
      shiny::numericInput(ns("lsl"), "Lower limit for the stack (e.g. minimum gap)", 0.05),
      shiny::numericInput(ns("usl"), "Upper limit for the stack", 0.45),
      shiny::numericInput(ns("cp"), "Process capability of normal dimensions (tolerance = 3σ × Cp)", 1, min = 0.3, max = 3, step = 0.1),
      shiny::numericInput(ns("n"), "Monte Carlo trials", 100000, min = 1000, max = 1e6, step = 10000),
      shiny::numericInput(ns("seed"), "Random seed (optional)", 1),
      shiny::downloadButton(ns("download"), "Download histogram")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Dimensions (+ adds to the stack, − subtracts)"), shiny::uiOutput(ns("rows"))),
      bslib::card(bslib::card_header("Stack limits"), shiny::uiOutput(ns("headline")), shiny::tableOutput(ns("limits")),
                  shiny::helpText("Worst case adds all tolerances; RSS assumes independent, centred dimensions; ",
                                  "Monte Carlo samples each dimension from its distribution.")),
      bslib::layout_columns(col_widths = c(6, 6),
        bslib::card(shiny::plotOutput(ns("hist"), height = 360)),
        bslib::card(shiny::plotOutput(ns("contrib"), height = 360))),
      bslib::card(bslib::card_header("Contributions"), shiny::tableOutput(ns("contrib_tab")))
    )
  )
}

stackupServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    k <- shiny::reactive({
      shiny::validate(shiny::need(isTRUE(input$k >= 1 && input$k <= 20), "Choose 1 to 20 dimensions."))
      as.integer(input$k)
    })
    output$rows <- shiny::renderUI({
      head <- shiny::fluidRow(shiny::column(3, shiny::tags$b("Name")), shiny::column(2, shiny::tags$b("Nominal")),
                              shiny::column(2, shiny::tags$b("− tol")), shiny::column(2, shiny::tags$b("+ tol")),
                              shiny::column(1, shiny::tags$b("±")), shiny::column(2, shiny::tags$b("Distribution")))
      rows <- lapply(seq_len(k()), function(i) {
        old <- function(w, d) shiny::isolate(input[[paste0(w, i)]]) %||% d
        d <- if (i <= nrow(stack_defaults)) stack_defaults[i, ] else data.frame(name = paste0("Dim", i), nominal = 1, tol = 0.05, sign = 1)
        id <- function(w) session$ns(paste0(w, i))
        shiny::fluidRow(class = "align-items-center",
          shiny::column(3, shiny::textInput(id("name"), NULL, old("name", d$name))),
          shiny::column(2, shiny::numericInput(id("nom"), NULL, old("nom", d$nominal))),
          shiny::column(2, shiny::numericInput(id("tm"), NULL, old("tm", d$tol), min = 0)),
          shiny::column(2, shiny::numericInput(id("tp"), NULL, old("tp", d$tol), min = 0)),
          shiny::column(1, shiny::selectInput(id("sign"), NULL, c("+" = 1, "−" = -1), old("sign", d$sign))),
          shiny::column(2, shiny::selectInput(id("dist"), NULL, c("Normal" = "normal", "Uniform" = "uniform"), old("dist", "normal"))))
      })
      shiny::tagList(head, rows)
    })
    parts <- shiny::reactive({
      g <- function(w) lapply(seq_len(k()), function(i) input[[paste0(w, i)]])
      shiny::req(length(g("dist")) == k(), all(!vapply(g("dist"), is.null, logical(1))))
      num <- function(w) vapply(g(w), function(v) if (is.null(v) || is.na(v)) NA_real_ else as.numeric(v), numeric(1))
      p <- data.frame(name = vapply(g("name"), function(v) v %||% "", ""), nominal = num("nom"), tol_minus = num("tm"),
                      tol_plus = num("tp"), sign = num("sign"), dist = unlist(g("dist")), stringsAsFactors = FALSE)
      shiny::validate(shiny::need(!anyNA(p[, 2:5]), "Fill in the nominal and both tolerances for every dimension."))
      p
    })
    res <- shiny::reactive({
      n <- input$n
      shiny::validate(shiny::need(isTRUE(n >= 1000 && n <= 1e6), "Use 1,000 to 1,000,000 trials."))
      validate_run(stackup(parts(), num_or_na(input$lsl), num_or_na(input$usl), n, num_or_na(input$seed), input$cp %||% 1))
    })
    output$headline <- shiny::renderUI(shiny::tags$p(sprintf("Nominal stack %.5g; mean %.5g; standard deviation %.4g.",
                                                             res()$nominal, res()$centre, res()$sd)))
    output$limits <- shiny::renderTable(res()$limits, digits = 5)
    output$contrib_tab <- shiny::renderTable(res()$contributions, digits = 4)
    output$hist <- shiny::renderPlot(sim_histogram(res(), "Monte Carlo stack distribution"))
    output$contrib <- shiny::renderPlot(stackup_plot(res()))
    output$download <- plot_download("stackup", function() sim_histogram(res(), "Monte Carlo stack distribution"))
  })
}

# ---- Monte Carlo simulation -------------------------------------------------------------
mc_defaults <- data.frame(name = c("R", "I", "X3", "X4", "X5", "X6"), dist = c("normal", "uniform", "normal", "normal", "normal", "normal"),
                          p1 = c(10, 1.9, 0, 0, 0, 0), p2 = c(0.2, 2.1, 1, 1, 1, 1), stringsAsFactors = FALSE)
mc_dists <- c("Normal" = "normal", "Uniform" = "uniform", "Triangular" = "triangular", "Lognormal" = "lognormal",
              "Weibull" = "weibull", "Exponential" = "exponential", "Constant" = "constant")

montecarloUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Monte Carlo simulation",
    shiny::tagList(
      shiny::numericInput(ns("k"), "Number of inputs", 2, min = 1, max = 12),
      shiny::textInput(ns("formula"), "Output Y =", "I^2 * R"),
      shiny::helpText("Use the input names with + − * / ^, exp, log, sqrt, abs, pmin, pmax, ifelse and similar functions."),
      shiny::numericInput(ns("lsl"), "LSL (optional)", 35),
      shiny::numericInput(ns("usl"), "USL (optional)", 45),
      shiny::numericInput(ns("n"), "Trials", 100000, min = 1000, max = 2e6, step = 10000),
      shiny::numericInput(ns("seed"), "Random seed (optional)", 1),
      shiny::actionButton(ns("run"), "Run simulation", class = "btn-primary", icon = shiny::icon("play")),
      shiny::downloadButton(ns("download"), "Download histogram")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Inputs"), shiny::uiOutput(ns("rows")),
                  shiny::helpText("Parameters: Normal (mean, SD); Uniform (min, max); Triangular (min, mode, max); ",
                                  "Lognormal (meanlog, sdlog); Weibull (shape, scale); Exponential (mean); Constant (value).")),
      bslib::layout_columns(col_widths = c(7, 5),
        bslib::card(shiny::plotOutput(ns("hist"), height = 380)),
        bslib::card(bslib::card_header("Output summary"), shiny::tableOutput(ns("summary")))),
      bslib::card(bslib::card_header("Sensitivity (share of output variation)"), shiny::tableOutput(ns("sens")),
                  shiny::helpText("Contribution = squared Spearman rank correlation with Y, scaled to 100%. ",
                                  "Reduce the variation of the top inputs first."))
    )
  )
}

montecarloServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    k <- shiny::reactive({
      shiny::validate(shiny::need(isTRUE(input$k >= 1 && input$k <= 12), "Choose 1 to 12 inputs."))
      as.integer(input$k)
    })
    output$rows <- shiny::renderUI({
      head <- shiny::fluidRow(shiny::column(3, shiny::tags$b("Name")), shiny::column(3, shiny::tags$b("Distribution")),
                              shiny::column(2, shiny::tags$b("Param 1")), shiny::column(2, shiny::tags$b("Param 2")),
                              shiny::column(2, shiny::tags$b("Param 3")))
      rows <- lapply(seq_len(k()), function(i) {
        old <- function(w, d) shiny::isolate(input[[paste0(w, i)]]) %||% d
        d <- if (i <= nrow(mc_defaults)) mc_defaults[i, ] else data.frame(name = paste0("X", i), dist = "normal", p1 = 0, p2 = 1)
        id <- function(w) session$ns(paste0(w, i))
        shiny::fluidRow(class = "align-items-center",
          shiny::column(3, shiny::textInput(id("name"), NULL, old("name", d$name))),
          shiny::column(3, shiny::selectInput(id("dist"), NULL, mc_dists, old("dist", d$dist))),
          shiny::column(2, shiny::numericInput(id("p1"), NULL, old("p1", d$p1))),
          shiny::column(2, shiny::numericInput(id("p2"), NULL, old("p2", d$p2))),
          shiny::column(2, shiny::numericInput(id("p3"), NULL, old("p3", NA))))
      })
      shiny::tagList(head, rows)
    })
    res <- shiny::eventReactive(list(input$run, is.null(input$name1)), {
      shiny::req(!is.null(input$name1))
      g <- function(w) lapply(seq_len(k()), function(i) input[[paste0(w, i)]])
      num <- function(w) vapply(g(w), function(v) if (is.null(v) || is.na(v)) NA_real_ else as.numeric(v), numeric(1))
      inp <- data.frame(name = vapply(g("name"), function(v) trimws(v %||% ""), ""),
                        dist = vapply(g("dist"), function(v) v %||% "normal", ""),
                        p1 = num("p1"), p2 = num("p2"), p3 = num("p3"), stringsAsFactors = FALSE)
      need <- vapply(inp$dist, function(d) sum(nzchar(sim_param_names[[d]])), numeric(1))
      ok <- vapply(seq_len(nrow(inp)), function(i) !anyNA(unlist(inp[i, c("p1", "p2", "p3")])[seq_len(need[i])]), logical(1))
      shiny::validate(shiny::need(all(ok), "Fill in the parameters each distribution needs."))
      validate_run(monte_carlo(inp, input$formula, input$n, num_or_na(input$lsl), num_or_na(input$usl), num_or_na(input$seed)))
    }, ignoreNULL = FALSE)
    output$hist <- shiny::renderPlot(sim_histogram(res()))
    output$summary <- shiny::renderTable(res()$summary, digits = 5, na = "")
    output$sens <- shiny::renderTable(res()$sensitivity, digits = 3)
    output$download <- plot_download("monte-carlo", function() sim_histogram(res()))
  })
}
