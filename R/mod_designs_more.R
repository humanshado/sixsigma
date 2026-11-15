# DOE screens for general full factorials, split-plot designs, definitive
# screening and D-optimal designs.

create_buttons <- function(ns) shiny::tagList(
  shiny::checkboxInput(ns("randomize"), "Randomize run order", TRUE),
  blank_numeric(ns("seed"), "Random seed (optional)"),
  shiny::textInput(ns("response"), "Response column name", "Response"),
  shiny::actionButton(ns("create"), "Create design and open worksheet", class = "btn-primary", icon = shiny::icon("flask")))

# Numeric defaults for screens that need continuous factors.
numeric_factor_defaults <- function() within(default_factors, {
  name[6] <- "Additive"; low[6] <- "0"; high[6] <- "5"
})

notify_error <- function(expr) tryCatch(expr, error = function(e) { shiny::showNotification(conditionMessage(e), type = "error"); NULL })

# ---- General full factorial ---------------------------------------------------------
gff_defaults <- data.frame(name = c("Machine", "Material", "Speed", "Operator", "Shift", "Supplier"),
                           levels = c("M1, M2, M3", "Steel, Aluminium", "100, 150, 200", "Ann, Bob", "Day, Night", "S1, S2, S3"),
                           stringsAsFactors = FALSE)

gffCreateUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Create general full factorial design",
    shiny::tagList(
      shiny::numericInput(ns("k"), "Number of factors", 2, min = 1, max = 6),
      shiny::numericInput(ns("reps"), "Replicates", 2, min = 1, max = 10),
      shiny::checkboxInput(ns("block"), "Run each replicate as a block", FALSE),
      create_buttons(ns),
      shiny::helpText("Every combination of levels is run. Analyze the results with Models > General ANOVA ",
                      "(add Blocks as a factor if you blocked by replicate).")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Factors and levels"), shiny::uiOutput(ns("factor_inputs"))),
      bslib::card(bslib::card_header("Design summary"), shiny::tableOutput(ns("summary")))
    )
  )
}

gffCreateServer <- function(id, on_create) {
  shiny::moduleServer(id, function(input, output, session) {
    k <- shiny::reactive({
      shiny::validate(shiny::need(isTRUE(input$k >= 1 && input$k <= 6), "Choose 1 to 6 factors."))
      as.integer(input$k)
    })
    output$factor_inputs <- shiny::renderUI({
      rows <- lapply(seq_len(k()), function(i) {
        old <- function(what, def) shiny::isolate(input[[paste0(what, i)]]) %||% def
        d <- if (i <= nrow(gff_defaults)) gff_defaults[i, ] else data.frame(name = paste0("Factor", i), levels = "1, 2")
        shiny::fluidRow(class = "align-items-center",
          shiny::column(1, shiny::tags$b(LETTERS[i])),
          shiny::column(4, shiny::textInput(session$ns(paste0("name", i)), NULL, old("name", d$name))),
          shiny::column(7, shiny::textInput(session$ns(paste0("levels", i)), NULL, old("levels", d$levels))))
      })
      shiny::tagList(shiny::fluidRow(shiny::column(1), shiny::column(4, shiny::tags$b("Name")),
                                     shiny::column(7, shiny::tags$b("Levels (comma separated)"))), rows)
    })
    inputs <- shiny::reactive({
      get <- function(w) vapply(seq_len(k()), function(i) input[[paste0(w, i)]] %||% "", "")
      list(names = get("name"), levels = get("levels"))
    })
    output$summary <- shiny::renderTable({
      f <- inputs()
      nl <- vapply(f$levels, function(s) { v <- trimws(strsplit(s, ",", fixed = TRUE)[[1]]); sum(nzchar(v)) }, numeric(1))
      data.frame(Factor = f$names, Levels = as.integer(nl)) |>
        rbind(data.frame(Factor = "Total runs", Levels = as.integer(prod(nl) * (input$reps %||% 1))))
    })
    shiny::observeEvent(input$create, {
      f <- inputs()
      res <- notify_error(gff_create(f$names, f$levels, input$reps, input$randomize, num_or_na(input$seed),
                                     trimws(input$response), isTRUE(input$block)))
      if (is.null(res)) return()
      on_create(list(name = sprintf("General full factorial, %d runs", nrow(res)), df = res))
      shiny::showNotification("Design created. Enter results in the response column.", type = "message", duration = 6)
    })
  })
}

# ---- Split-plot ---------------------------------------------------------------------
splitplotCreateUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Create split-plot design",
    shiny::tagList(
      shiny::numericInput(ns("k"), "Number of factors", 3, min = 2, max = 7),
      shiny::selectInput(ns("htc"), "Hard-to-change factors", NULL, multiple = TRUE),
      shiny::numericInput(ns("wp_reps"), "Replicates of each whole plot", 2, min = 2, max = 6),
      create_buttons(ns),
      shiny::helpText("Hard-to-change factors (e.g. oven temperature) are set once per whole plot; easy-to-change ",
                      "factors are run in random order within each whole plot. Whole plots need at least two ",
                      "replicates so whole-plot error can be estimated.")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Factors (numeric low and high levels)"), shiny::uiOutput(ns("factor_inputs"))),
      bslib::card(bslib::card_header("Design summary"), shiny::tableOutput(ns("summary")))
    )
  )
}

splitplotCreateServer <- function(id, on_create) {
  shiny::moduleServer(id, function(input, output, session) {
    k <- shiny::reactive({
      shiny::validate(shiny::need(isTRUE(input$k >= 2 && input$k <= 7), "Choose 2 to 7 factors."))
      as.integer(input$k)
    })
    output$factor_inputs <- shiny::renderUI(factor_rows(session, input, k(), numeric_factor_defaults()))
    names_now <- shiny::reactive(vapply(seq_len(k()), function(i) trimws(input[[paste0("name", i)]] %||% numeric_factor_defaults()$name[i]), ""))
    shiny::observe({
      nm <- names_now()
      prev <- shiny::isolate(input$htc)
      sel <- if (length(intersect(prev, nm))) intersect(prev, nm) else nm[1]
      shiny::updateSelectInput(session, "htc", choices = nm, selected = sel)
    })
    output$summary <- shiny::renderTable({
      nh <- length(intersect(input$htc, names_now())); ne <- k() - nh
      data.frame(Item = c("Whole-plot factor settings", "Whole plots", "Runs per whole plot", "Total runs"),
                 Value = as.integer(c(2^nh, 2^nh * input$wp_reps, 2^ne, 2^nh * input$wp_reps * 2^ne)))
    })
    shiny::observeEvent(input$create, {
      res <- notify_error({
        f <- read_factor_rows(input, k())
        splitplot_create(f, f$name %in% input$htc, input$wp_reps, input$randomize, num_or_na(input$seed), trimws(input$response))
      })
      if (is.null(res)) return()
      on_create(list(name = sprintf("Split-plot design, %d runs", nrow(res)), df = res))
      shiny::showNotification("Split-plot design created. Runs are listed in whole-plot order.", type = "message", duration = 6)
    })
  })
}

splitplotAnalyzeUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Analyze split-plot design",
    shiny::tagList(
      column_picker(ns("y"), "Response"),
      column_picker(ns("factors"), "Factors (two-level)", multiple = TRUE),
      column_picker(ns("wp"), "Whole plot column"),
      shiny::selectInput(ns("order"), "Terms", c("Main effects and 2-way interactions" = 2, "Main effects only" = 1,
                                                 "Up to 3-way interactions" = 3))
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Fixed effects (REML)"), shiny::tableOutput(ns("table")),
                  shiny::helpText("Whole-plot terms are tested against whole-plot error, subplot terms against ",
                                  "subplot error, so their standard errors and degrees of freedom differ.")),
      bslib::card(bslib::card_header("Variance components"), shiny::tableOutput(ns("vc")))
    )
  )
}

splitplotAnalyzeServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      df <- data(); nm <- names(df)
      fac <- setdiff(two_level_cols(df), c("WholePlot", "RunOrder", "StdOrder"))
      update_pickers(session, df, list(y = list(type = "numeric", default = if ("Response" %in% nm) "Response" else utils::tail(numeric_cols(df), 1)),
                                       wp = list(type = "any", default = if ("WholePlot" %in% nm) "WholePlot" else 1)))
      shiny::updateSelectInput(session, "factors", choices = nm, selected = keep_selected(session, "factors", nm, fac))
    })
    res <- shiny::reactive({
      df <- data()
      f <- setdiff(intersect(input$factors, names(df)), c(input$y, input$wp))
      validate_run(splitplot_analyze(df, input$y, f, input$wp, as.integer(input$order)))
    })
    output$table <- shiny::renderTable(res()$table, digits = 4)
    output$vc <- shiny::renderTable(res()$vc, digits = 4)
  })
}

# ---- Definitive screening and D-optimal ---------------------------------------------
optimalCreateUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Create definitive screening / D-optimal design",
    shiny::tagList(
      shiny::selectInput(ns("type"), "Design", c("Definitive screening" = "dsd", "D-optimal" = "dopt")),
      shiny::numericInput(ns("k"), "Number of factors", 4, min = 1, max = 12),
      shiny::conditionalPanel("input.type == 'dsd'", ns = ns,
        shiny::numericInput(ns("extra"), "Extra center points", 0, min = 0, max = 10)),
      shiny::conditionalPanel("input.type == 'dopt'", ns = ns,
        shiny::selectInput(ns("model"), "Model to estimate", c("Quadratic (response surface)" = "quadratic",
                                                               "Main effects and 2-way interactions" = "interaction",
                                                               "Main effects only" = "linear")),
        shiny::numericInput(ns("runs"), "Number of runs", 12, min = 2, max = 200),
        shiny::textAreaInput(ns("constraints"), "Linear constraints (one per line, actual units)", "",
                             placeholder = "Temperature + 5*Time <= 300", rows = 3)),
      create_buttons(ns)
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Factors (numeric low and high levels)"), shiny::uiOutput(ns("factor_inputs"))),
      bslib::card(bslib::card_header("Design preview"), shiny::uiOutput(ns("info")), shiny::tableOutput(ns("preview")))
    )
  )
}

optimalCreateServer <- function(id, on_create) {
  shiny::moduleServer(id, function(input, output, session) {
    k <- shiny::reactive({
      lo <- if (identical(input$type, "dsd")) 3 else 1
      shiny::validate(shiny::need(isTRUE(input$k >= lo && input$k <= 12), sprintf("Choose %d to 12 factors.", lo)))
      as.integer(input$k)
    })
    output$factor_inputs <- shiny::renderUI(factor_rows(session, input, k(), numeric_factor_defaults()))
    build <- function(randomize, seed) {
      f <- read_factor_rows(input, k())
      if (identical(input$type, "dsd")) dsd_create(f, input$extra %||% 0, randomize, seed, trimws(input$response))
      else doptimal_create(f, input$runs, input$model, strsplit(input$constraints %||% "", "\n", fixed = TRUE)[[1]],
                           randomize = randomize, seed = seed, response = trimws(input$response))
    }
    preview <- shiny::reactive({
      shiny::req(input$type)
      validate_run(build(FALSE, 1))
    })
    output$info <- shiny::renderUI({
      d <- preview()
      if (identical(input$type, "dsd"))
        shiny::helpText(sprintf("%d runs. Main effects are orthogonal to each other and to all quadratic and two-factor ",
                                nrow(d)), "interaction effects. Analyze with Models > Stepwise regression (main effects first, ",
                        "then the largest quadratic or interaction terms).")
      else {
        e <- attr(d, "efficiency")
        shiny::helpText(sprintf("%d runs chosen from %d candidate points for a model with %d terms; D-efficiency %.1f%%. ",
                                nrow(d), e[["Candidates"]], e[["Terms"]], e[["D-efficiency (%)"]]),
                        "Analyze with Models > Regression or DOE > Analyze response surface.")
      }
    })
    output$preview <- shiny::renderTable({ d <- preview(); d[order(d$StdOrder), setdiff(names(d), c("RunOrder", trimws(input$response))), drop = FALSE] })
    shiny::observeEvent(input$create, {
      res <- notify_error(build(input$randomize, num_or_na(input$seed)))
      if (is.null(res)) return()
      nm <- if (identical(input$type, "dsd")) "Definitive screening design" else "D-optimal design"
      on_create(list(name = sprintf("%s, %d runs", nm, nrow(res)), df = res))
      shiny::showNotification("Design created. Enter results in the response column.", type = "message", duration = 6)
    })
  })
}
