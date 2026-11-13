# DOE > Create factorial design, DOE > Analyze factorial design.

default_factors <- data.frame(
  name = c("Temperature", "Time", "Pressure", "Concentration", "Speed", "Catalyst", "pH", "FeedRate"),
  low = c("150", "10", "1", "5", "100", "A", "6", "2"),
  high = c("200", "20", "2", "10", "200", "B", "8", "4"),
  stringsAsFactors = FALSE
)

doeCreateUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Create factorial design",
    shiny::tagList(
      shiny::numericInput(ns("k"), "Number of factors", 3, min = 2, max = DOE_MAX_FACTORS),
      shiny::helpText("Up to 8 factors for full and fractional factorials; up to 23 with Plackett-Burman screening designs."),
      shiny::selectInput(ns("runs"), "Design", choices = NULL),
      shiny::numericInput(ns("reps"), "Replicates", 1, min = 1, max = 10),
      shiny::numericInput(ns("center"), "Center points", 0, min = 0, max = 20),
      shiny::checkboxInput(ns("randomize"), "Randomize run order", TRUE),
      blank_numeric(ns("seed"), "Random seed (optional, for a repeatable order)"),
      shiny::textInput(ns("response"), "Response column name", "Response"),
      shiny::actionButton(ns("create"), "Create design and open worksheet", class = "btn-primary", icon = shiny::icon("flask"))
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Factors"),
                  shiny::helpText("Levels can be numbers (e.g. 150 and 200) or text (e.g. A and B). ",
                                  "Center points need numeric levels."),
                  shiny::uiOutput(ns("factor_inputs"))),
      bslib::card(bslib::card_header("Design summary"), shiny::tableOutput(ns("summary")),
                  shiny::uiOutput(ns("defining"))),
      bslib::card(bslib::card_header("Alias structure"), shiny::tableOutput(ns("aliases")),
                  shiny::helpText("Aliased effects cannot be told apart in this design. Resolution III designs ",
                                  "alias main effects with two-factor interactions; resolution IV aliases ",
                                  "two-factor interactions with each other; resolution V and full factorials ",
                                  "estimate all main effects and two-factor interactions separately."))
    )
  )
}

doeCreateServer <- function(id, on_create) {
  shiny::moduleServer(id, function(input, output, session) {
    k <- shiny::reactive({
      k <- input$k
      shiny::validate(shiny::need(isTRUE(k >= 2 && k <= DOE_MAX_FACTORS && k == round(k)),
                                  sprintf("Choose between 2 and %d factors.", DOE_MAX_FACTORS)))
      as.integer(k)
    })
    shiny::observeEvent(k(), {
      opts <- doe_options(k())
      full <- 2^k()
      # Default to the full factorial for up to 4 factors, otherwise the
      # smallest design of resolution V or higher (or the largest available).
      res_ok <- grepl("Full|resolution (V|VI|VII)", names(opts))
      sel <- if (k() <= 4) full else if (any(res_ok)) opts[res_ok][1] else utils::tail(opts, 1)
      shiny::updateSelectInput(session, "runs", choices = opts, selected = sel)
    })

    output$factor_inputs <- shiny::renderUI({
      n <- k()
      header <- shiny::fluidRow(
        shiny::column(1), shiny::column(5, shiny::tags$b("Name")),
        shiny::column(3, shiny::tags$b("Low level")), shiny::column(3, shiny::tags$b("High level")))
      rows <- lapply(seq_len(n), function(i) {
        old <- function(what, def) shiny::isolate(input[[paste0(what, i)]]) %||% def
        dflt <- if (i <= nrow(default_factors)) default_factors[i, ] else
          data.frame(name = paste0("Factor", i), low = "-1", high = "1")
        shiny::fluidRow(
          class = "align-items-center",
          shiny::column(1, shiny::tags$b(LETTERS[i])),
          shiny::column(5, shiny::textInput(session$ns(paste0("name", i)), NULL, old("name", dflt$name))),
          shiny::column(3, shiny::textInput(session$ns(paste0("low", i)), NULL, old("low", dflt$low))),
          shiny::column(3, shiny::textInput(session$ns(paste0("high", i)), NULL, old("high", dflt$high)))
        )
      })
      rows <- c(list(header), rows)
      shiny::tagList(rows)
    })

    factors <- shiny::reactive({
      n <- k()
      get <- function(what) vapply(seq_len(n), function(i) trimws(input[[paste0(what, i)]] %||% ""), "")
      nm <- get("name"); lo <- get("low"); hi <- get("high")
      shiny::req(length(nm) == n)
      shiny::validate(shiny::need(all(nzchar(c(nm, lo, hi))), "Enter a name and two levels for every factor."))
      lo_n <- suppressWarnings(as.numeric(lo)); hi_n <- suppressWarnings(as.numeric(hi))
      if (all(!is.na(c(lo_n, hi_n)))) data.frame(name = nm, low = lo_n, high = hi_n)
      else data.frame(name = nm, low = lo, high = hi, stringsAsFactors = FALSE)
    })

    runs <- shiny::reactive({
      r <- suppressWarnings(as.integer(input$runs))
      shiny::req(!is.na(r), r %in% doe_options(k()))
      r
    })

    output$summary <- shiny::renderTable(int_cols({
      a <- doe_aliases(k(), runs())
      reps <- max(1, input$reps %||% 1); ct <- max(0, input$center %||% 0)
      data.frame(Factors = k(), `Base runs` = runs(), Replicates = reps, `Center points` = ct,
                 `Total runs` = runs() * reps + ct, Resolution = a$resolution,
                 Fraction = if (runs() == 2^k()) "Full" else if (is_pb(k(), runs())) "Plackett-Burman"
                            else sprintf("1/%d", 2^k() / runs()), check.names = FALSE)
    }))
    output$defining <- shiny::renderUI({
      a <- doe_aliases(k(), runs())
      gens <- doe_coded(k(), runs())$generators
      shiny::tags$p(
        if (length(gens)) shiny::tagList(shiny::tags$b("Generators: "),
                                         paste(sprintf("%s = %s", names(gens), gens), collapse = ", "), shiny::tags$br()),
        shiny::tags$b("Defining relation: "), a$defining)
    })
    output$aliases <- shiny::renderTable({
      f <- factors()
      a <- doe_aliases(k(), runs(), f$name)
      if (runs() == 2^k()) data.frame(Result = "Full factorial: every effect is estimated separately.") else a$table
    })

    shiny::observeEvent(input$create, {
      f <- tryCatch(factors(), error = function(e) NULL)
      if (is.null(f)) return(shiny::showNotification("Enter a name and two levels for every factor.", type = "error"))
      d <- tryCatch(
        doe_create(f, runs(), replicates = input$reps, center = input$center, randomize = input$randomize,
                   seed = num_or_na(input$seed), response = input$response),
        error = function(e) { shiny::showNotification(conditionMessage(e), type = "error"); NULL })
      if (is.null(d)) return()
      label <- sprintf("%d-factor design, %d runs", nrow(f), nrow(d))
      on_create(list(name = label, df = d))
      shiny::showNotification(sprintf("Created a %s. Enter your measurements in the '%s' column.", label, input$response),
                              type = "message", duration = 8)
    })
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Analysis -------------------------------------------------------------------

doeAnalyzeUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Analyze factorial design",
    shiny::tagList(
      column_picker(ns("y"), "Response"),
      column_picker(ns("factors"), "Factors", multiple = TRUE),
      shiny::helpText("Each factor needs exactly two levels (plus an optional center value). ",
                      "Levels can be numbers, text, or -/+."),
      shiny::selectInput(ns("order"), "Include terms up to", c("Main effects only" = 1, "2-factor interactions" = 2,
                                                               "3-factor interactions" = 3, "All interactions" = 99),
                         selected = 2),
      shiny::numericInput(ns("alpha"), "Significance level (α)", 0.05, min = 0.001, max = 0.5, step = 0.01),
      shiny::downloadButton(ns("download"), "Download effects plots"),
      shiny::helpText("Try the ss.data.doe1 sample dataset (pizza dough experiment).")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Model"), shiny::uiOutput(ns("method")),
                  shiny::tableOutput(ns("summary")), shiny::tags$pre(shiny::textOutput(ns("equation")))),
      bslib::card(bslib::card_header("Effects and coefficients (coded units)"), shiny::tableOutput(ns("effects")),
                  shiny::helpText("Effect = change in the response going from the low to the high level. ",
                                  "Coefficient = half the effect.")),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(shiny::plotOutput(ns("pareto"), height = 420)),
        bslib::card(shiny::plotOutput(ns("normal"), height = 420))
      ),
      bslib::card(shiny::plotOutput(ns("main"), height = 340)),
      bslib::card(shiny::plotOutput(ns("interaction"), height = 480)),
      shiny::uiOutput(ns("resid_card")),
      bslib::card(bslib::card_header("Factor levels"), shiny::tableOutput(ns("levels")))
    )
  )
}

# Columns that look like two-level factors (2 levels, or 3 with a center value).
two_level_cols <- function(df, exclude = character(0)) {
  skip <- c("StdOrder", "RunOrder", "CenterPt", "Blocks", "repl", "ord", exclude)
  names(df)[vapply(names(df), function(n) {
    if (n %in% skip) return(FALSE)
    ok <- tryCatch({ doe_code_column(df[[n]], n); TRUE }, error = function(e) FALSE)
    ok && length(unique(stats::na.omit(df[[n]]))) >= 2
  }, logical(1))]
}

doeAnalyzeServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      df <- data()
      nums <- numeric_cols(df)
      resp_guess <- setdiff(nums, two_level_cols(df))
      resp_guess <- setdiff(resp_guess, c("StdOrder", "RunOrder", "CenterPt", "repl", "ord"))
      update_pickers(session, df, list(y = list(type = "numeric", default = if (length(resp_guess)) utils::tail(resp_guess, 1) else 1)))
      fac <- two_level_cols(df, exclude = shiny::isolate(input$y))
      shiny::updateSelectInput(session, "factors", choices = names(df),
                               selected = keep_selected(session, "factors", names(df), fac))
    })

    res <- shiny::reactive({
      df <- data()
      shiny::req(input$y %in% names(df))
      f <- intersect(input$factors, names(df))
      a <- input$alpha
      shiny::validate(shiny::need(isTRUE(a > 0 && a < 1), "The significance level must be between 0 and 1."))
      validate_run(doe_analyze(df, input$y, f, as.integer(input$order), a))
    })

    output$method <- shiny::renderUI({
      r <- res()
      shiny::tagList(
        shiny::tags$p(shiny::tags$b("Significance: "), r$method),
        if (length(r$aliased)) shiny::tags$p(shiny::tags$b("Removed because aliased with other terms: "),
                                             paste(r$aliased, collapse = ", ")),
        {
          sig <- r$table$Term[r$table$Significant == "Yes"]
          shiny::div(class = "alert alert-info mb-2",
                     if (length(sig)) paste("Significant terms at α =", r$alpha, ":", paste(sig, collapse = ", "))
                     else paste("No terms are significant at α =", r$alpha))
        }
      )
    })
    output$summary <- shiny::renderTable(int_cols(res()$summary), digits = 4, na = "")
    output$equation <- shiny::renderText(paste("Coded equation:", doe_equation(res())))
    output$effects <- shiny::renderTable(res()$table[, setdiff(names(res()$table), "Code")], digits = 4, na = "")
    output$pareto <- shiny::renderPlot(doe_pareto_plot(res()))
    output$normal <- shiny::renderPlot(doe_normal_plot(res()))
    output$main <- shiny::renderPlot(doe_main_effects_plot(res()))
    output$interaction <- shiny::renderPlot(doe_interaction_plot(res()))
    output$levels <- shiny::renderTable(res()$levels)

    output$resid_card <- shiny::renderUI({
      if (stats::df.residual(res()$fit) < 1) return(NULL)
      bslib::card(bslib::card_header("Residual plots"), shiny::plotOutput(session$ns("resid"), height = 520))
    })
    output$resid <- shiny::renderPlot(draw_grid(reg_residual_plots(res()$fit)))

    output$download <- plot_download("doe-effects", function() {
      r <- res()
      draw_grid(list(doe_pareto_plot(r), doe_normal_plot(r), doe_main_effects_plot(r), doe_interaction_plot(r)))
    }, width = 14, height = 10)
  })
}
