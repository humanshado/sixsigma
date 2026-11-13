# DOE > Response surface designs (create and analyze).

# Rows of name / low / high inputs for k factors, keeping typed values.
factor_rows <- function(session, input, k, defaults = default_factors, levels = TRUE) {
  header <- shiny::fluidRow(
    shiny::column(1), shiny::column(if (levels) 5 else 11, shiny::tags$b("Name")),
    if (levels) shiny::column(3, shiny::tags$b("Low")), if (levels) shiny::column(3, shiny::tags$b("High")))
  rows <- lapply(seq_len(k), function(i) {
    old <- function(what, def) shiny::isolate(input[[paste0(what, i)]]) %||% def
    dflt <- if (i <= nrow(defaults)) defaults[i, ] else data.frame(name = paste0("Factor", i), low = "-1", high = "1")
    shiny::fluidRow(
      class = "align-items-center",
      shiny::column(1, shiny::tags$b(LETTERS[i])),
      shiny::column(if (levels) 5 else 11, shiny::textInput(session$ns(paste0("name", i)), NULL, old("name", dflt$name))),
      if (levels) shiny::column(3, shiny::textInput(session$ns(paste0("low", i)), NULL, old("low", dflt$low))),
      if (levels) shiny::column(3, shiny::textInput(session$ns(paste0("high", i)), NULL, old("high", dflt$high))))
  })
  shiny::tagList(header, rows)
}

read_factor_rows <- function(input, k, numeric_levels = TRUE) {
  get <- function(what) vapply(seq_len(k), function(i) trimws(input[[paste0(what, i)]] %||% ""), "")
  nm <- get("name")
  if (!numeric_levels) return(data.frame(name = nm, stringsAsFactors = FALSE))
  lo <- suppressWarnings(as.numeric(get("low"))); hi <- suppressWarnings(as.numeric(get("high")))
  if (any(is.na(c(lo, hi)))) stop("Enter numeric low and high levels for every factor.")
  data.frame(name = nm, low = lo, high = hi, stringsAsFactors = FALSE)
}

rsmCreateUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Create response surface design",
    shiny::tagList(
      shiny::selectInput(ns("type"), "Design", c("Central composite (CCD)" = "ccd", "Box-Behnken" = "bbd")),
      shiny::numericInput(ns("k"), "Number of factors", 2, min = 2, max = 6),
      shiny::conditionalPanel("input.type == 'ccd'", ns = ns,
        shiny::selectInput(ns("alpha"), "Axial points",
                           c("Rotatable (α = F^¼)" = "rotatable", "Face-centred (α = 1)" = "face")),
        shiny::checkboxInput(ns("fraction"), "Half-fraction factorial part (5+ factors)", FALSE)),
      shiny::numericInput(ns("center"), "Center points", 5, min = 1, max = 20),
      shiny::numericInput(ns("reps"), "Replicates", 1, min = 1, max = 5),
      shiny::checkboxInput(ns("randomize"), "Randomize run order", TRUE),
      blank_numeric(ns("seed"), "Random seed (optional)"),
      shiny::textInput(ns("response"), "Response column name", "Response"),
      shiny::actionButton(ns("create"), "Create design and open worksheet", class = "btn-primary", icon = shiny::icon("flask"))
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Factors"),
                  shiny::helpText("Low and high are the factorial levels (coded -1 and +1). Rotatable CCD axial points ",
                                  "fall outside this range; face-centred designs and Box-Behnken designs stay inside it."),
                  shiny::uiOutput(ns("factor_inputs"))),
      bslib::card(bslib::card_header("Design summary"), shiny::tableOutput(ns("summary"))),
      bslib::card(bslib::card_header("Design points (coded units)"), shiny::tableOutput(ns("preview")))
    )
  )
}

rsmCreateServer <- function(id, on_create) {
  shiny::moduleServer(id, function(input, output, session) {
    k <- shiny::reactive({
      lim <- if (input$type == "bbd") c(3, 5) else c(2, 6)
      shiny::validate(shiny::need(isTRUE(input$k >= lim[1] && input$k <= lim[2] && input$k == round(input$k)),
                                  sprintf("Choose %d to %d factors for this design.", lim[1], lim[2])))
      as.integer(input$k)
    })
    shiny::observeEvent(input$type, if (input$type == "bbd" && isTRUE(input$k < 3)) shiny::updateNumericInput(session, "k", value = 3))
    output$factor_inputs <- shiny::renderUI(factor_rows(session, input, k()))
    design <- shiny::reactive({
      ct <- max(1, input$center %||% 1)
      validate_run(if (input$type == "ccd") rsm_ccd(k(), input$alpha, ct, isTRUE(input$fraction)) else rsm_bbd(k(), ct))
    })
    output$summary <- shiny::renderTable({
      d <- design(); reps <- max(1, input$reps %||% 1)
      tab <- table(d$type)
      data.frame(Design = if (input$type == "ccd") "Central composite" else "Box-Behnken", Factors = k(),
                 `Points per replicate` = paste(sprintf("%d %s", as.integer(tab), tolower(names(tab))), collapse = ", "),
                 `Total runs` = nrow(d$matrix) * reps, Alpha = round(d$alpha, 4), check.names = FALSE)
    })
    output$preview <- shiny::renderTable({
      d <- design(); m <- as.data.frame(round(d$matrix, 4)); cbind(Type = d$type, m)
    })
    shiny::observeEvent(input$create, {
      res <- tryCatch({
        f <- read_factor_rows(input, k())
        rsm_worksheet(design(), f, replicates = input$reps, randomize = input$randomize,
                      seed = num_or_na(input$seed), response = trimws(input$response))
      }, error = function(e) { shiny::showNotification(conditionMessage(e), type = "error"); NULL })
      if (is.null(res)) return()
      on_create(list(name = sprintf("Response surface design, %d runs", nrow(res)), df = res))
      shiny::showNotification(sprintf("Created %d runs. Enter results in the '%s' column.", nrow(res), input$response),
                              type = "message", duration = 8)
    })
  })
}

rsmAnalyzeUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Analyze response surface",
    shiny::tagList(
      column_picker(ns("y"), "Response"),
      column_picker(ns("factors"), "Factors (numeric)", multiple = TRUE),
      shiny::selectInput(ns("order"), "Model", c("Full quadratic" = "quadratic", "Linear + interactions" = "interaction",
                                                 "Linear (first order)" = "linear")),
      shiny::tags$hr(),
      shiny::tags$b("Contour and surface plots"),
      shiny::selectInput(ns("f1"), "Horizontal axis", NULL),
      shiny::selectInput(ns("f2"), "Vertical axis", NULL),
      shiny::uiOutput(ns("hold_ui")),
      shiny::sliderInput(ns("theta"), "Surface rotation", -180, 180, 35, step = 5),
      shiny::tags$hr(),
      shiny::tags$b("Path of steepest ascent"),
      shiny::radioButtons(ns("dir"), NULL, c("Ascent (maximize)" = "ascent", "Descent (minimize)" = "descent"), inline = TRUE),
      shiny::numericInput(ns("steps"), "Steps", 6, min = 1, max = 30),
      shiny::downloadButton(ns("download"), "Download contour plot")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Model summary"), shiny::tableOutput(ns("summary")),
                  shiny::helpText("Factors are coded so the range of the data spans -1 to +1.")),
      bslib::card(bslib::card_header("Coefficients (coded units)"), shiny::tableOutput(ns("coef"))),
      bslib::card(bslib::card_header("Analysis of variance"), shiny::tableOutput(ns("anova")),
                  shiny::helpText("A significant lack of fit (small p-value) means the model misses curvature or other structure.")),
      shiny::uiOutput(ns("stationary_card")),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(shiny::plotOutput(ns("contour"), height = 430)),
        bslib::card(shiny::plotOutput(ns("surface"), height = 430))
      ),
      bslib::card(bslib::card_header("Path of steepest ascent / descent"), shiny::tableOutput(ns("steepest")),
                  shiny::helpText("Computed from a first-order (linear) model of the same data, whatever model is chosen above; ",
                                  "use it to move towards a better region before running a response surface design. ",
                                  "Steps move along the gradient; the factor with ",
                                  "the largest effect changes by one coded unit (half its range) per step. Predictions ",
                                  "beyond the experimental region are extrapolations: run confirmation experiments."))
    )
  )
}

rsmAnalyzeServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      df <- data()
      nums <- numeric_cols(df)
      skip <- c("StdOrder", "RunOrder", "CenterPt", "Run")
      cand <- setdiff(nums, skip)
      update_pickers(session, df, list(y = list(type = "numeric", default = utils::tail(cand, 1))))
      shiny::updateSelectInput(session, "factors", choices = nums,
                               selected = keep_selected(session, "factors", nums, utils::head(setdiff(cand, utils::tail(cand, 1)), 3)))
    })
    model <- shiny::reactive({
      df <- data(); shiny::req(input$y %in% names(df))
      validate_run(rsm_fit(df, input$y, intersect(input$factors, names(df)), input$order))
    })
    shiny::observeEvent(model(), {
      f <- model()$factors
      shiny::updateSelectInput(session, "f1", choices = f, selected = if (isTRUE(input$f1 %in% f)) input$f1 else f[1])
      shiny::updateSelectInput(session, "f2", choices = f,
                               selected = if (isTRUE(input$f2 %in% f) && input$f2 != input$f1) input$f2 else f[min(2, length(f))])
    })
    output$hold_ui <- shiny::renderUI({
      m <- model(); others <- setdiff(m$factors, c(input$f1, input$f2))
      if (!length(others)) return(NULL)
      shiny::tagList(lapply(others, function(f) {
        cd <- m$coding[[f]]
        shiny::sliderInput(session$ns(paste0("hold_", make.names(f))), paste("Hold", f, "at"),
                           min = signif(cd$mid - cd$half, 4), max = signif(cd$mid + cd$half, 4),
                           value = signif(cd$mid, 4), step = signif(cd$half / 20, 2))
      }))
    })
    hold <- shiny::reactive({
      m <- model(); others <- setdiff(m$factors, c(input$f1, input$f2))
      stats::setNames(lapply(others, function(f) {
        v <- input[[paste0("hold_", make.names(f))]]; cd <- m$coding[[f]]
        if (is.null(v)) 0 else (v - cd$mid) / cd$half
      }), others)
    })
    output$summary <- shiny::renderTable(int_cols(rsm_summary(model())), digits = 4)
    output$coef <- shiny::renderTable(rsm_coefficients(model()), digits = 4)
    output$anova <- shiny::renderTable(int_cols(rsm_anova(model())), digits = 4, na = "")
    output$stationary_card <- shiny::renderUI({
      s <- rsm_stationary(model())
      if (is.null(s)) return(NULL)
      bslib::card(bslib::card_header("Stationary point (canonical analysis)"),
                  shiny::div(class = "alert alert-info mb-2", s$note),
                  if (!is.null(s$point)) shiny::tableOutput(session$ns("stationary")),
                  if (!is.null(s$eigenvalues)) shiny::helpText(sprintf(
                    "Eigenvalues: %s. All negative = maximum, all positive = minimum, mixed signs = saddle.",
                    paste(signif(s$eigenvalues, 4), collapse = ", "))))
    })
    output$stationary <- shiny::renderTable(rsm_stationary(model())$point, digits = 5)
    plot_axes_ok <- shiny::reactive({
      shiny::validate(shiny::need(length(model()$factors) >= 2, "Contour plots need at least two factors."),
                      shiny::need(isTRUE(input$f1 != input$f2), "Choose two different factors for the axes."),
                      shiny::need(all(c(input$f1, input$f2) %in% model()$factors), "Choose plot axes from the model's factors."))
      TRUE
    })
    contour <- function() {
      plot_axes_ok(); m <- model(); df <- data()
      raw <- data.frame(x = df[[input$f1]], y = df[[input$f2]])
      rsm_contour_plot(m, input$f1, input$f2, hold(), raw[stats::complete.cases(raw), ])
    }
    output$contour <- shiny::renderPlot(contour())
    output$surface <- shiny::renderPlot({ plot_axes_ok(); rsm_surface_plot(model(), input$f1, input$f2, hold(), theta = input$theta) })
    # Steepest ascent is a first-order method: always use a linear fit.
    output$steepest <- shiny::renderTable({
      m <- model(); df <- data()
      lin <- validate_run(rsm_fit(df, m$response, m$factors, "linear"))
      validate_run(rsm_steepest(lin, input$steps, input$dir))
    }, digits = 5)
    output$download <- plot_download("contour-plot", contour, height = 6)
  })
}
