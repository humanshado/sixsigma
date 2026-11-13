# DOE > Taguchi designs and mixture designs (create and analyze).

taguchiCreateUI <- function(id) {
  ns <- shiny::NS(id)
  choices <- stats::setNames(names(oa_tables), sprintf("%s - %s", names(oa_tables), vapply(oa_tables, `[[`, "", "levels")))
  analysis_layout(
    "Create Taguchi design",
    shiny::tagList(
      shiny::selectInput(ns("array"), "Orthogonal array", choices, selected = "L9"),
      shiny::numericInput(ns("k"), "Number of factors", 3, min = 1),
      shiny::numericInput(ns("reps"), "Repeated measurements per run (noise replicates)", 2, min = 1, max = 10),
      shiny::textInput(ns("response"), "Response column prefix", "Y"),
      shiny::actionButton(ns("create"), "Create design and open worksheet", class = "btn-primary", icon = shiny::icon("flask")),
      shiny::helpText("Factors are assigned to the first columns of the array. Taguchi runs are not randomized ",
                      "here; randomize the run order when you carry out the experiment.")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Factors"), shiny::uiOutput(ns("factor_inputs"))),
      bslib::card(bslib::card_header("Orthogonal array"), shiny::tableOutput(ns("array_tab")))
    )
  )
}

taguchiCreateServer <- function(id, on_create) {
  shiny::moduleServer(id, function(input, output, session) {
    ncols <- shiny::reactive(ncol(taguchi_array(input$array)))
    k <- shiny::reactive({
      shiny::validate(shiny::need(isTRUE(input$k >= 1 && input$k <= ncols()),
                                  sprintf("%s has %d columns: choose 1 to %d factors.", input$array, ncols(), ncols())))
      as.integer(input$k)
    })
    shiny::observeEvent(input$array, shiny::updateNumericInput(session, "k", max = ncols(),
                                                                 value = min(input$k %||% 3, ncols())))
    output$factor_inputs <- shiny::renderUI(factor_rows(session, input, k(), levels = FALSE))
    output$array_tab <- shiny::renderTable({
      m <- taguchi_array(input$array); data.frame(Run = seq_len(nrow(m)), m)
    })
    shiny::observeEvent(input$create, {
      res <- tryCatch({
        f <- read_factor_rows(input, k(), numeric_levels = FALSE)
        taguchi_create(input$array, f$name, replicates = input$reps, response = trimws(input$response))
      }, error = function(e) { shiny::showNotification(conditionMessage(e), type = "error"); NULL })
      if (is.null(res)) return()
      on_create(list(name = sprintf("Taguchi %s design", input$array), df = res))
      shiny::showNotification("Design created. Factor columns hold level numbers; enter the measurements in the response columns.",
                              type = "message", duration = 8)
    })
  })
}

taguchiAnalyzeUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Analyze Taguchi design",
    shiny::tagList(
      column_picker(ns("factors"), "Factor columns", multiple = TRUE),
      column_picker(ns("responses"), "Response columns (one per repeat)", multiple = TRUE),
      shiny::selectInput(ns("type"), "Signal-to-noise ratio", sn_types),
      shiny::downloadButton(ns("download"), "Download main effects plot")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Best settings"), shiny::tableOutput(ns("best")), shiny::uiOutput(ns("pred"))),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(bslib::card_header("Response table for S/N ratios"), shiny::tableOutput(ns("sn_tab"))),
        bslib::card(bslib::card_header("Response table for means"), shiny::tableOutput(ns("mean_tab")))
      ),
      bslib::card(shiny::plotOutput(ns("plot"), height = 480)),
      bslib::card(bslib::card_header("S/N ratio and mean per run"), shiny::tableOutput(ns("runs")))
    )
  )
}

taguchiAnalyzeServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      df <- data(); nm <- names(df)
      resp <- nm[grepl("^Y[0-9]*$|^Response", nm)]
      if (!length(resp)) resp <- utils::tail(numeric_cols(df), 1)
      fac <- setdiff(nm, c(resp, "Run", "StdOrder", "RunOrder"))
      shiny::updateSelectInput(session, "factors", choices = nm, selected = keep_selected(session, "factors", nm, fac))
      shiny::updateSelectInput(session, "responses", choices = nm, selected = keep_selected(session, "responses", nm, resp))
    })
    res <- shiny::reactive({
      df <- data()
      f <- intersect(input$factors, names(df)); r <- intersect(input$responses, names(df))
      shiny::validate(shiny::need(!length(intersect(f, r)), "A column cannot be both a factor and a response."))
      validate_run(taguchi_analyze(df, f, r, input$type))
    })
    output$best <- shiny::renderTable(res()$best)
    output$pred <- shiny::renderUI(shiny::helpText(sprintf(
      "Predicted at the best levels (additive model): S/N = %.3f, mean = %.4g. Confirm with a verification run.",
      res()$predicted[["sn"]], res()$predicted[["mean"]])))
    output$sn_tab <- shiny::renderTable(res()$sn_table$table, digits = 4, na = "")
    output$mean_tab <- shiny::renderTable(res()$mean_table$table, digits = 4, na = "")
    output$plot <- shiny::renderPlot(taguchi_effects_plot(res()))
    output$runs <- shiny::renderTable({
      df <- data(); r <- res()
      cbind(df[, r$factors, drop = FALSE], `S/N` = r$sn, Mean = r$mean)
    }, digits = 4)
    output$download <- plot_download("taguchi-effects", function() taguchi_effects_plot(res()), height = 6)
  })
}

# ---- Mixtures ---------------------------------------------------------------------

mixtureCreateUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Create mixture design",
    shiny::tagList(
      shiny::textInput(ns("components"), "Components (comma separated)", "A, B, C"),
      shiny::selectInput(ns("type"), "Design", c("Simplex centroid" = "centroid", "Simplex lattice" = "lattice")),
      shiny::conditionalPanel("input.type == 'lattice'", ns = ns,
        shiny::numericInput(ns("degree"), "Lattice degree (proportions in steps of 1/degree)", 2, min = 1, max = 6)),
      shiny::checkboxInput(ns("axial"), "Add axial check blends (and the overall centroid)", TRUE),
      shiny::numericInput(ns("reps"), "Replicates", 1, min = 1, max = 5),
      shiny::checkboxInput(ns("randomize"), "Randomize run order", TRUE),
      blank_numeric(ns("seed"), "Random seed (optional)"),
      shiny::textInput(ns("response"), "Response column name", "Response"),
      shiny::actionButton(ns("create"), "Create design and open worksheet", class = "btn-primary", icon = shiny::icon("flask"))
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Blends (proportions)"), shiny::tableOutput(ns("preview")),
                  shiny::helpText("Each row's proportions add up to 1. Simplex centroid designs fit the special cubic model; ",
                                  "a lattice of degree 2 fits the quadratic model, degree 3 the cubic."))
    )
  )
}

mixtureCreateServer <- function(id, on_create) {
  shiny::moduleServer(id, function(input, output, session) {
    comps <- shiny::reactive({ v <- trimws(strsplit(input$components, ",", fixed = TRUE)[[1]]); v[nzchar(v)] })
    design <- shiny::reactive(validate_run(mixture_create(comps(), input$type, input$degree, isTRUE(input$axial), 1,
                                                          randomize = FALSE)))
    output$preview <- shiny::renderTable(design()[, comps(), drop = FALSE], digits = 4)
    shiny::observeEvent(input$create, {
      res <- tryCatch(mixture_create(comps(), input$type, input$degree, isTRUE(input$axial), input$reps,
                                     input$randomize, num_or_na(input$seed), trimws(input$response)),
                      error = function(e) { shiny::showNotification(conditionMessage(e), type = "error"); NULL })
      if (is.null(res)) return()
      on_create(list(name = sprintf("Mixture design, %d blends", nrow(res)), df = res))
      shiny::showNotification("Mixture design created. Enter results in the response column.", type = "message", duration = 8)
    })
  })
}

mixtureAnalyzeUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Analyze mixture design",
    shiny::tagList(
      column_picker(ns("y"), "Response"),
      column_picker(ns("components"), "Components", multiple = TRUE),
      shiny::selectInput(ns("model"), "Scheffe model", c("Quadratic" = "quadratic", "Linear" = "linear", "Special cubic" = "cubic")),
      shiny::selectInput(ns("tern"), "Ternary plot components (three)", NULL, multiple = TRUE),
      shiny::uiOutput(ns("hold_ui")),
      shiny::downloadButton(ns("download"), "Download ternary plot")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Model"), shiny::tableOutput(ns("summary")), shiny::tableOutput(ns("coef")),
                  shiny::helpText("Scheffe models have no intercept. Linear coefficients are the predicted responses of ",
                                  "the pure components; a positive blending term means the components work together ",
                                  "(synergy), a negative one that they work against each other.")),
      bslib::layout_columns(
        col_widths = c(7, 5),
        bslib::card(shiny::plotOutput(ns("ternary"), height = 480)),
        bslib::card(shiny::plotOutput(ns("trace"), height = 480))
      )
    )
  )
}

mixtureAnalyzeServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      df <- data(); nums <- numeric_cols(df)
      cand <- setdiff(nums, c("StdOrder", "RunOrder", "Run"))
      update_pickers(session, df, list(y = list(type = "numeric", default = utils::tail(cand, 1))))
      comp <- cand[vapply(cand, function(n) all(df[[n]] >= 0 & df[[n]] <= 1, na.rm = TRUE), logical(1))]
      shiny::updateSelectInput(session, "components", choices = nums, selected = keep_selected(session, "components", nums, comp))
    })
    model <- shiny::reactive({
      df <- data(); shiny::req(input$y %in% names(df))
      validate_run(mixture_fit(df, input$y, intersect(input$components, names(df)), input$model))
    })
    shiny::observeEvent(model(), {
      cmp <- model()$components
      sel <- if (length(intersect(input$tern, cmp)) == 3) intersect(input$tern, cmp) else cmp[1:3]
      shiny::updateSelectInput(session, "tern", choices = cmp, selected = sel)
    })
    output$hold_ui <- shiny::renderUI({
      m <- model(); others <- setdiff(m$components, input$tern)
      if (!length(others)) return(NULL)
      shiny::tagList(lapply(others, function(cn) shiny::numericInput(session$ns(paste0("hold_", make.names(cn))),
                                                                      paste("Fix", cn, "at proportion"), 0, min = 0, max = 0.9, step = 0.05)))
    })
    co <- shiny::reactive(mixture_coefficients(model()))
    output$summary <- shiny::renderTable(int_cols(co()$summary), digits = 4)
    output$coef <- shiny::renderTable(co()$coef, digits = 4)
    tern <- function() {
      m <- model()
      shiny::validate(shiny::need(length(input$tern) == 3 && all(input$tern %in% m$components), "Choose three components for the ternary plot."))
      others <- setdiff(m$components, input$tern)
      hold <- stats::setNames(lapply(others, function(cn) num_or_na(input[[paste0("hold_", make.names(cn))]]) %||% 0), others)
      hold <- lapply(hold, function(v) if (is.na(v)) 0 else v)
      validate_run(mixture_ternary(m, input$tern, hold))
    }
    output$ternary <- shiny::renderPlot(tern())
    output$trace <- shiny::renderPlot(mixture_trace(model()))
    output$download <- plot_download("mixture-ternary", tern, width = 8, height = 7)
  })
}
