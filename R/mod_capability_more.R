# Quality Tools > Capability sixpack, Attribute capability, Multivariate capability.

sixpackUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Capability sixpack",
    shiny::tagList(
      column_picker(ns("var"), "Data"),
      shiny::radioButtons(ns("sg_mode"), "Subgroups", c("Individual values" = "none", "Fixed size" = "size", "Group column" = "col")),
      shiny::conditionalPanel("input.sg_mode == 'size'", ns = ns, shiny::numericInput(ns("sg_size"), "Subgroup size", 5, min = 2)),
      shiny::conditionalPanel("input.sg_mode == 'col'", ns = ns, column_picker(ns("sg_col"), "Subgroup column")),
      blank_numeric(ns("lsl"), "Lower spec limit (LSL)"),
      blank_numeric(ns("usl"), "Upper spec limit (USL)"),
      blank_numeric(ns("target"), "Target (optional)"),
      shiny::downloadButton(ns("download"), "Download sixpack"),
      shiny::helpText("One page to judge whether a capability study is valid: the process must be stable (control ",
                      "charts), roughly normal (probability plot), and then capable (histogram and capability plot).")
    ),
    shiny::tagList(
      bslib::card(shiny::plotOutput(ns("plot"), height = 860)),
      bslib::card(bslib::card_header("Capability indices"), shiny::tableOutput(ns("tab")))
    )
  )
}

sixpackServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    prev_cols <- shiny::reactiveVal(NULL)
    shiny::observeEvent(data(), {
      update_pickers(session, data(), list(var = list(type = "numeric", default = 1), sg_col = list(type = "any", default = 1)))
      if (!identical(names(data()), prev_cols())) { prev_cols(names(data())); clear_numeric(session, c("lsl", "usl", "target")) }
    })
    spec <- debounced_inputs(input, c("lsl", "usl", "target"))
    res <- shiny::reactive({
      df <- data(); shiny::req(input$var %in% names(df))
      x <- df[[input$var]]
      g <- switch(input$sg_mode, none = NULL, size = { shiny::validate(shiny::need(isTRUE(input$sg_size >= 2), "Subgroup size must be at least 2."))
                                                       ceiling(seq_along(x) / input$sg_size) },
                  col = { shiny::req(input$sg_col %in% names(df)); df[[input$sg_col]] })
      lsl <- num_or_na(spec$lsl()); usl <- num_or_na(spec$usl())
      shiny::validate(shiny::need(!(is.na(lsl) && is.na(usl)), "Enter at least one specification limit."),
                      shiny::need(is.na(lsl) || is.na(usl) || lsl < usl, "LSL must be below USL."),
                      shiny::need(!specs_far_from_data(x, lsl, usl), "The specification limits are far outside the data."))
      validate_run(cap_sixpack(x, g, lsl, usl, num_or_na(spec$target())))
    })
    draw <- function() draw_grid(res()$plots, ncol = 2)
    output$plot <- shiny::renderPlot(draw())
    output$tab <- shiny::renderTable(res()$table, digits = 4, na = "")
    output$download <- plot_download("capability-sixpack", draw, width = 12, height = 12)
  })
}

attrCapUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Attribute capability",
    shiny::tagList(
      shiny::radioButtons(ns("type"), "Data", c("Defectives (binomial)" = "binomial", "Defects (Poisson)" = "poisson")),
      column_picker(ns("d"), "Defectives / defects per sample"),
      shiny::radioButtons(ns("n_mode"), "Sample size", c("Constant" = "const", "Column" = "col"), inline = TRUE),
      shiny::conditionalPanel("input.n_mode == 'const'", ns = ns, shiny::numericInput(ns("n_const"), "Sample size", 50, min = 1)),
      shiny::conditionalPanel("input.n_mode == 'col'", ns = ns, column_picker(ns("n_col"), "Sample size column")),
      shiny::numericInput(ns("conf"), "Confidence level (%)", 95, min = 50, max = 99.9),
      shiny::downloadButton(ns("download"), "Download charts"),
      shiny::helpText("Try qcr's orangejuice (binomial) or circuit (Poisson) datasets.")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Summary"), shiny::tableOutput(ns("summary"))),
      bslib::card(shiny::plotOutput(ns("plot"), height = 620))
    )
  )
}

attrCapServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), update_pickers(session, data(), list(
      d = list(type = "numeric", default = 1),
      n_col = list(type = "numeric", default = utils::tail(numeric_cols(data()), 1)))))
    res <- shiny::reactive({
      df <- data(); shiny::req(input$d %in% names(df))
      d <- df[[input$d]]
      n <- if (input$n_mode == "const") rep(input$n_const, length(d)) else { shiny::req(input$n_col %in% names(df)); df[[input$n_col]] }
      conf <- input$conf / 100
      validate_run(if (input$type == "binomial") cap_binomial(d, n, conf) else cap_poisson(d, n, conf))
    })
    draw <- function() draw_grid(cap_attribute_plots(res(), input$type), ncol = 2)
    output$summary <- shiny::renderTable(res()$summary, digits = 5)
    output$plot <- shiny::renderPlot(draw())
    output$download <- plot_download("attribute-capability", draw, width = 12, height = 9)
  })
}

mvCapUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Multivariate capability",
    shiny::tagList(
      column_picker(ns("vars"), "Variables (two or more)", multiple = TRUE),
      shiny::uiOutput(ns("spec_ui")),
      shiny::selectInput(ns("px"), "Plot: horizontal axis", NULL),
      shiny::selectInput(ns("py"), "Plot: vertical axis", NULL),
      shiny::helpText("For several correlated characteristics with their own specification limits. Try qcr's dowel1 ",
                      "(diameter 0.47-0.53, length 0.95-1.05).")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Multivariate capability vector (Shahriari et al.)"), shiny::tableOutput(ns("vector")),
                  shiny::helpText("CpM > 1: the process region fits inside the tolerance region by volume. PV close to 1: the ",
                                  "mean is close to the target (small = off target). LI = 1: the process region lies entirely ",
                                  "inside the specifications. Expected PPM uses a multivariate normal model.")),
      bslib::card(bslib::card_header("Each variable on its own"), shiny::tableOutput(ns("per_var"))),
      bslib::card(shiny::plotOutput(ns("plot"), height = 480))
    )
  )
}

mvCapServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      nums <- numeric_cols(data())
      shiny::updateSelectInput(session, "vars", choices = nums, selected = keep_selected(session, "vars", nums, utils::head(nums, 2)))
    })
    vars <- shiny::reactive(intersect(input$vars, names(data())))
    output$spec_ui <- shiny::renderUI({
      v <- vars()
      if (length(v) < 2) return(shiny::helpText("Choose at least two variables."))
      df <- data()
      shiny::tagList(lapply(v, function(cn) {
        id <- make.names(cn); keep <- function(w) shiny::isolate(input[[paste0(w, "_", id)]])
        shiny::fluidRow(shiny::column(12, shiny::tags$b(cn)),
          shiny::column(4, shiny::numericInput(session$ns(paste0("lsl_", id)), "LSL", keep("lsl") %||% NA)),
          shiny::column(4, shiny::numericInput(session$ns(paste0("usl_", id)), "USL", keep("usl") %||% NA)),
          shiny::column(4, shiny::numericInput(session$ns(paste0("tgt_", id)), "Target", keep("tgt") %||% NA)))
      }))
    })
    shiny::observeEvent(vars(), {
      v <- vars()
      shiny::updateSelectInput(session, "px", choices = v, selected = if (isTRUE(input$px %in% v)) input$px else v[1])
      shiny::updateSelectInput(session, "py", choices = v, selected = if (isTRUE(input$py %in% v)) input$py else v[min(2, length(v))])
    })
    res <- shiny::reactive({
      v <- vars(); shiny::validate(shiny::need(length(v) >= 2, "Choose at least two numeric variables."))
      g <- function(w) vapply(v, function(cn) num_or_na(input[[paste0(w, "_", make.names(cn))]]), numeric(1))
      lsl <- g("lsl"); usl <- g("usl"); tg <- g("tgt")
      shiny::validate(shiny::need(all(!is.na(lsl) & !is.na(usl)), "Enter LSL and USL for every variable."))
      if (any(is.na(tg))) tg <- ifelse(is.na(tg), (lsl + usl) / 2, tg)
      validate_run(cap_multivariate(data()[, v, drop = FALSE], unname(lsl), unname(usl), unname(tg)))
    })
    output$vector <- shiny::renderTable(res()$vector, digits = 4)
    output$per_var <- shiny::renderTable(res()$per_var, digits = 4)
    output$plot <- shiny::renderPlot({
      r <- res(); v <- colnames(r$X)
      shiny::validate(shiny::need(isTRUE(input$px != input$py), "Choose two different variables to plot."))
      cap_multivariate_plot(r, match(input$px, v), match(input$py, v))
    })
  })
}
