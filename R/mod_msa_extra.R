# Quality Tools > Type 1 gage study, Gage linearity and bias, Multi-vari chart.

gageType1UI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Type 1 gage study",
    shiny::tagList(
      column_picker(ns("x"), "Measurements of one reference part"),
      blank_numeric(ns("ref"), "Reference value"),
      blank_numeric(ns("tol"), "Tolerance (USL - LSL)"),
      shiny::numericInput(ns("K"), "Percent of tolerance for Cg (K)", 20, min = 1, max = 100),
      shiny::numericInput(ns("L"), "Study variation (number of std. devs.)", 6, min = 1, max = 8),
      shiny::downloadButton(ns("download"), "Download run chart"),
      shiny::helpText("Measure one master part 25-50 times with the gage. Cg checks repeatability; ",
                      "Cgk also includes bias. Both should be at least 1.33.")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Results"), shiny::uiOutput(ns("verdict")), shiny::tableOutput(ns("stats"))),
      bslib::card(shiny::plotOutput(ns("plot"), height = 420))
    )
  )
}

gageType1Server <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), update_pickers(session, data(), list(x = list(type = "numeric", default = 1))))
    spec <- debounced_inputs(input, c("ref", "tol"))
    res <- shiny::reactive({
      df <- data(); shiny::req(input$x %in% names(df))
      ref <- num_or_na(spec$ref()); tol <- num_or_na(spec$tol())
      shiny::validate(shiny::need(!is.na(ref), "Enter the reference value of the master part."),
                      shiny::need(isTRUE(tol > 0), "Enter the tolerance (USL - LSL)."))
      validate_run(gage_type1(df[[input$x]], ref, tol, input$K, input$L))
    })
    output$verdict <- shiny::renderUI({
      r <- res()
      shiny::div(class = paste("alert mb-2", if (r$cg >= 1.33 && r$cgk >= 1.33) "alert-success" else "alert-warning"), r$verdict)
    })
    output$stats <- shiny::renderTable(res()$stats, digits = 5)
    output$plot <- shiny::renderPlot(gage_type1_plot(res()))
    output$download <- plot_download("type1-gage", function() gage_type1_plot(res()), height = 5)
  })
}

gageLinUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Gage linearity and bias",
    shiny::tagList(
      column_picker(ns("ref"), "Reference value column"),
      column_picker(ns("meas"), "Measurement column"),
      blank_numeric(ns("pv"), "Process variation (6 x process sigma, optional)"),
      shiny::numericInput(ns("conf"), "Confidence level (%)", 95, min = 50, max = 99.9),
      shiny::downloadButton(ns("download"), "Download plot"),
      shiny::helpText("Measure several parts spanning the operating range, each with a known reference value, ",
                      "a number of times. Bias = measurement - reference.")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Conclusions"), shiny::uiOutput(ns("verdict"))),
      bslib::card(shiny::plotOutput(ns("plot"), height = 420)),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(bslib::card_header("Linearity"), shiny::tableOutput(ns("coef")), shiny::tableOutput(ns("summary"))),
        bslib::card(bslib::card_header("Bias by reference value"), shiny::tableOutput(ns("per")))
      )
    )
  )
}

gageLinServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), update_pickers(session, data(), list(
      ref = list(type = "numeric", default = 1), meas = list(type = "numeric", default = 2))))
    res <- shiny::reactive({
      df <- data(); shiny::req(input$ref %in% names(df), input$meas %in% names(df))
      shiny::validate(shiny::need(input$ref != input$meas, "Choose different reference and measurement columns."))
      validate_run(gage_linearity(df, input$ref, input$meas, num_or_na(input$pv), input$conf / 100))
    })
    output$verdict <- shiny::renderUI(shiny::tags$ul(lapply(res()$verdict, shiny::tags$li)))
    output$plot <- shiny::renderPlot(gage_linearity_plot(res()))
    output$coef <- shiny::renderTable(res()$coef, digits = 5)
    output$summary <- shiny::renderTable(res()$summary, digits = 5, na = "")
    output$per <- shiny::renderTable(int_cols(res()$per), digits = 5, na = "")
    output$download <- plot_download("gage-linearity", function() gage_linearity_plot(res()), height = 5)
  })
}

multivariUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Multi-vari chart",
    shiny::tagList(
      column_picker(ns("y"), "Response"),
      column_picker(ns("x"), "Factor on the horizontal axis"),
      column_picker(ns("lines"), "Factor for lines (optional)"),
      column_picker(ns("panel"), "Factor for panels (optional)"),
      shiny::downloadButton(ns("download"), "Download chart"),
      shiny::helpText("Shows how much of the variation comes from each source (within a level, between levels, ",
                      "between panels): a quick visual way to find the dominant source of variation.")
    ),
    bslib::card(shiny::plotOutput(ns("plot"), height = 560))
  )
}

multivariServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      df <- data(); cats <- names(df)[!vapply(df, is.numeric, logical(1))]
      update_pickers(session, df, list(
        y = list(type = "numeric", default = 1),
        x = list(type = "any", default = if (length(cats)) cats[1] else 1),
        lines = list(type = "any", optional = TRUE, default = if (length(cats) > 1) cats[2] else ""),
        panel = list(type = "any", optional = TRUE, default = if (length(cats) > 2) cats[3] else "")))
    })
    draw <- function() {
      df <- data(); shiny::req(input$y %in% names(df), input$x %in% names(df))
      validate_run(multivari_plot(df, input$y, input$x, empty_to_null(input$lines), empty_to_null(input$panel)))
    }
    output$plot <- shiny::renderPlot(draw())
    output$download <- plot_download("multi-vari", draw, height = 6)
  })
}
