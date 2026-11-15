# MSA > Gage R&R (expanded): extra factors and unbalanced data.

gageExpUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Gage R&R (expanded)",
    shiny::tagList(
      column_picker(ns("meas"), "Measurement"),
      column_picker(ns("part"), "Part"),
      column_picker(ns("op"), "Operator"),
      column_picker(ns("extra"), "Extra factors (e.g. gauge, fixture)", multiple = TRUE),
      shiny::checkboxInput(ns("inter"), "Include two-way interactions", TRUE),
      blank_numeric(ns("tol"), "Tolerance (USL - LSL, optional)"),
      shiny::numericInput(ns("k"), "Study variation (number of std. devs.)", 6, min = 1, max = 6),
      shiny::helpText("Uses REML variance components, so it works with missing measurements (unbalanced studies) ",
                      "and with extra random factors. Components estimated as negative are set to zero.")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Gage evaluation"), shiny::uiOutput(ns("verdict")), shiny::tableOutput(ns("tab"))),
      bslib::card(shiny::plotOutput(ns("plot"), height = 420)),
      bslib::card(bslib::card_header("Variance components (REML)"), shiny::tableOutput(ns("vc")))
    )
  )
}

gageExpServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      df <- data(); cats <- names(df)[!vapply(df, is.numeric, logical(1))]
      update_pickers(session, df, list(meas = list(type = "numeric", default = 1),
                                       part = list(type = "any", default = if (length(cats)) cats[1] else 1),
                                       op = list(type = "any", default = if (length(cats) > 1) cats[2] else 2)))
      shiny::updateSelectInput(session, "extra", choices = names(df), selected = keep_selected(session, "extra", names(df), character(0)))
    })
    g <- shiny::reactive({
      df <- data(); shiny::req(all(c(input$meas, input$part, input$op) %in% names(df)))
      validate_run(gage_expanded(df, input$meas, input$part, input$op, setdiff(intersect(input$extra, names(df)), c(input$part, input$op)),
                                 isTRUE(input$inter), num_or_na(input$tol), input$k))
    })
    output$verdict <- shiny::renderUI({
      r <- g()
      shiny::tagList(shiny::div(class = paste("alert mb-2", if (r$pct_grr < 10) "alert-success" else if (r$pct_grr <= 30) "alert-warning" else "alert-danger"),
                                r$verdict),
                     shiny::tags$p(sprintf("Number of distinct categories: %d%s. %s", r$ndc, if (r$ndc >= 5) " (5 or more is adequate)" else " (fewer than 5: the gage cannot tell parts apart well)",
                                           if (r$balanced) "Balanced study." else "Unbalanced study (REML handles the missing cells).")))
    })
    output$tab <- shiny::renderTable(g()$table, digits = 4)
    output$vc <- shiny::renderTable(g()$components, digits = 6)
    output$plot <- shiny::renderPlot(draw_grid(gage_expanded_plot(g()), ncol = 2))
  })
}
