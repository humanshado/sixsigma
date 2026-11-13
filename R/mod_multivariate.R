# Quality Tools > Multivariate control charts (T^2, MEWMA, MCUSUM, depth r/Q/S).

mv_types <- list(
  "Parametric (multivariate normal)" = c("Hotelling T-squared" = "t2", "MEWMA (small shifts)" = "mewma",
                                         "MCUSUM (small shifts)" = "mcusum"),
  "Nonparametric (data depth)" = c("r chart - individual observations" = "r", "Q chart - subgroups" = "q",
                                   "S chart - gradual drift" = "s")
)

multivariateUI <- function(id) {
  ns <- shiny::NS(id)
  is_type <- function(...) sprintf("[%s].indexOf(input.type) >= 0", paste0("'", c(...), "'", collapse = ","))
  analysis_layout(
    "Multivariate control charts",
    shiny::tagList(
      shiny::selectInput(ns("type"), "Chart", mv_types),
      column_picker(ns("vars"), "Variables (two or more)", multiple = TRUE),
      shiny::conditionalPanel(is_type("t2", "mewma", "mcusum", "q"), ns = ns,
        shiny::radioButtons(ns("sg_mode"), "Samples",
                            c("Individual observations" = "none", "Subgroups of fixed size" = "size",
                              "Subgroups from a column" = "col")),
        shiny::conditionalPanel("input.sg_mode == 'size'", ns = ns,
          shiny::numericInput(ns("sg_size"), "Subgroup size", 5, min = 2)),
        shiny::conditionalPanel("input.sg_mode == 'col'", ns = ns,
          column_picker(ns("sg_col"), "Subgroup column"))),
      shiny::conditionalPanel(is_type("t2", "mewma", "mcusum"), ns = ns,
        blank_numeric(ns("n_ref"), "Reference (Phase I): first N samples (blank = all)", min = 3)),
      shiny::conditionalPanel(is_type("r", "q", "s"), ns = ns,
        shiny::numericInput(ns("n_ref_np"), "Reference sample: first N rows", 20, min = 4),
        shiny::selectInput(ns("depth"), "Depth", c("Mahalanobis" = "mahalanobis", "Spatial" = "spatial"))),
      shiny::conditionalPanel(is_type("t2", "r", "q", "s"), ns = ns,
        shiny::numericInput(ns("alpha"), "False alarm rate α", 0.01, min = 0.0001, max = 0.2, step = 0.005)),
      shiny::conditionalPanel(is_type("mewma"), ns = ns,
        shiny::selectInput(ns("lambda"), "Lambda (λ)", sprintf("%.1f", seq(0.1, 0.9, 0.1))),
        blank_numeric(ns("mewma_h"), "Upper limit h (blank = tabulated, ARL ≈ 200)")),
      shiny::conditionalPanel(is_type("mcusum"), ns = ns,
        shiny::fluidRow(
          shiny::column(6, shiny::numericInput(ns("mc_k"), "k", 0.5, min = 0, step = 0.1)),
          shiny::column(6, shiny::numericInput(ns("mc_h"), "h", 5.5, min = 0.5, step = 0.5)))),
      shiny::downloadButton(ns("download"), "Download chart"),
      shiny::helpText("Monitors several correlated variables at once. Depth charts make no normality ",
                      "assumption: each new point is ranked by how central it is within the reference sample. ",
                      "Try the qcr dowel1 or archery1 datasets.")
    ),
    shiny::tagList(
      bslib::card(shiny::plotOutput(ns("chart"), height = 460)),
      bslib::card(bslib::card_header("Out-of-control signals"), shiny::tableOutput(ns("signals")),
                  shiny::uiOutput(ns("contrib_note"))),
      shiny::uiOutput(ns("setup_card"))
    )
  )
}

multivariateServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      df <- data()
      nums <- setdiff(numeric_cols(df), c("sample", "end", "arrow", "StdOrder", "RunOrder"))
      shiny::updateSelectInput(session, "vars", choices = numeric_cols(df),
                               selected = keep_selected(session, "vars", numeric_cols(df), utils::head(nums, 2)))
      update_pickers(session, df, list(sg_col = list(type = "any", default = 1)))
      shiny::updateNumericInput(session, "n_ref_np", value = max(4, floor(nrow(df) / 2)))
    })

    result <- shiny::reactive({
      df <- data()
      vars <- intersect(input$vars, names(df))
      shiny::validate(shiny::need(length(vars) >= 2, "Choose at least two numeric variables."))
      grp <- function() {
        switch(input$sg_mode,
               none = mv_groups(df, vars),
               size = mv_groups(df, vars, size = input$sg_size),
               col = { shiny::req(input$sg_col %in% names(df)); mv_groups(df, vars, group = input$sg_col) })
      }
      a <- input$alpha
      if (input$type %in% c("t2", "r", "q", "s"))
        shiny::validate(shiny::need(isTRUE(a > 0 && a < 0.5), "α must be between 0 and 0.5."))
      validate_run(switch(input$type,
        t2 = mv_t2(grp(), a, num_or_na(input$n_ref)),
        mewma = mv_mewma(grp(), as.numeric(input$lambda), num_or_na(input$mewma_h), num_or_na(input$n_ref)),
        mcusum = mv_mcusum(grp(), input$mc_k, input$mc_h, num_or_na(input$n_ref)),
        r = np_r_chart(df, vars, input$n_ref_np, a, input$depth),
        q = np_q_chart(df, vars, input$n_ref_np,
                       size = if (input$sg_mode == "size") input$sg_size else if (input$sg_mode == "none") NA,
                       group = if (input$sg_mode == "col") input$sg_col, alpha = a, method = input$depth),
        s = np_s_chart(df, vars, input$n_ref_np, a, input$depth)
      ))
    })

    draw <- function() print(plot_chart(result()$charts[[1]]))
    output$chart <- shiny::renderPlot(draw())
    output$download <- plot_download("multivariate-chart", draw, height = 6)

    output$signals <- shiny::renderTable({
      r <- result()
      if (!is.null(r$contributions)) return(r$contributions)
      v <- violation_table(r$charts)
      if (is.null(v)) data.frame(Result = "No out-of-control signals.") else v[, c("Sample", "Value", "Rule")]
    }, digits = 4)
    output$contrib_note <- shiny::renderUI({
      if (is.null(result()$contributions)) return(NULL)
      shiny::helpText("Each variable column shows how much T² drops when that variable is left out: ",
                      "the largest drop points to the variable most responsible for the signal.")
    })
    output$setup_card <- shiny::renderUI({
      s <- result()$setup
      if (is.null(s)) return(NULL)
      bslib::card(bslib::card_header("Reference estimates (Phase I)"),
                  shiny::tags$p(sprintf("%d samples used for estimates, %s.", s$m,
                                        if (s$n == 1) "individual observations" else paste("subgroup size", s$n))),
                  shiny::tableOutput(session$ns("center")),
                  shiny::tags$b(if (s$n == 1) "Covariance matrix" else "Pooled within-subgroup covariance matrix"),
                  shiny::tableOutput(session$ns("cov")))
    })
    output$center <- shiny::renderTable({
      s <- result()$setup; shiny::req(s)
      data.frame(t(stats::setNames(s$center, s$vars)), check.names = FALSE)
    }, digits = 5, caption = "Mean vector", caption.placement = "top")
    output$cov <- shiny::renderTable({
      s <- result()$setup; shiny::req(s)
      cbind(` ` = s$vars, as.data.frame(s$S))
    }, digits = 6)
  })
}
