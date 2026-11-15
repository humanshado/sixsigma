# Models > General ANOVA (general linear model).

anovaUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "General ANOVA",
    shiny::tagList(
      column_picker(ns("y"), "Response"),
      column_picker(ns("factors"), "Factors", multiple = TRUE),
      shiny::selectInput(ns("random"), "Random factors (others are fixed)", NULL, multiple = TRUE),
      column_picker(ns("nested"), "Nested factor (optional)"),
      shiny::conditionalPanel("input.nested != '' && input.nested != '(none)'", ns = ns,
        shiny::selectInput(ns("within"), "...nested within", NULL, multiple = TRUE)),
      column_picker(ns("covariates"), "Covariates (numeric, optional)", multiple = TRUE),
      shiny::selectInput(ns("order"), "Interactions between crossed factors", c("None" = 1, "Up to 2-way" = 2, "Up to 3-way" = 3), selected = 2),
      shiny::numericInput(ns("conf"), "Family confidence for Tukey comparisons (%)", 95, min = 50, max = 99.9),
      shiny::helpText("Random factors are samples from a larger population (batches, operators, days); their ",
                      "levels are not of interest themselves. Nested: each level of the nested factor appears ",
                      "within only one level of its parent (e.g. batches within suppliers).")
    ),
    shiny::tagList(
      shiny::uiOutput(ns("note")),
      bslib::card(bslib::card_header("Analysis of variance (adjusted sums of squares)"), shiny::tableOutput(ns("tab")),
                  shiny::tableOutput(ns("summary"))),
      shiny::uiOutput(ns("random_cards")),
      shiny::uiOutput(ns("tukey_card")),
      bslib::card(shiny::plotOutput(ns("effects"), height = 320)),
      shiny::uiOutput(ns("inter_card")),
      bslib::card(bslib::card_header("Residual plots"), shiny::plotOutput(ns("resid"), height = 480))
    )
  )
}

anovaServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      df <- data(); cats <- names(df)[!vapply(df, is.numeric, logical(1))]
      update_pickers(session, df, list(y = list(type = "numeric", default = 1), nested = list(type = "any", optional = TRUE, default = "")))
      shiny::updateSelectInput(session, "factors", choices = names(df), selected = keep_selected(session, "factors", names(df), utils::head(cats, 2)))
      shiny::updateSelectInput(session, "covariates", choices = numeric_cols(df), selected = keep_selected(session, "covariates", numeric_cols(df), character(0)))
    })
    shiny::observeEvent(input$factors, {
      f <- input$factors %||% character(0)
      shiny::updateSelectInput(session, "random", choices = f, selected = intersect(input$random, f))
      shiny::updateSelectInput(session, "within", choices = setdiff(f, input$nested), selected = intersect(input$within, f))
    }, ignoreNULL = FALSE)
    shiny::observeEvent(input$nested, {
      f <- input$factors %||% character(0)
      shiny::updateSelectInput(session, "within", choices = setdiff(f, input$nested), selected = setdiff(intersect(input$within, f), input$nested))
    })
    g <- shiny::reactive({
      df <- data(); shiny::req(input$y %in% names(df))
      f <- setdiff(intersect(input$factors, names(df)), input$y)
      nb <- empty_to_null(input$nested)
      nested <- if (!is.null(nb)) {
        shiny::validate(shiny::need(nb %in% f, "Add the nested factor to the factors list."),
                        shiny::need(length(input$within) >= 1, "Choose the factor(s) it is nested within."))
        stats::setNames(list(intersect(input$within, f)), nb)
      } else list()
      validate_run(glm_anova(df, input$y, f, setdiff(intersect(input$covariates, names(df)), c(input$y, f)),
                             intersect(input$random, f), nested, as.integer(input$order)))
    })
    output$note <- shiny::renderUI(if (!is.null(g()$note)) shiny::div(class = "alert alert-warning", g()$note))
    output$tab <- shiny::renderTable(int_cols(g()$table), digits = 4, na = "")
    output$summary <- shiny::renderTable(g()$summary, digits = 4)
    output$random_cards <- shiny::renderUI({
      r <- g()
      shiny::tagList(
        if (!is.null(r$ems)) bslib::card(bslib::card_header("Expected mean squares"), shiny::tableOutput(session$ns("ems")),
                                          shiny::helpText("Each term is tested against the mean square (or combination) whose ",
                                                          "expectation matches its own without the term's component.")),
        if (!is.null(r$vc)) bslib::card(bslib::card_header("Variance components"), shiny::tableOutput(session$ns("vc")),
                                         shiny::helpText("Negative estimates are shown but counted as zero in the percentages.")))
    })
    output$ems <- shiny::renderTable(g()$ems)
    output$vc <- shiny::renderTable(g()$vc, digits = 5)
    tukey <- shiny::reactive({
      r <- g()
      if (length(r$random) || ncol(r$dd) > length(r$factors) + 1) return(NULL)
      fit <- stats::aov(stats::formula(r$fit_formula), data = r$dd)
      th <- tryCatch(stats::TukeyHSD(fit, which = unname(r$vn[r$factors]), conf.level = input$conf / 100), error = function(e) NULL)
      if (is.null(th)) return(NULL)
      do.call(rbind, lapply(names(th), function(nm) {
        t <- th[[nm]]
        data.frame(Factor = names(r$vn)[match(nm, r$vn)], Comparison = rownames(t), Difference = t[, "diff"], `CI lower` = t[, "lwr"],
                   `CI upper` = t[, "upr"], `Adjusted p` = t[, "p adj"], check.names = FALSE, row.names = NULL)
      }))
    })
    output$tukey_card <- shiny::renderUI({
      if (is.null(tukey())) return(NULL)
      bslib::card(bslib::card_header("Tukey pairwise comparisons (fixed factors)"), shiny::tableOutput(session$ns("tukey")))
    })
    output$tukey <- shiny::renderTable(tukey(), digits = 4)
    output$effects <- shiny::renderPlot(glm_effects_plot(g()))
    output$inter_card <- shiny::renderUI({
      if (length(g()$factors) < 2) return(NULL)
      bslib::card(shiny::plotOutput(session$ns("inter"), height = 380))
    })
    output$inter <- shiny::renderPlot(glm_interaction_plot(g()))
    output$resid <- shiny::renderPlot(draw_grid(reg_residual_plots(g()$lmfit)))
  })
}
