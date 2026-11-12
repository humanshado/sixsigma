# Stat > Regression.

regressionUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Regression",
    shiny::tagList(
      column_picker(ns("y"), "Response"),
      column_picker(ns("x"), "Predictors", multiple = TRUE),
      shiny::helpText("Text columns are treated as categorical predictors."),
      shiny::selectInput(ns("poly"), "Model for a single numeric predictor",
                         c("Linear" = 1, "Quadratic" = 2, "Cubic" = 3)),
      shiny::checkboxInput(ns("interact"), "Include two-way interactions (several predictors)", FALSE),
      shiny::numericInput(ns("conf"), "Confidence level (%)", 95, min = 50, max = 99.9, step = 0.5),
      shiny::downloadButton(ns("download"), "Download residual plots")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Regression equation"),
                  shiny::tags$pre(shiny::textOutput(ns("equation"))),
                  shiny::tableOutput(ns("summary"))),
      shiny::uiOutput(ns("fitted_card")),
      bslib::card(bslib::card_header("Coefficients"), shiny::tableOutput(ns("coef")),
                  shiny::helpText("VIF above 5 suggests the predictors are strongly correlated with each other.")),
      bslib::card(bslib::card_header("Analysis of variance"), shiny::tableOutput(ns("anova"))),
      bslib::card(bslib::card_header("Residual plots"), shiny::plotOutput(ns("resid"), height = 560)),
      bslib::card(bslib::card_header("Unusual observations"), shiny::tableOutput(ns("unusual"))),
      bslib::card(bslib::card_header("Prediction"),
                  shiny::helpText("Enter predictor values to get the fitted response with confidence (CI) and prediction (PI) intervals."),
                  shiny::uiOutput(ns("pred_inputs")),
                  shiny::tableOutput(ns("pred")))
    )
  )
}

regressionServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      df <- data()
      nums <- numeric_cols(df)
      update_pickers(session, df, list(y = list(type = "numeric", default = utils::tail(nums, 1))))
      x_default <- setdiff(nums, utils::tail(nums, 1))
      shiny::updateSelectInput(session, "x", choices = names(df),
                              selected = keep_selected(session, "x", names(df), utils::head(x_default, 1)))
    })

    conf <- shiny::reactive({
      a <- input$conf / 100
      shiny::validate(shiny::need(isTRUE(a > 0 && a < 1), "Confidence level must be between 0 and 100."))
      a
    })

    model <- shiny::reactive({
      df <- data()
      shiny::req(input$y %in% names(df))
      x <- intersect(input$x, names(df))
      validate_run(fit_regression(df, input$y, x, isTRUE(input$interact), as.integer(input$poly)))
    })

    output$equation <- shiny::renderText(reg_equation(model()))
    output$summary <- shiny::renderTable(int_cols(reg_summary(model())), digits = 4)
    output$coef <- shiny::renderTable(reg_coefficients(model()), digits = 4, na = "")
    output$anova <- shiny::renderTable(int_cols(reg_anova(model())), digits = 4, na = "")
    output$unusual <- shiny::renderTable({
      u <- reg_unusual(model())
      if (is.null(u)) data.frame(Result = "No unusual observations.") else u
    }, digits = 4)

    output$fitted_card <- shiny::renderUI({
      if (!model()$single_num) return(NULL)
      bslib::card(bslib::card_header("Fitted line plot"), shiny::plotOutput(session$ns("fitted"), height = 440))
    })
    output$fitted <- shiny::renderPlot({
      shiny::req(model()$single_num)
      reg_fitted_line(model(), conf())
    })

    draw_resid <- function() draw_grid(reg_residual_plots(model()$fit))
    output$resid <- shiny::renderPlot(draw_resid())
    output$download <- plot_download("residual-plots", draw_resid, height = 8)

    output$pred_inputs <- shiny::renderUI({
      m <- model()
      shiny::fluidRow(lapply(m$x, function(v) {
        col <- m$data[[v]]
        shiny::column(3, if (is.factor(col)) {
          shiny::selectInput(session$ns(paste0("pred_", make.names(v))), v, levels(col))
        } else {
          shiny::numericInput(session$ns(paste0("pred_", make.names(v))), v, signif(mean(col), 4))
        })
      }))
    })
    output$pred <- shiny::renderTable({
      m <- model()
      vals <- lapply(m$x, function(v) input[[paste0("pred_", make.names(v))]])
      shiny::req(all(!vapply(vals, is.null, logical(1))))
      shiny::validate(shiny::need(all(!is.na(unlist(vals))), "Enter a value for every predictor."))
      nd <- as.data.frame(stats::setNames(vals, m$x), check.names = FALSE, stringsAsFactors = FALSE)
      validate_run(reg_predict(m, nd, conf()))
    }, digits = 4)
  })
}
