# Models > Binary logistic, Ordinal logistic, Stepwise and Best subsets regression.

logitUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Binary logistic regression",
    shiny::tagList(
      column_picker(ns("y"), "Response (two outcomes)"),
      shiny::selectInput(ns("event"), "Event (outcome modelled)", NULL),
      column_picker(ns("x"), "Predictors", multiple = TRUE),
      shiny::numericInput(ns("conf"), "Confidence level (%)", 95, min = 50, max = 99.9),
      shiny::helpText("For pass/fail, defective/good or other yes/no outcomes. Odds ratio > 1: the event becomes more ",
                      "likely as the predictor increases (or versus the first level of a categorical predictor).")
    ),
    shiny::tagList(
      shiny::uiOutput(ns("warn")),
      bslib::card(bslib::card_header("Coefficients and odds ratios"), shiny::tableOutput(ns("coef"))),
      bslib::layout_columns(col_widths = c(6, 6),
        bslib::card(bslib::card_header("Deviance table (likelihood-ratio tests)"), shiny::tableOutput(ns("dev"))),
        bslib::card(bslib::card_header("Model fit"), shiny::tableOutput(ns("fit")),
                    shiny::helpText("Hosmer-Lemeshow p > 0.05: no evidence of lack of fit. Area under the ROC curve: 0.5 = no ",
                                    "better than chance, 1 = perfect separation."))),
      bslib::card(bslib::card_header("Classification table (cut-off 0.5)"), shiny::tableOutput(ns("class"))),
      bslib::card(shiny::plotOutput(ns("plot"), height = 400))
    )
  )
}

logitServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      df <- data(); two <- names(df)[vapply(df, function(v) length(unique(stats::na.omit(v))) == 2, logical(1))]
      update_pickers(session, df, list(y = list(type = "any", default = if (length(two)) two[1] else 1)))
      cand <- setdiff(names(df), input$y)
      shiny::updateSelectInput(session, "x", choices = names(df), selected = keep_selected(session, "x", names(df), utils::head(setdiff(numeric_cols(df), two), 1)))
    })
    shiny::observeEvent(list(data(), input$y), {
      df <- data(); shiny::req(input$y %in% names(df))
      lv <- sort(unique(as.character(stats::na.omit(df[[input$y]]))))
      shiny::updateSelectInput(session, "event", choices = lv, selected = if (isTRUE(input$event %in% lv)) input$event else utils::tail(lv, 1))
    })
    m <- shiny::reactive({
      df <- data(); shiny::req(input$y %in% names(df))
      validate_run(logit_fit(df, input$y, intersect(input$x, names(df)), input$event))
    })
    output$warn <- shiny::renderUI(if (isTRUE(m()$separation)) shiny::div(class = "alert alert-warning",
      "Some fitted probabilities are 0 or 1 (complete or quasi-complete separation): coefficients and p-values may be unreliable."))
    output$coef <- shiny::renderTable(logit_coefficients(m(), input$conf / 100), digits = 5, na = "")
    output$dev <- shiny::renderTable(int_cols(logit_deviance(m())), digits = 4)
    s <- shiny::reactive(logit_summary(m()))
    output$fit <- shiny::renderTable(s()$fit, digits = 4)
    output$class <- shiny::renderTable(int_cols(s()$classification), digits = 1)
    output$plot <- shiny::renderPlot(logit_plot(m()))
  })
}

ordinalUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Ordinal logistic regression",
    shiny::tagList(
      column_picker(ns("y"), "Response (ordered categories)"),
      shiny::selectInput(ns("order"), "Category order (lowest first)", NULL, multiple = TRUE),
      column_picker(ns("x"), "Predictors", multiple = TRUE),
      shiny::numericInput(ns("conf"), "Confidence level (%)", 95, min = 50, max = 99.9),
      shiny::helpText("For ratings such as poor / fair / good or 1-5 scores. Proportional-odds model: a positive ",
                      "coefficient shifts responses towards higher categories.")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Model test"), shiny::tableOutput(ns("test"))),
      bslib::card(bslib::card_header("Coefficients and odds ratios"), shiny::tableOutput(ns("coef"))),
      bslib::card(shiny::plotOutput(ns("plot"), height = 420))
    )
  )
}

ordinalServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      df <- data()
      few <- names(df)[vapply(df, function(v) dplyr_n(v) %in% 3:10, logical(1))]
      update_pickers(session, df, list(y = list(type = "any", default = if (length(few)) few[1] else 1)))
      shiny::updateSelectInput(session, "x", choices = names(df), selected = keep_selected(session, "x", names(df), utils::head(numeric_cols(df), 1)))
    })
    shiny::observeEvent(list(data(), input$y), {
      df <- data(); shiny::req(input$y %in% names(df))
      v <- stats::na.omit(df[[input$y]])
      lv <- if (is.numeric(v)) as.character(sort(unique(v))) else if (is.factor(v)) levels(droplevels(v)) else sort(unique(as.character(v)))
      shiny::updateSelectInput(session, "order", choices = lv, selected = lv)
    })
    m <- shiny::reactive({
      df <- data(); shiny::req(input$y %in% names(df))
      x <- setdiff(intersect(input$x, names(df)), input$y)
      validate_run(ordinal_fit(df, input$y, x, input$order))
    })
    output$test <- shiny::renderTable(m()$test, digits = 4)
    output$coef <- shiny::renderTable(ordinal_coefficients(m(), input$conf / 100), digits = 4, na = "")
    output$plot <- shiny::renderPlot({ g <- ordinal_plot(m()); shiny::validate(shiny::need(!is.null(g), "Add a numeric predictor to plot predicted probabilities.")); g })
  })
}

dplyr_n <- function(v) length(unique(stats::na.omit(v)))

stepwiseUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Stepwise regression",
    shiny::tagList(
      column_picker(ns("y"), "Response"),
      column_picker(ns("x"), "Candidate predictors", multiple = TRUE),
      column_picker(ns("force"), "Always include (optional)", multiple = TRUE),
      shiny::selectInput(ns("method"), "Method", c("Stepwise (forward and backward)" = "both", "Forward selection" = "forward",
                                                   "Backward elimination" = "backward")),
      shiny::fluidRow(shiny::column(6, shiny::numericInput(ns("a_in"), "Alpha to enter", 0.15, min = 0.001, max = 0.5, step = 0.05)),
                      shiny::column(6, shiny::numericInput(ns("a_out"), "Alpha to remove", 0.15, min = 0.001, max = 0.5, step = 0.05))),
      shiny::helpText("Automatic selection is a screening aid: check the chosen model with subject knowledge and residual plots.")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Selection steps"), shiny::tableOutput(ns("steps"))),
      bslib::card(bslib::card_header("Final model"), shiny::tableOutput(ns("summary")), shiny::tags$pre(shiny::textOutput(ns("eq"))),
                  shiny::tableOutput(ns("coef")))
    )
  )
}

stepwiseServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      df <- data(); nums <- numeric_cols(df)
      update_pickers(session, df, list(y = list(type = "numeric", default = 1)))
      shiny::updateSelectInput(session, "x", choices = names(df), selected = keep_selected(session, "x", names(df), nums[-1]))
      shiny::updateSelectInput(session, "force", choices = names(df), selected = keep_selected(session, "force", names(df), character(0)))
    })
    r <- shiny::reactive({
      df <- data(); shiny::req(input$y %in% names(df))
      x <- setdiff(intersect(input$x, names(df)), input$y)
      validate_run(stepwise_reg(df, input$y, x, input$method, input$a_in, input$a_out, intersect(input$force, x)))
    })
    output$steps <- shiny::renderTable(r()$steps, digits = 5)
    output$summary <- shiny::renderTable(int_cols(r()$summary), digits = 4)
    output$eq <- shiny::renderText({
      cf <- stats::coef(r()$fit); nm <- gsub("`", "", names(cf))
      paste(input$y, "=", formatC(cf[1], digits = 5, format = "g"),
            paste(sprintf("%s %s %s", ifelse(cf[-1] < 0, "-", "+"), formatC(abs(cf[-1]), digits = 5, format = "g"), nm[-1]), collapse = " "))
    })
    output$coef <- shiny::renderTable({
      s <- summary(r()$fit)$coefficients
      data.frame(Term = gsub("`", "", rownames(s)), Coef = s[, 1], `SE coef` = s[, 2], T = s[, 3], `p-value` = s[, 4], check.names = FALSE)
    }, digits = 5)
  })
}

bestSubsetsUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Best subsets regression",
    shiny::tagList(
      column_picker(ns("y"), "Response"),
      column_picker(ns("x"), "Predictors (numeric, up to 15)", multiple = TRUE),
      shiny::numericInput(ns("best"), "Models to show for each size", 2, min = 1, max = 5),
      shiny::helpText("Good models have high adjusted R-sq, small S and Mallows' Cp close to the number of predictors + 1.")
    ),
    bslib::card(bslib::card_header("Best models by number of predictors"), shiny::tableOutput(ns("tab")))
  )
}

bestSubsetsServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      df <- data(); nums <- numeric_cols(df)
      update_pickers(session, df, list(y = list(type = "numeric", default = 1)))
      shiny::updateSelectInput(session, "x", choices = nums, selected = keep_selected(session, "x", nums, utils::head(nums[-1], 8)))
    })
    output$tab <- shiny::renderTable({
      df <- data(); shiny::req(input$y %in% names(df))
      int_cols(validate_run(best_subsets(df, input$y, setdiff(intersect(input$x, names(df)), input$y), input$best)))
    }, digits = 3)
  })
}
