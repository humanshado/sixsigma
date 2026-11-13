# DOE > Response optimizer (desirability).

optimizerUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Response optimizer",
    shiny::tagList(
      column_picker(ns("responses"), "Responses", multiple = TRUE),
      column_picker(ns("factors"), "Factors (numeric)", multiple = TRUE),
      shiny::selectInput(ns("order"), "Model for each response",
                         c("Full quadratic" = "quadratic", "Linear + interactions" = "interaction", "Linear" = "linear")),
      shiny::actionButton(ns("go"), "Optimize", class = "btn-primary", icon = shiny::icon("bullseye")),
      shiny::helpText("Set a goal for each response, then click Optimize. The search stays within the range of ",
                      "the factor settings in the data.")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Goals"), shiny::uiOutput(ns("goals")),
                  shiny::helpText("Maximize: desirability rises from 0 at Lower to 1 at Target. Minimize: falls from 1 at ",
                                  "Target to 0 at Upper. Target: 1 at Target, 0 at Lower and Upper. Weight > 1 stresses ",
                                  "getting close to the target; Imp. (importance, 1-5) sets each response's share of D.")),
      bslib::layout_columns(
        col_widths = c(5, 7),
        bslib::card(bslib::card_header("Optimal settings"), shiny::tableOutput(ns("settings")),
                    shiny::uiOutput(ns("dvalue"))),
        bslib::card(bslib::card_header("Predicted responses"), shiny::tableOutput(ns("responses_tab")))
      ),
      bslib::card(shiny::plotOutput(ns("plot"), height = 520))
    )
  )
}

optimizerServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      df <- data(); nums <- numeric_cols(df)
      cand <- setdiff(nums, c("StdOrder", "RunOrder", "CenterPt", "Run"))
      shiny::updateSelectInput(session, "responses", choices = nums,
                               selected = keep_selected(session, "responses", nums, utils::tail(cand, 1)))
      shiny::updateSelectInput(session, "factors", choices = nums,
                               selected = keep_selected(session, "factors", nums, utils::head(setdiff(cand, utils::tail(cand, 1)), 3)))
    })
    resp <- shiny::reactive(intersect(input$responses, names(data())))

    output$goals <- shiny::renderUI({
      df <- data(); r <- resp()
      if (!length(r)) return(shiny::helpText("Choose one or more responses."))
      head <- shiny::fluidRow(lapply(c("Response", "Goal", "Lower", "Target", "Upper", "Weight", "Imp."),
                                     function(h) shiny::column(if (h %in% c("Response", "Goal")) 2 else 1 + (h %in% c("Lower", "Target", "Upper")),
                                                               shiny::tags$b(h))))
      rows <- lapply(seq_along(r), function(i) {
        y <- df[[r[i]]]; rg <- range(y, na.rm = TRUE)
        if (!all(is.finite(rg))) rg <- c(0, 1)
        id <- function(w) session$ns(paste0(w, "_", make.names(r[i])))
        keep <- function(w, def) shiny::isolate(input[[paste0(w, "_", make.names(r[i]))]]) %||% def
        shiny::fluidRow(class = "align-items-center",
          shiny::column(2, shiny::tags$span(r[i])),
          shiny::column(2, shiny::selectInput(id("goal"), NULL, c("Maximize" = "maximize", "Minimize" = "minimize", "Target" = "target"),
                                              selected = keep("goal", "maximize"))),
          shiny::column(2, shiny::numericInput(id("lower"), NULL, keep("lower", signif(rg[1], 4)))),
          shiny::column(2, shiny::numericInput(id("target"), NULL, keep("target", signif(rg[2], 4)))),
          shiny::column(2, shiny::numericInput(id("upper"), NULL, keep("upper", signif(rg[2] + diff(rg) * 0.1, 4)))),
          shiny::column(1, shiny::numericInput(id("weight"), NULL, keep("weight", 1), min = 0.1, max = 10, step = 0.1)),
          shiny::column(1, shiny::numericInput(id("importance"), NULL, keep("importance", 1), min = 1, max = 5)))
      })
      shiny::tagList(head, rows)
    })

    result <- shiny::eventReactive(input$go, {
      df <- data(); r <- resp(); f <- intersect(input$factors, names(df))
      shiny::validate(shiny::need(length(r) >= 1, "Choose at least one response."),
                      shiny::need(length(f) >= 1, "Choose at least one factor."),
                      shiny::need(!length(intersect(r, f)), "A column cannot be both a response and a factor."))
      # Use rows where every response and factor is present so all models share one coding.
      keep <- stats::complete.cases(df[, c(r, f), drop = FALSE])
      shiny::validate(shiny::need(sum(keep) >= 3, "Enter response values for the runs first."))
      d <- df[keep, , drop = FALSE]
      models <- lapply(r, function(y) validate_run(rsm_fit(d, y, f, input$order)))
      goals <- lapply(r, function(y) {
        g <- function(w) input[[paste0(w, "_", make.names(y))]]
        list(response = y, goal = g("goal") %||% "maximize", lower = num_or_na(g("lower")), target = num_or_na(g("target")),
             upper = num_or_na(g("upper")), weight = num_or_na(g("weight")) %||% 1, importance = num_or_na(g("importance")) %||% 1)
      })
      for (g in goals) shiny::validate(shiny::need(isTRUE(g$weight > 0) && isTRUE(g$importance > 0),
                                                   "Weights and importances must be positive."))
      opt <- shiny::withProgress(message = "Searching for the best settings...",
                                 validate_run(optimize_desirability(models, goals)))
      list(models = models, goals = goals, opt = opt)
    })

    output$settings <- shiny::renderTable(result()$opt$settings[, c("Factor", "Value")], digits = 5)
    output$dvalue <- shiny::renderUI({
      D <- result()$opt$D
      shiny::div(class = paste("alert mb-0", if (D >= 0.8) "alert-success" else if (D > 0) "alert-warning" else "alert-danger"),
                 sprintf("Overall desirability D = %.4f", D),
                 if (D == 0) " - no settings in the region meet every goal's limits; relax the limits.")
    })
    output$responses_tab <- shiny::renderTable(result()$opt$responses, digits = 5)
    output$plot <- shiny::renderPlot({ r <- result(); optimizer_plot(r$models, r$opt, r$goals) })
  })
}
