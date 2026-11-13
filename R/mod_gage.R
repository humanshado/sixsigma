# Quality Tools > Gage R&R (SixSigma::ss.rr).

gageUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Gage R&R study",
    shiny::tagList(
      column_picker(ns("var"), "Measurement"),
      column_picker(ns("part"), "Part"),
      column_picker(ns("appr"), "Operator (appraiser)"),
      shiny::radioButtons(ns("method"), "Design", c("Crossed" = "crossed", "Nested" = "nested")),
      blank_numeric(ns("lsl"), "Lower spec limit (optional)"),
      blank_numeric(ns("usl"), "Upper spec limit (optional)"),
      shiny::numericInput(ns("sigma"), "Study variation (number of sigmas)", 6, min = 1, max = 6),
      shiny::numericInput(ns("alpha"), "Alpha to remove interaction term", 0.05, min = 0, max = 1, step = 0.01),
      shiny::downloadButton(ns("download"), "Download report image"),
      shiny::helpText("Each row is one measurement. Use the ss.data.rr sample dataset for an example."),
      shiny::tags$hr(),
      shiny::tags$b("Plan a new study"),
      shiny::fluidRow(
        shiny::column(4, shiny::numericInput(ns("plan_parts"), "Parts", 10, min = 2, max = 50)),
        shiny::column(4, shiny::numericInput(ns("plan_ops"), "Operators", 3, min = 1, max = 10)),
        shiny::column(4, shiny::numericInput(ns("plan_reps"), "Trials", 2, min = 1, max = 5))),
      shiny::actionButton(ns("plan"), "Create study worksheet", icon = shiny::icon("table")),
      shiny::helpText("Creates a worksheet with the part order randomized within each operator and trial.")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Gage R&R report"), shiny::plotOutput(ns("plot"), height = 700)),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(bslib::card_header("Variance components"), shiny::tableOutput(ns("varcomp"))),
        bslib::card(bslib::card_header("Study variation"), shiny::tableOutput(ns("studyvar")),
                    shiny::textOutput(ns("ncat")),
                    shiny::helpText("Total Gage R&R under 10% of study variation is generally acceptable; ",
                                    "10-30% may be acceptable; over 30% needs improvement."))
      ),
      bslib::card(bslib::card_header("ANOVA output"), shiny::verbatimTextOutput(ns("anova")))
    )
  )
}

gageServer <- function(id, data, on_create = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$plan, {
      res <- tryCatch(gage_rr_worksheet(input$plan_parts, input$plan_ops, input$plan_reps),
                      error = function(e) { shiny::showNotification(conditionMessage(e), type = "error"); NULL })
      if (is.null(res) || is.null(on_create)) return()
      on_create(list(name = sprintf("Gage R&R study, %d measurements", nrow(res)), df = res))
      shiny::showNotification("Study worksheet created. Enter each measurement in the Measurement column.", type = "message")
    })
    shiny::observeEvent(data(), {
      df <- data()
      fac <- names(df)[!vapply(df, is.numeric, logical(1))]
      update_pickers(session, df, list(
        var = list(type = "numeric", default = 1),
        part = list(type = "any", default = if (length(fac)) fac[1] else 1),
        appr = list(type = "any", default = if (length(fac) > 1) fac[2] else 2)
      ))
    })

    call_rr <- function(print_plot) {
      df <- data()
      shiny::req(all(c(input$var, input$part, input$appr) %in% names(df)))
      shiny::validate(shiny::need(length(unique(c(input$var, input$part, input$appr))) == 3,
                                  "Measurement, part and operator must be different columns."))
      lsl <- num_or_na(input$lsl); usl <- num_or_na(input$usl)
      df <- df[stats::complete.cases(df[, c(input$var, input$part, input$appr)]), , drop = FALSE]
      SixSigma::ss.rr(var = input$var, part = input$part, appr = input$appr, data = df,
                      lsl = lsl, usl = usl, sigma = input$sigma, alphaLim = input$alpha,
                      method = input$method, print_plot = print_plot)
    }

    result <- shiny::reactive({
      out <- NULL
      txt <- validate_run(capture_text(out <- without_plot(call_rr(FALSE))))
      list(res = out, text = txt)
    })

    output$plot <- shiny::renderPlot(validate_run(invisible(capture_text(call_rr(TRUE)))))
    output$download <- plot_download("gage-rr", function() invisible(capture_text(call_rr(TRUE))), height = 9)

    output$varcomp <- shiny::renderTable({
      vc <- as.data.frame(result()$res$varComp)
      cbind(Source = rownames(vc), vc)
    }, digits = 4)
    output$studyvar <- shiny::renderTable({
      sv <- as.data.frame(result()$res$studyVar)
      sv <- sv[, colSums(!is.na(sv)) > 0, drop = FALSE]
      cbind(Source = rownames(sv), sv)
    }, digits = 4)
    output$ncat <- shiny::renderText(paste("Number of distinct categories:", result()$res$ncat))
    output$anova <- shiny::renderText(result()$text)
  })
}
