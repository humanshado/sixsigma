# Quality Tools > Attribute agreement analysis.

agreementUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Attribute agreement analysis",
    shiny::tagList(
      column_picker(ns("sample"), "Sample (part) column"),
      column_picker(ns("appraiser"), "Appraiser column"),
      column_picker(ns("response"), "Rating (response) column"),
      column_picker(ns("standard"), "Known standard (optional)"),
      shiny::checkboxInput(ns("ordinal"), "Ratings are ordinal (e.g. 1 = poor ... 5 = excellent)", FALSE),
      shiny::numericInput(ns("conf"), "Confidence level (%)", 95, min = 50, max = 99.9),
      shiny::downloadButton(ns("download"), "Download chart"),
      shiny::helpText("One row per rating. Every appraiser rates every sample the same number of times. ",
                      "Try the Examples dataset attribute_agreement.")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("Summary"), shiny::uiOutput(ns("summary"))),
      bslib::card(shiny::plotOutput(ns("plot"), height = 360)),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(bslib::card_header("Within appraisers (repeatability)"), shiny::tableOutput(ns("within"))),
        bslib::card(bslib::card_header("Each appraiser vs standard"), shiny::tableOutput(ns("vs_std")))
      ),
      shiny::uiOutput(ns("disagree_card")),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(bslib::card_header("Between appraisers (reproducibility)"), shiny::tableOutput(ns("between")),
                    shiny::tableOutput(ns("between_kappa"))),
        bslib::card(bslib::card_header("All appraisers vs standard"), shiny::tableOutput(ns("all_std")))
      ),
      shiny::uiOutput(ns("kendall_card")),
      bslib::card(bslib::card_header("Fleiss' kappa by appraiser"), shiny::uiOutput(ns("kappas")),
                  shiny::helpText("Kappa: 1 = perfect agreement, 0 = no better than chance. As a rule of thumb, ",
                                  "kappa above 0.9 is excellent and below 0.7 needs improvement."))
    )
  )
}

agreementServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      df <- data(); nm <- names(df)
      pick <- function(pat, def) { h <- nm[grepl(pat, nm, ignore.case = TRUE)]; if (length(h)) h[1] else def }
      update_pickers(session, df, list(
        sample = list(type = "any", default = pick("sample|part|item", 1)),
        appraiser = list(type = "any", default = pick("apprais|operator|inspector|rater", 2)),
        response = list(type = "any", default = pick("result|response|rating|score|decision", 3)),
        standard = list(type = "any", optional = TRUE, default = pick("standard|reference|truth|master", ""))))
    })
    res <- shiny::reactive({
      df <- data()
      shiny::req(all(c(input$sample, input$appraiser, input$response) %in% names(df)))
      conf <- input$conf / 100
      shiny::validate(shiny::need(isTRUE(conf > 0 && conf < 1), "Confidence level must be between 0 and 100."))
      validate_run(agreement_analysis(df, input$sample, input$appraiser, input$response, empty_to_null(input$standard),
                                      conf, isTRUE(input$ordinal)))
    })
    output$summary <- shiny::renderUI({
      a <- res()
      items <- list(sprintf("%d samples, %d appraisers, %d trials each; responses: %s.", a$n_samples, length(a$appraisers),
                            a$trials, paste(a$levels, collapse = ", ")),
                    sprintf("Between appraisers: %.0f%% of samples rated identically by everyone (Fleiss' kappa %.3f).",
                            a$between$Percent, a$between_kappa$Kappa[nrow(a$between_kappa)]))
      if (!is.null(a$all_std)) items <- c(items, sprintf("All appraisers matched the standard on every trial for %.0f%% of samples.",
                                                          a$all_std$Percent))
      if (a$trials < 2) items <- c(items, "Only one trial per appraiser: within-appraiser repeatability cannot be assessed.")
      shiny::tags$ul(lapply(items, shiny::tags$li))
    })
    output$plot <- shiny::renderPlot({ g <- agreement_plot(res()); shiny::req(g); g })
    output$within <- shiny::renderTable({ w <- res()$within; if (is.null(w)) data.frame(Note = "Needs two or more trials.") else int_cols(w) }, digits = 2)
    output$vs_std <- shiny::renderTable({ v <- res()$vs_std; if (is.null(v)) data.frame(Note = "No standard column chosen.") else int_cols(v) }, digits = 2)
    output$disagree_card <- shiny::renderUI({
      if (is.null(res()$disagree)) return(NULL)
      bslib::card(bslib::card_header("Disagreement with the standard"), shiny::tableOutput(session$ns("disagree")),
                  shiny::helpText("Consistently wrong: every trial differs from the standard. Mixed: the appraiser gave different answers across trials."))
    })
    output$disagree <- shiny::renderTable(int_cols(res()$disagree), digits = 1)
    output$between <- shiny::renderTable(int_cols(res()$between), digits = 2)
    output$between_kappa <- shiny::renderTable(res()$between_kappa, digits = 4)
    output$all_std <- shiny::renderTable({ v <- res()$all_std; if (is.null(v)) data.frame(Note = "No standard column chosen.") else int_cols(v) }, digits = 2)
    output$kendall_card <- shiny::renderUI({
      if (is.null(res()$kendall)) return(NULL)
      bslib::card(bslib::card_header("Kendall's coefficient of concordance (ordinal ratings)"), shiny::tableOutput(session$ns("kendall")),
                  shiny::helpText("W near 1 means the rankings agree closely; a small p-value means the agreement is not due to chance."))
    })
    output$kendall <- shiny::renderTable(int_cols(res()$kendall), digits = 4)
    output$kappas <- shiny::renderUI({
      a <- res()
      blocks <- list()
      if (!is.null(a$within_kappa)) blocks <- c(blocks, lapply(names(a$within_kappa), function(n)
        shiny::tagList(shiny::tags$b(sprintf("%s - within (trial vs trial)", n)), shiny::renderTable(a$within_kappa[[n]], digits = 4))))
      if (!is.null(a$vs_std_kappa)) blocks <- c(blocks, lapply(names(a$vs_std_kappa), function(n)
        shiny::tagList(shiny::tags$b(sprintf("%s - vs standard (each trial)", n)), shiny::renderTable(a$vs_std_kappa[[n]], digits = 4))))
      shiny::tagList(blocks)
    })
    output$download <- plot_download("attribute-agreement", function() agreement_plot(res()), height = 5)
  })
}
