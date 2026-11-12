# Stat > Hypothesis tests.

test_types <- list(
  "Means" = c("1-sample t" = "one_t", "2-sample t" = "two_t", "Paired t" = "paired_t",
              "One-way ANOVA" = "anova"),
  "Variation" = c("Equal variances" = "variances"),
  "Nonparametric" = c("Mann-Whitney / Kruskal-Wallis" = "nonpar"),
  "Proportions and counts" = c("1 proportion" = "one_prop", "2 proportions" = "two_prop",
                               "Chi-square association" = "chisq")
)

hypothesisUI <- function(id) {
  ns <- shiny::NS(id)
  is_type <- function(...) sprintf("[%s].indexOf(input.type) >= 0", paste0("'", c(...), "'", collapse = ","))
  analysis_layout(
    "Hypothesis tests",
    shiny::tagList(
      shiny::selectInput(ns("type"), "Test", test_types),
      # one-sample
      shiny::conditionalPanel(is_type("one_t"), ns = ns,
        column_picker(ns("one_var"), "Sample column"),
        blank_numeric(ns("mu0"), "Hypothesised mean")),
      # grouped comparisons
      shiny::conditionalPanel(is_type("two_t", "anova", "variances", "nonpar"), ns = ns,
        shiny::radioButtons(ns("layout"), "Data layout",
                            c("Response + group column" = "stacked", "Each sample in its own column" = "unstacked")),
        shiny::conditionalPanel("input.layout == 'stacked'", ns = ns,
          column_picker(ns("resp"), "Response"),
          column_picker(ns("grp"), "Group")),
        shiny::conditionalPanel("input.layout == 'unstacked'", ns = ns,
          column_picker(ns("cols"), "Sample columns", multiple = TRUE))),
      shiny::conditionalPanel(is_type("two_t"), ns = ns,
        shiny::checkboxInput(ns("equal_var"), "Assume equal variances", FALSE)),
      # paired
      shiny::conditionalPanel(is_type("paired_t"), ns = ns,
        column_picker(ns("pair1"), "First sample"),
        column_picker(ns("pair2"), "Second sample")),
      # proportions
      shiny::conditionalPanel(is_type("one_prop"), ns = ns,
        shiny::numericInput(ns("x"), "Number of events", 12, min = 0),
        shiny::numericInput(ns("n"), "Number of trials", 200, min = 1),
        shiny::numericInput(ns("p0"), "Hypothesised proportion", 0.05, min = 0, max = 1, step = 0.01)),
      shiny::conditionalPanel(is_type("two_prop"), ns = ns,
        shiny::fluidRow(
          shiny::column(6, shiny::numericInput(ns("x1"), "Events 1", 18, min = 0),
                           shiny::numericInput(ns("n1"), "Trials 1", 300, min = 1)),
          shiny::column(6, shiny::numericInput(ns("x2"), "Events 2", 32, min = 0),
                           shiny::numericInput(ns("n2"), "Trials 2", 310, min = 1)))),
      # chi-square
      shiny::conditionalPanel(is_type("chisq"), ns = ns,
        column_picker(ns("cat1"), "Row variable"),
        column_picker(ns("cat2"), "Column variable")),
      # common options
      shiny::conditionalPanel(is_type("one_t", "two_t", "paired_t", "nonpar", "one_prop", "two_prop"), ns = ns,
        shiny::selectInput(ns("alt"), "Alternative hypothesis",
                           c("Not equal" = "two.sided", "Less than" = "less", "Greater than" = "greater"))),
      shiny::numericInput(ns("conf"), "Confidence level (%)", 95, min = 50, max = 99.9, step = 0.5),
      shiny::downloadButton(ns("download"), "Download chart")
    ),
    shiny::tagList(
      bslib::card(
        bslib::card_header(shiny::textOutput(ns("title"), inline = TRUE)),
        shiny::uiOutput(ns("hypotheses")),
        shiny::uiOutput(ns("conclusion"))
      ),
      bslib::layout_columns(
        col_widths = c(12, 12),
        bslib::card(bslib::card_header(shiny::textOutput(ns("summary_title"), inline = TRUE)),
                    shiny::tableOutput(ns("summary"))),
        bslib::card(bslib::card_header("Test results"), shiny::tableOutput(ns("result")))
      ),
      shiny::uiOutput(ns("extra_card")),
      bslib::card(shiny::plotOutput(ns("plot"), height = 420))
    )
  )
}

hypothesisServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      df <- data()
      nums <- numeric_cols(df)
      cats <- names(df)[!vapply(df, is.numeric, logical(1))]
      update_pickers(session, df, list(
        one_var = list(type = "numeric", default = 1),
        resp = list(type = "numeric", default = 1),
        grp = list(type = "any", default = if (length(cats)) cats[1] else 1),
        pair1 = list(type = "numeric", default = 1),
        pair2 = list(type = "numeric", default = 2),
        cat1 = list(type = "any", default = if (length(cats)) cats[1] else 1),
        cat2 = list(type = "any", default = if (length(cats) > 1) cats[2] else 2)
      ))
      shiny::updateSelectInput(session, "cols", choices = nums, selected = utils::head(nums, 2))
    })

    samples <- function(max_groups = Inf) {
      df <- data()
      if (input$layout == "stacked") {
        shiny::req(input$resp %in% names(df), input$grp %in% names(df))
        shiny::validate(shiny::need(input$resp != input$grp, "Response and group must be different columns."))
        by_group(df[[input$resp]], df[[input$grp]], max_groups = max_groups)
      } else {
        cols <- intersect(input$cols, names(df))
        shiny::validate(shiny::need(length(cols) >= 2, "Choose at least two sample columns."))
        if (length(cols) > max_groups) stop("Choose exactly ", max_groups, " columns for this test.")
        sp <- lapply(cols, function(cn) clean_num(df[[cn]], cn))
        stats::setNames(sp, cols)
      }
    }
    ylab <- function() if (input$layout == "stacked") input$resp else "Value"

    res <- shiny::reactive({
      conf <- input$conf / 100
      shiny::validate(shiny::need(isTRUE(conf > 0 && conf < 1), "Confidence level must be between 0 and 100."))
      df <- data()
      validate_run(switch(input$type,
        one_t = {
          shiny::req(input$one_var %in% names(df))
          mu0 <- num_or_na(input$mu0)
          shiny::validate(shiny::need(!is.na(mu0), "Enter the hypothesised mean."))
          ht_one_t(df[[input$one_var]], mu0, input$alt, conf, input$one_var)
        },
        two_t = ht_two_t(samples(2), input$alt, conf, input$equal_var, ylab()),
        paired_t = {
          shiny::req(input$pair1 %in% names(df), input$pair2 %in% names(df))
          shiny::validate(shiny::need(input$pair1 != input$pair2, "Choose two different columns."))
          ht_paired_t(df[[input$pair1]], df[[input$pair2]], input$alt, conf, c(input$pair1, input$pair2))
        },
        anova = ht_anova(samples(), conf, ylab()),
        variances = ht_variances(samples(), conf, ylab()),
        nonpar = ht_nonparametric(samples(), input$alt, ylab()),
        one_prop = ht_one_prop(num_or_na(input$x), num_or_na(input$n), num_or_na(input$p0), input$alt, conf),
        two_prop = ht_two_prop(num_or_na(input$x1), num_or_na(input$n1), num_or_na(input$x2),
                               num_or_na(input$n2), input$alt, conf),
        chisq = {
          shiny::req(input$cat1 %in% names(df), input$cat2 %in% names(df))
          shiny::validate(shiny::need(input$cat1 != input$cat2, "Choose two different columns."))
          ht_chisq(df[[input$cat1]], df[[input$cat2]], c(input$cat1, input$cat2))
        }
      ))
    })

    alpha <- shiny::reactive(round(1 - input$conf / 100, 6))

    output$title <- shiny::renderText(res()$title)
    output$hypotheses <- shiny::renderUI(shiny::tags$p(
      shiny::tags$b("Null hypothesis H₀: "), res()$h0, shiny::tags$br(),
      shiny::tags$b("Alternative H₁: "), res()$h1, shiny::tags$br(),
      shiny::tags$b("Significance level α: "), alpha()
    ))
    output$conclusion <- shiny::renderUI({
      r <- res()
      sig <- !is.na(r$p) && r$p < alpha()
      shiny::tagList(
        shiny::div(class = paste("alert mb-2", if (sig) "alert-warning" else "alert-success"),
                   test_conclusion(r$p, alpha())),
        if (!is.null(r$note)) shiny::helpText(r$note)
      )
    })
    output$summary_title <- shiny::renderText(if (is.null(res()$summary_title)) "Sample statistics" else res()$summary_title)
    output$summary <- shiny::renderTable(int_cols(res()$summary), digits = 4, na = "")
    output$result <- shiny::renderTable(int_cols(res()$result), digits = 4, na = "")
    output$extra_card <- shiny::renderUI({
      r <- res()
      if (is.null(r$extra)) return(NULL)
      bslib::card(bslib::card_header(r$extra_title), shiny::tableOutput(session$ns("extra")))
    })
    output$extra <- shiny::renderTable(int_cols(res()$extra), digits = 4, na = "")
    output$plot <- shiny::renderPlot(res()$plot)
    output$download <- plot_download("hypothesis-test", function() res()$plot, height = 5)
  })
}
