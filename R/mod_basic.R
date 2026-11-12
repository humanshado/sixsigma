# Stat > Basic Statistics: descriptive summary, normality, confidence interval.

basicUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Basic statistics",
    shiny::tagList(
      column_picker(ns("var"), "Variable"),
      column_picker(ns("by"), "By group (optional)"),
      shiny::numericInput(ns("conf"), "Confidence level (%)", 95, min = 50, max = 99.9, step = 0.5),
      shiny::numericInput(ns("bins"), "Histogram bins", 15, min = 3, max = 100)
    ),
    shiny::tagList(
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(bslib::card_header("Histogram with normal fit"),
                    shiny::plotOutput(ns("hist"), height = 340)),
        bslib::card(bslib::card_header("Normal probability plot"),
                    shiny::plotOutput(ns("qq"), height = 340))
      ),
      bslib::card(bslib::card_header("Descriptive statistics"), shiny::tableOutput(ns("desc"))),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(bslib::card_header("Normality tests"), shiny::tableOutput(ns("norm")),
                    shiny::helpText("A p-value below 0.05 suggests the data are not normally distributed.")),
        bslib::card(bslib::card_header("Confidence interval for the mean (SixSigma::ss.ci)"),
                    shiny::verbatimTextOutput(ns("ci")),
                    shiny::plotOutput(ns("ci_plot"), height = 300))
      )
    )
  )
}

describe <- function(x) {
  x <- x[!is.na(x)]
  q <- stats::quantile(x, c(0.25, 0.5, 0.75), names = FALSE)
  data.frame(N = length(x), Mean = mean(x), `Std. dev.` = stats::sd(x),
             `SE mean` = stats::sd(x) / sqrt(length(x)), Minimum = min(x), Q1 = q[1],
             Median = q[2], Q3 = q[3], Maximum = max(x),
             Skewness = e1071::skewness(x, type = 2), Kurtosis = e1071::kurtosis(x, type = 2),
             check.names = FALSE)
}

basicServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), update_pickers(session, data(), list(
      var = list(type = "numeric", default = 1),
      by = list(type = "any", optional = TRUE)
    )))

    x <- shiny::reactive({
      df <- data()
      shiny::req(input$var %in% names(df))
      v <- df[[input$var]]
      shiny::validate(shiny::need(sum(!is.na(v)) >= 3, "At least three values are needed."))
      v
    })
    groups <- shiny::reactive({
      by <- empty_to_null(input$by)
      if (is.null(by) || !(by %in% names(data()))) NULL else factor(data()[[by]])
    })

    output$desc <- shiny::renderTable({
      if (is.null(groups())) {
        cbind(Variable = input$var, describe(x()))
      } else {
        parts <- split(x(), groups())
        parts <- parts[vapply(parts, function(v) sum(!is.na(v)) > 1, logical(1))]
        do.call(rbind, lapply(names(parts), function(g) cbind(Group = g, describe(parts[[g]]))))
      }
    }, digits = 4)

    output$hist <- shiny::renderPlot({
      v <- x()[!is.na(x())]
      df <- data.frame(v = v)
      g <- ggplot2::ggplot(df, ggplot2::aes(v)) +
        ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                bins = input$bins, fill = "#9bb7d4", colour = "white") +
        ggplot2::stat_function(fun = stats::dnorm, args = list(mean = mean(v), sd = stats::sd(v)),
                               colour = "#c0392b", linewidth = 1) +
        ggplot2::labs(x = input$var, y = "Density") +
        ggplot2::theme_minimal(base_size = 13)
      g
    })

    output$qq <- shiny::renderPlot({
      v <- x()[!is.na(x())]
      ggplot2::ggplot(data.frame(v = v), ggplot2::aes(sample = v)) +
        ggplot2::stat_qq(colour = "#4a6fa5") + ggplot2::stat_qq_line(colour = "#c0392b") +
        ggplot2::labs(x = "Theoretical normal quantiles", y = input$var) +
        ggplot2::theme_minimal(base_size = 13)
    })

    output$norm <- shiny::renderTable({
      v <- x()[!is.na(x())]
      rows <- list()
      if (length(v) >= 8) {
        ad <- nortest::ad.test(v)
        rows[[length(rows) + 1]] <- data.frame(Test = "Anderson-Darling", Statistic = unname(ad$statistic), `p-value` = ad$p.value, check.names = FALSE)
      }
      if (length(v) <= 5000) {
        sw <- stats::shapiro.test(v)
        rows[[length(rows) + 1]] <- data.frame(Test = "Shapiro-Wilk", Statistic = unname(sw$statistic), `p-value` = sw$p.value, check.names = FALSE)
      }
      do.call(rbind, rows)
    }, digits = 4)

    alpha <- shiny::reactive({
      a <- 1 - input$conf / 100
      shiny::validate(shiny::need(isTRUE(a > 0 && a < 1), "Confidence level must be between 0 and 100."))
      a
    })
    output$ci <- shiny::renderText({
      v <- x()
      validate_run(capture_text(without_plot(
        suppressWarnings(SixSigma::ss.ci(v, alpha = alpha(), xname = input$var)))))
    })
    output$ci_plot <- shiny::renderPlot({
      v <- x()
      validate_run(invisible(capture_text(
        suppressWarnings(SixSigma::ss.ci(v, alpha = alpha(), xname = input$var)))))
    })
  })
}
