# Quality Tools > Pareto chart.

# Tally categories, sort by count and add cumulative percentages.
# `counts` NULL means each element of `categories` is one occurrence.
# Categories that together make up the last `other_pct` percent are combined
# into "Other", which is always placed last.
pareto_table <- function(categories, counts = NULL, other_pct = 0) {
  categories <- as.character(categories)
  if (is.null(counts)) counts <- rep(1, length(categories))
  if (!is.numeric(counts)) stop("The count column must be numeric.")
  keep <- !is.na(categories) & nzchar(trimws(categories)) & !is.na(counts)
  if (!any(keep)) stop("There is no data to chart.")
  if (any(counts[keep] < 0)) stop("Counts cannot be negative.")
  tot <- tapply(counts[keep], trimws(categories[keep]), sum)
  tot <- sort(tot[tot > 0], decreasing = TRUE)
  if (!length(tot)) stop("All counts are zero.")
  if (other_pct > 0 && length(tot) > 2) {
    share <- cumsum(tot) / sum(tot)
    # Keep categories until the cumulative share reaches (100 - other_pct)%.
    n_keep <- max(1, which(share >= 1 - other_pct / 100 - 1e-12)[1])
    if (n_keep < length(tot) - 1) {
      tot <- c(tot[seq_len(n_keep)], Other = sum(tot[-seq_len(n_keep)]))
    }
  }
  pct <- 100 * tot / sum(tot)
  data.frame(Category = names(tot), Count = as.numeric(tot), Percent = as.numeric(pct),
             `Cumulative %` = cumsum(as.numeric(pct)), check.names = FALSE)
}

pareto_plot <- function(tab, title = "Pareto chart", xlab = "Category", line80 = TRUE) {
  tab$Category <- factor(tab$Category, levels = tab$Category)
  scale <- sum(tab$Count)
  vital <- tab$`Cumulative %` - tab$Percent < 80
  g <- ggplot2::ggplot(tab, ggplot2::aes(Category)) +
    ggplot2::geom_col(ggplot2::aes(y = Count, fill = vital), width = 0.75) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "#4a6fa5", `FALSE` = "#b8c7da"),
                               labels = c(`TRUE` = "Vital few (up to 80%)", `FALSE` = "Useful many"), name = NULL) +
    ggplot2::geom_text(ggplot2::aes(y = Count / 2, label = Count), colour = "white", fontface = "bold", size = 3.8) +
    ggplot2::geom_line(ggplot2::aes(y = `Cumulative %` / 100 * scale, group = 1), colour = "#c0392b", linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(y = `Cumulative %` / 100 * scale), colour = "#c0392b", size = 2.5) +
    ggplot2::scale_y_continuous(name = "Count", limits = c(0, scale * 1.02), expand = ggplot2::expansion(mult = c(0, 0.03)),
                                sec.axis = ggplot2::sec_axis(~ . / scale * 100, name = "Cumulative percent",
                                                             breaks = seq(0, 100, 20))) +
    ggplot2::labs(title = title, x = xlab) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), legend.position = "top",
                   axis.text.x = ggplot2::element_text(angle = if (nrow(tab) > 6) 30 else 0,
                                                       hjust = if (nrow(tab) > 6) 1 else 0.5))
  if (line80) g <- g + ggplot2::geom_hline(yintercept = 0.8 * scale, linetype = 3, colour = "grey40")
  g
}

paretoUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Pareto chart",
    shiny::tagList(
      shiny::radioButtons(ns("source"), "Data",
                          c("Type categories and counts" = "typed",
                            "Category + count columns" = "counts",
                            "One row per defect (raw column)" = "raw")),
      shiny::conditionalPanel("input.source == 'typed'", ns = ns,
        shiny::textAreaInput(ns("typed"), "One category per line as  Category: count", rows = 9,
                             value = paste("Scratches: 54", "Dents: 38", "Wrong colour: 21", "Missing screw: 12",
                                           "Misaligned label: 9", "Cracks: 5", "Other packaging: 3", sep = "\n"))),
      shiny::conditionalPanel("input.source != 'typed'", ns = ns,
        column_picker(ns("cat"), "Category column")),
      shiny::conditionalPanel("input.source == 'counts'", ns = ns,
        column_picker(ns("count"), "Count column")),
      shiny::sliderInput(ns("other"), "Combine the smallest categories making up the last ... % into \"Other\"",
                         min = 0, max = 20, value = 0, step = 1, post = "%"),
      shiny::textInput(ns("title"), "Chart title", "Pareto chart of defects"),
      shiny::checkboxInput(ns("line80"), "Show 80% reference line", TRUE),
      shiny::downloadButton(ns("download"), "Download chart")
    ),
    shiny::tagList(
      bslib::card(shiny::plotOutput(ns("plot"), height = 480)),
      bslib::card(bslib::card_header("Pareto table"), shiny::tableOutput(ns("table")),
                  shiny::textOutput(ns("insight")))
    )
  )
}

parse_typed_counts <- function(txt) {
  lines <- trimws(strsplit(txt, "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines)]
  if (!length(lines)) stop("Enter at least one category.")
  if (!all(grepl(":", lines, fixed = TRUE))) stop("Each line needs a category, a colon and a count.")
  cats <- trimws(sub(":[^:]*$", "", lines))
  counts <- suppressWarnings(as.numeric(trimws(sub("^.*:", "", lines))))
  if (any(is.na(counts))) stop("Every count must be a number.")
  list(cats = cats, counts = counts)
}

paretoServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      df <- data()
      cats <- names(df)[!vapply(df, is.numeric, logical(1))]
      update_pickers(session, df, list(
        cat = list(type = "any", default = if (length(cats)) cats[1] else 1),
        count = list(type = "numeric", default = utils::tail(numeric_cols(df), 1))
      ))
    })

    tab <- shiny::reactive({
      df <- data()
      validate_run(switch(input$source,
        typed = { p <- parse_typed_counts(input$typed); pareto_table(p$cats, p$counts, input$other) },
        counts = {
          shiny::req(input$cat %in% names(df), input$count %in% names(df))
          pareto_table(df[[input$cat]], df[[input$count]], input$other)
        },
        raw = {
          shiny::req(input$cat %in% names(df))
          pareto_table(df[[input$cat]], NULL, input$other)
        }
      ))
    })
    xlab <- shiny::reactive(if (input$source == "typed") "Category" else input$cat)
    plot <- function() pareto_plot(tab(), input$title, xlab(), input$line80)

    output$plot <- shiny::renderPlot(plot())
    output$download <- plot_download("pareto", plot, height = 6)
    output$table <- shiny::renderTable(int_cols(tab()), digits = 1)
    output$insight <- shiny::renderText({
      t <- tab()
      k <- which(t$`Cumulative %` >= 80 - 1e-9)[1]
      sprintf("%d of %d categories (%s) account for %.0f%% of the total.",
              k, nrow(t), paste(t$Category[seq_len(k)], collapse = ", "), t$`Cumulative %`[k])
    })
  })
}
