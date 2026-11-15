# Quality Tools > FMEA worksheet.

fmeaUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "FMEA",
    shiny::tagList(
      shiny::actionButton(ns("add"), "Add row", icon = shiny::icon("plus")),
      shiny::actionButton(ns("del"), "Delete selected rows", icon = shiny::icon("trash")),
      shiny::tags$hr(),
      shiny::numericInput(ns("threshold"), "RPN action threshold", 100, min = 1, max = 1000),
      shiny::numericInput(ns("sev"), "Always act when severity is at least", 9, min = 1, max = 10),
      shiny::tags$hr(),
      shiny::actionButton(ns("example"), "Load example FMEA", icon = shiny::icon("file-lines")),
      shiny::fileInput(ns("import"), "Import CSV", accept = ".csv"),
      shiny::downloadButton(ns("export"), "Export CSV"),
      shiny::helpText("Double-click a cell to edit. Rate Severity, Occurrence and Detection from 1 (best) to 10 (worst). ",
                      "'New' ratings are the expected values after the recommended action.")
    ),
    shiny::tagList(
      bslib::card(bslib::card_header("FMEA worksheet"), DT::DTOutput(ns("table"))),
      bslib::card(bslib::card_header("Prioritized failure modes"), shiny::uiOutput(ns("summary")), shiny::tableOutput(ns("ranked"))),
      bslib::layout_columns(
        col_widths = c(7, 5),
        bslib::card(shiny::plotOutput(ns("pareto"), height = 420)),
        bslib::card(shiny::plotOutput(ns("matrix"), height = 420))
      )
    )
  )
}

# Returns list(get, set) so projects can save and restore the worksheet.
fmeaServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    fm <- shiny::reactiveVal(fmea_example())
    version <- shiny::reactiveVal(0)
    set_fm <- function(d) { fm(d); version(shiny::isolate(version()) + 1) }

    output$table <- DT::renderDT({
      version()
      DT::datatable(shiny::isolate(fm()), rownames = TRUE, selection = "multiple",
                    editable = list(target = "cell", disable = list(columns = 0)),
                    options = list(pageLength = 10, scrollX = TRUE, ordering = FALSE))
    })
    shiny::observeEvent(input$table_cell_edit, {
      info <- input$table_cell_edit; d <- fm()
      i <- info$row; j <- info$col
      shiny::req(j >= 1, j <= ncol(d))
      val <- trimws(as.character(info$value))
      if (names(d)[j] %in% fmea_ratings) {
        v <- suppressWarnings(as.numeric(val))
        if (nzchar(val) && (is.na(v) || v < 1 || v > 10 || v != round(v))) {
          shiny::showNotification("Ratings must be whole numbers from 1 to 10.", type = "error")
          return(DT::replaceData(DT::dataTableProxy("table"), d, resetPaging = FALSE, rownames = TRUE))
        }
        d[i, j] <- if (nzchar(val)) v else NA
      } else d[i, j] <- if (nzchar(val)) val else NA
      fm(d)
    })
    shiny::observeEvent(input$add, set_fm(rbind(fm(), fmea_blank(1))))
    shiny::observeEvent(input$del, {
      s <- input$table_rows_selected
      if (!length(s)) return(shiny::showNotification("Select rows in the table first (click them).", type = "warning"))
      d <- fm()[-s, , drop = FALSE]
      set_fm(if (nrow(d)) d else fmea_blank(1))
    })
    shiny::observeEvent(input$example, set_fm(fmea_example()))
    shiny::observeEvent(input$import, {
      d <- tryCatch(utils::read.csv(input$import$datapath, check.names = FALSE, stringsAsFactors = FALSE),
                    error = function(e) NULL)
      if (is.null(d)) return(shiny::showNotification("Could not read that file.", type = "error"))
      out <- fmea_blank(nrow(d))
      matched <- intersect(fmea_columns, names(d))
      if (!length(matched)) return(shiny::showNotification("No FMEA columns found (expected names such as 'Failure mode', 'S', 'O', 'D').", type = "error"))
      for (cn in matched) out[[cn]] <- if (cn %in% fmea_ratings) suppressWarnings(as.numeric(d[[cn]])) else as.character(d[[cn]])
      set_fm(fmea_clean(out))
      shiny::showNotification(sprintf("Imported %d rows (%d of %d columns matched).", nrow(out), length(matched), length(fmea_columns)), type = "message")
    })
    output$export <- shiny::downloadHandler(
      filename = function() sprintf("fmea-%s.csv", format(Sys.Date())),
      content = function(file) {
        r <- fmea_compute(fm(), input$threshold, input$sev)
        out <- fm(); out$RPN <- r$RPN; out$`New RPN` <- r$`New RPN`
        utils::write.csv(out, file, row.names = FALSE, na = "")
      })

    ranked <- shiny::reactive(fmea_compute(fm(), num_or_na(input$threshold) %||% 100, num_or_na(input$sev) %||% 9))
    output$summary <- shiny::renderUI({
      r <- ranked(); ok <- !is.na(r$RPN)
      shiny::helpText(sprintf("%d failure modes rated; %d need action (RPN ≥ %g or severity ≥ %g); highest RPN %s.",
                              sum(ok), sum(grepl("^High", r$Priority)), input$threshold, input$sev,
                              if (any(ok)) format(max(r$RPN, na.rm = TRUE)) else "-"))
    })
    output$ranked <- shiny::renderTable({
      r <- ranked()
      r <- r[order(-(grepl("^High", r$Priority)), -ifelse(is.na(r$RPN), -1, r$RPN)), ]
      int_cols(r)
    }, digits = 0, na = "")
    output$pareto <- shiny::renderPlot(validate_run(fmea_pareto(ranked(), input$threshold)))
    output$matrix <- shiny::renderPlot(validate_run(fmea_matrix(ranked())))

    list(get = shiny::reactive(fm()), set = set_fm)
  })
}
