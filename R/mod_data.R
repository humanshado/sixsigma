# Data worksheet: choose a sample dataset, upload a file, paste data or use a
# design created in the DOE menu. Cells can be edited and columns added.
# Returns a reactive holding the active data frame.

dataUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Worksheet",
    shiny::tagList(
      shiny::radioButtons(ns("source"), "Data source",
                          c("Sample dataset" = "sample", "Upload file" = "upload", "Paste data" = "paste",
                            "Created worksheet (DOE, gage study)" = "design")),
      shiny::conditionalPanel(
        "input.source == 'sample'", ns = ns,
        shiny::selectInput(ns("sample"), "Dataset", choices = sample_datasets(), selected = "ss.data.ca"),
        shiny::helpText("Datasets from the SixSigma package (book \"Six Sigma with R\") and the qcr package.")
      ),
      shiny::conditionalPanel(
        "input.source == 'upload'", ns = ns,
        shiny::fileInput(ns("file"), "CSV or Excel file", accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls")),
        shiny::selectInput(ns("sep"), "Separator (text files)",
                           c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t")),
        shiny::selectInput(ns("dec"), "Decimal mark", c("Point (.)" = ".", "Comma (,)" = ",")),
        shiny::numericInput(ns("sheet"), "Excel sheet number", 1, min = 1)
      ),
      shiny::conditionalPanel(
        "input.source == 'paste'", ns = ns,
        shiny::textAreaInput(ns("pasted"), "Paste from a spreadsheet (first row = column names)",
                             rows = 10, placeholder = "diameter\n10.4\n10.26\n9.95"),
        shiny::actionButton(ns("use_paste"), "Use pasted data", class = "btn-primary")
      ),
      shiny::conditionalPanel(
        "input.source == 'design'", ns = ns,
        shiny::helpText("Designs made in DOE > Create factorial design appear here. Type the measured ",
                        "results into the response column, then use DOE > Analyze factorial design.")
      ),
      shiny::hr(),
      shiny::tags$b("Edit the worksheet"),
      shiny::helpText("Double-click a cell to edit it. Leave a cell empty for a missing value."),
      shiny::textInput(ns("new_col"), "New column name", ""),
      shiny::radioButtons(ns("new_type"), NULL, c("Numeric" = "numeric", "Text" = "text"), inline = TRUE),
      shiny::actionButton(ns("add_col"), "Add column", icon = shiny::icon("plus")),
      shiny::hr(),
      shiny::downloadButton(ns("export"), "Export as CSV")
    ),
    shiny::tagList(
      bslib::card(
        bslib::card_header(shiny::textOutput(ns("title"), inline = TRUE)),
        DT::DTOutput(ns("table"))
      ),
      bslib::card(
        bslib::card_header("Columns"),
        shiny::tableOutput(ns("columns"))
      )
    )
  )
}

read_pasted <- function(txt) {
  if (!nzchar(trimws(txt))) stop("Nothing was pasted.")
  first <- strsplit(txt, "\n", fixed = TRUE)[[1]][1]
  sep <- if (grepl("\t", first)) "\t" else if (grepl(";", first)) ";" else if (grepl(",", first)) "," else ""
  utils::read.table(text = txt, header = TRUE, sep = sep, stringsAsFactors = FALSE,
                    check.names = TRUE, strip.white = TRUE)
}

read_upload <- function(path, name, sep, dec, sheet) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("xlsx", "xls")) {
    as.data.frame(readxl::read_excel(path, sheet = sheet))
  } else {
    utils::read.table(path, header = TRUE, sep = sep, dec = dec, stringsAsFactors = FALSE,
                      check.names = TRUE, strip.white = TRUE, quote = "\"'")
  }
}

dataServer <- function(id, injected = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    pasted <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$use_paste, {
      df <- tryCatch(read_pasted(input$pasted), error = function(e) {
        shiny::showNotification(paste("Could not read pasted data:", conditionMessage(e)), type = "error")
        NULL
      })
      pasted(df)
    })
    if (!is.null(injected)) {
      shiny::observeEvent(injected(), shiny::updateRadioButtons(session, "source", selected = "design"))
    }

    loaded <- shiny::reactive({
      switch(input$source,
        sample = {
          shiny::req(input$sample)
          list(name = input$sample, df = load_sample(input$sample))
        },
        upload = {
          shiny::req(input$file)
          df <- validate_run(read_upload(input$file$datapath, input$file$name,
                                         input$sep, input$dec, input$sheet))
          list(name = input$file$name, df = df)
        },
        paste = {
          shiny::req(pasted())
          list(name = "Pasted data", df = pasted())
        },
        design = {
          d <- if (is.null(injected)) NULL else injected()
          shiny::validate(shiny::need(!is.null(d), "No design yet. Create one in DOE > Create factorial design."))
          d
        }
      )
    })

    # The worksheet being edited. `version` changes only when the table must be
    # redrawn (new data or a new column); cell edits update it in place.
    current <- shiny::reactiveVal(NULL)
    sheet_name <- shiny::reactiveVal("")
    version <- shiny::reactiveVal(0)
    shiny::observeEvent(loaded(), {
      current(loaded()$df)
      sheet_name(loaded()$name)
      version(version() + 1)
    })

    proxy <- DT::dataTableProxy("table")
    output$table <- DT::renderDT({
      version()
      DT::datatable(shiny::isolate(current()), rownames = TRUE, selection = "none",
                    editable = list(target = "cell", disable = list(columns = 0)),
                    options = list(pageLength = 15, scrollX = TRUE))
    })

    shiny::observeEvent(input$table_cell_edit, {
      info <- input$table_cell_edit
      df <- current()
      i <- info$row; j <- info$col
      shiny::req(j >= 1, j <= ncol(df), i >= 1, i <= nrow(df))
      val <- trimws(as.character(info$value))
      col <- df[[j]]
      shown_ok <- TRUE  # the browser already shows what was typed
      if (is.numeric(col)) {
        num <- if (!nzchar(val)) NA_real_ else suppressWarnings(as.numeric(val))
        if (nzchar(val) && is.na(num)) {
          shiny::showNotification(sprintf("'%s' is not a number. Column '%s' is numeric.", val, names(df)[j]),
                                  type = "error")
          shown_ok <- FALSE
        } else {
          if (is.integer(col) && !is.na(num) && num != round(num)) col <- as.numeric(col)
          col[i] <- num
          df[[j]] <- col
        }
      } else {
        col <- as.character(col)
        col[i] <- if (nzchar(val)) val else NA_character_
        df[[j]] <- col
      }
      current(df)
      # Redraw only to undo a rejected entry; redrawing after every edit
      # swallows the next double-click.
      if (!shown_ok) DT::replaceData(proxy, df, resetPaging = FALSE, rownames = TRUE)
    })

    shiny::observeEvent(input$add_col, {
      nm <- trimws(input$new_col)
      df <- current()
      if (!nzchar(nm)) return(shiny::showNotification("Enter a name for the new column.", type = "warning"))
      if (nm %in% names(df)) return(shiny::showNotification("A column with that name already exists.", type = "warning"))
      df[[nm]] <- if (input$new_type == "numeric") rep(NA_real_, nrow(df)) else rep(NA_character_, nrow(df))
      current(df)
      version(version() + 1)
      shiny::updateTextInput(session, "new_col", value = "")
    })

    output$title <- shiny::renderText({
      df <- current(); shiny::req(df)
      sprintf("%s  -  %d rows x %d columns", sheet_name(), nrow(df), ncol(df))
    })
    output$columns <- shiny::renderTable({
      df <- current(); shiny::req(df)
      data.frame(
        Column = names(df),
        Type = vapply(df, function(v) class(v)[1], ""),
        `Non-missing` = vapply(df, function(v) sum(!is.na(v)), integer(1)),
        Missing = vapply(df, function(v) sum(is.na(v)), integer(1)),
        check.names = FALSE
      )
    })
    output$export <- shiny::downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9_.-]", "_", sheet_name()), ".csv"),
      content = function(file) utils::write.csv(current(), file, row.names = FALSE)
    )

    shiny::reactive({
      shiny::req(current())
      current()
    })
  })
}
