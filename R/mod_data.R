# Data worksheet: choose a sample dataset, upload a file or paste data.
# Returns a reactive holding the active data frame.

dataUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Worksheet",
    shiny::tagList(
      shiny::radioButtons(ns("source"), "Data source",
                          c("Sample dataset" = "sample", "Upload file" = "upload", "Paste data" = "paste")),
      shiny::conditionalPanel(
        "input.source == 'sample'", ns = ns,
        shiny::selectInput(ns("sample"), "Dataset", choices = sample_datasets(), selected = "ss.data.ca"),
        shiny::helpText("Datasets from the SixSigma package and the book \"Six Sigma with R\".")
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

dataServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    pasted <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$use_paste, {
      df <- tryCatch(read_pasted(input$pasted), error = function(e) {
        shiny::showNotification(paste("Could not read pasted data:", conditionMessage(e)), type = "error")
        NULL
      })
      pasted(df)
    })

    data <- shiny::reactive({
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
        }
      )
    })

    output$title <- shiny::renderText({
      d <- data()
      sprintf("%s  -  %d rows x %d columns", d$name, nrow(d$df), ncol(d$df))
    })
    output$table <- DT::renderDT(
      DT::datatable(data()$df, rownames = TRUE,
                    options = list(pageLength = 15, scrollX = TRUE))
    )
    output$columns <- shiny::renderTable({
      df <- data()$df
      data.frame(
        Column = names(df),
        Type = vapply(df, function(v) class(v)[1], ""),
        `Non-missing` = vapply(df, function(v) sum(!is.na(v)), integer(1)),
        Missing = vapply(df, function(v) sum(is.na(v)), integer(1)),
        check.names = FALSE
      )
    })
    output$export <- shiny::downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9_.-]", "_", data()$name), ".csv"),
      content = function(file) utils::write.csv(data()$df, file, row.names = FALSE)
    )

    shiny::reactive(data()$df)
  })
}
