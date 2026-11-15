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
        bslib::card_header(shiny::div(class = "d-flex justify-content-between align-items-center",
          "Transform data",
          shiny::actionButton(ns("undo"), "Undo", icon = shiny::icon("rotate-left"), class = "btn-sm"))),
        shiny::fluidRow(
          shiny::column(4, shiny::selectInput(ns("op"), NULL, c(
            "Filter rows" = "subset", "Sort rows" = "sort", "Calculated column (formula)" = "calc",
            "Stack columns (wide to long)" = "stack", "Unstack a column by group (long to wide)" = "unstack",
            "Recode values" = "recode", "Change column type" = "convert", "Rename a column" = "rename",
            "Delete columns" = "delete", "Remove rows with missing values" = "dropna"))),
          shiny::column(8, shiny::uiOutput(ns("op_ui")))),
        shiny::actionButton(ns("apply"), "Apply", class = "btn-primary", icon = shiny::icon("check")),
        shiny::textOutput(ns("history"), inline = TRUE)
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
    history <- shiny::reactiveVal(list())
    # Replace the worksheet, keeping the previous version for Undo.
    change <- function(df_new, redraw = TRUE) {
      prev <- shiny::isolate(current())
      if (!is.null(prev)) history(utils::head(c(list(prev), shiny::isolate(history())), 25))
      current(df_new)
      if (redraw) version(shiny::isolate(version()) + 1)
    }
    sheet_name <- shiny::reactiveVal("")
    version <- shiny::reactiveVal(0)
    shiny::observeEvent(loaded(), {
      history(list())
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
      change(df, redraw = FALSE)
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
      change(df)
      shiny::updateTextInput(session, "new_col", value = "")
    })

    output$op_ui <- shiny::renderUI({
      df <- current(); shiny::req(df)
      nm <- names(df); ns <- session$ns
      col <- function(id, label, multiple = FALSE, sel = NULL) shiny::selectInput(ns(id), label, nm, multiple = multiple, selected = sel)
      switch(input$op,
        subset = shiny::fluidRow(shiny::column(4, col("sub_col", "Column")),
                                 shiny::column(4, shiny::selectInput(ns("sub_op"), "Condition", dt_subset_ops)),
                                 shiny::column(4, shiny::textInput(ns("sub_val"), "Value", ""))),
        sort = shiny::fluidRow(shiny::column(6, col("sort_col", "Sort by")),
                               shiny::column(6, shiny::checkboxInput(ns("sort_desc"), "Largest first", FALSE))),
        calc = shiny::tagList(shiny::fluidRow(shiny::column(4, shiny::textInput(ns("calc_name"), "New column", "Calc")),
                                              shiny::column(8, shiny::textInput(ns("calc_formula"), "Formula", "",
                                                                                placeholder = "e.g. (Length - mean(Length)) / sd(Length)"))),
                              shiny::helpText("Use column names, numbers and + - * / ^ ( ) comparisons, and: ",
                                              paste(setdiff(dt_allowed_functions, c("+", "-", "*", "/", "^", "%%", "%/%", "(", "==", "!=", "<", ">", "<=", ">=", "&", "|", "!")),
                                                    collapse = ", "), ". Put names with spaces in backticks.")),
        stack = shiny::tagList(col("stack_cols", "Columns to stack", TRUE),
                               shiny::fluidRow(shiny::column(4, col("stack_keep", "Columns to repeat (optional)", TRUE)),
                                               shiny::column(4, shiny::textInput(ns("stack_value"), "Name for values", "Value")),
                                               shiny::column(4, shiny::textInput(ns("stack_label"), "Name for labels", "Source")))),
        unstack = shiny::fluidRow(shiny::column(6, col("un_value", "Values")), shiny::column(6, col("un_by", "Group by", sel = nm[min(2, length(nm))]))),
        recode = shiny::fluidRow(shiny::column(4, col("rec_col", "Column"), shiny::textInput(ns("rec_new"), "Save as (blank = replace)", "")),
                                 shiny::column(8, shiny::textAreaInput(ns("rec_rules"), "Rules: one per line as  old = new", rows = 4))),
        convert = shiny::fluidRow(shiny::column(6, col("conv_col", "Column")),
                                  shiny::column(6, shiny::selectInput(ns("conv_type"), "New type", c("Numeric" = "numeric", "Text" = "text", "Category" = "category")))),
        rename = shiny::fluidRow(shiny::column(6, col("ren_col", "Column")), shiny::column(6, shiny::textInput(ns("ren_new"), "New name", ""))),
        delete = col("del_cols", "Columns to delete", TRUE),
        dropna = col("na_cols", "Check these columns for missing values", TRUE, sel = nm))
    })
    shiny::observeEvent(input$apply, {
      df <- current(); shiny::req(df)
      res <- tryCatch(switch(input$op,
        subset = dt_subset(df, input$sub_col, input$sub_op, input$sub_val),
        sort = dt_sort(df, input$sort_col, isTRUE(input$sort_desc)),
        calc = dt_calc(df, input$calc_name, input$calc_formula),
        stack = dt_stack(df, input$stack_cols, input$stack_value, input$stack_label, input$stack_keep),
        unstack = dt_unstack(df, input$un_value, input$un_by),
        recode = dt_recode(df, input$rec_col, input$rec_rules, input$rec_new),
        convert = dt_convert(df, input$conv_col, input$conv_type),
        rename = dt_rename(df, input$ren_col, input$ren_new),
        delete = dt_delete(df, input$del_cols),
        dropna = dt_drop_missing(df, if (length(input$na_cols)) input$na_cols else names(df))),
        error = function(e) { shiny::showNotification(conditionMessage(e), type = "error"); NULL })
      if (is.null(res)) return()
      if (!nrow(res)) return(shiny::showNotification("That would leave no rows; nothing was changed.", type = "warning"))
      change(res)
      shiny::showNotification(sprintf("Done: %d rows x %d columns. Use Undo to reverse.", nrow(res), ncol(res)), type = "message")
    })
    shiny::observeEvent(input$undo, {
      h <- history()
      if (!length(h)) return(shiny::showNotification("Nothing to undo.", type = "warning"))
      current(h[[1]]); history(h[-1]); version(version() + 1)
    })
    output$history <- shiny::renderText({
      n <- length(history()); if (n) sprintf("  %d change(s) can be undone.", n) else ""
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
