# Data worksheet: choose a sample dataset, upload a file, paste data or use a
# design created in the DOE menu. Cells can be edited and columns added.
# Several worksheets can be open. Returns list(data = reactive active data frame,
# get/set for project files).

dataUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Worksheet",
    shiny::tagList(
      shiny::radioButtons(ns("source"), "Open data from",
                          c("Sample dataset" = "sample", "Upload file" = "upload", "Paste data" = "paste")),
      shiny::helpText("New data replaces the current worksheet only if it has not been changed; otherwise it ",
                      "opens in a new worksheet. Designs and study worksheets from other menus always open in a new one."),
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
        bslib::card_header(shiny::div(class = "d-flex flex-wrap gap-2 align-items-center",
          shiny::tags$span(shiny::icon("table"), "Worksheet"),
          shiny::div(style = "min-width: 260px; margin-bottom: -1rem;", shiny::selectInput(ns("ws"), NULL, NULL)),
          shiny::actionButton(ns("ws_new"), "New", icon = shiny::icon("file"), class = "btn-sm"),
          shiny::actionButton(ns("ws_dup"), "Duplicate", icon = shiny::icon("copy"), class = "btn-sm"),
          shiny::actionButton(ns("ws_rename"), "Rename", icon = shiny::icon("pen"), class = "btn-sm"),
          shiny::actionButton(ns("ws_close"), "Close", icon = shiny::icon("xmark"), class = "btn-sm"),
          shiny::div(class = "ms-auto",
            shiny::actionButton(ns("undo"), "Undo", icon = shiny::icon("rotate-left"), class = "btn-sm"),
            shiny::actionButton(ns("redo"), "Redo", icon = shiny::icon("rotate-right"), class = "btn-sm")))),
        shiny::tags$div(class = "fw-semibold", shiny::textOutput(ns("title"), inline = TRUE)),
        DT::DTOutput(ns("table"))
      ),
      bslib::card(
        bslib::card_header("Transform data"),
        shiny::fluidRow(
          shiny::column(4, shiny::selectInput(ns("op"), NULL, c(
            "Filter rows" = "subset", "Sort rows" = "sort", "Calculated column (formula)" = "calc",
            "Stack columns (wide to long)" = "stack", "Unstack a column by group (long to wide)" = "unstack",
            "Recode values" = "recode", "Change column type" = "convert", "Rename a column" = "rename",
            "Delete columns" = "delete", "Remove rows with missing values" = "dropna",
            "Append rows from another worksheet" = "append", "Join columns from another worksheet" = "join"))),
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
        })
    })

    # All open worksheets (see R/worksheets.R). `version` changes only when the
    # table must be redrawn; cell edits update the data in place.
    store <- shiny::reactiveVal(ws_store())
    version <- shiny::reactiveVal(0)
    current <- shiny::reactive({ a <- ws_active(store()); if (is.null(a)) NULL else a$df })
    sheet_name <- shiny::reactive(ws_active(store())$name %||% "")
    redraw <- function() version(shiny::isolate(version()) + 1)
    update_store <- function(f, ..., redraw_table = TRUE) {
      st <- tryCatch(f(shiny::isolate(store()), ...), error = function(e) { shiny::showNotification(conditionMessage(e), type = "warning"); NULL })
      if (is.null(st)) return(invisible(FALSE))
      store(st); if (redraw_table) redraw()
      invisible(TRUE)
    }
    change <- function(df_new, redraw = TRUE) update_store(ws_change, df_new, redraw_table = redraw)
    shiny::observeEvent(loaded(), update_store(ws_load, loaded()$df, loaded()$name))
    if (!is.null(injected)) {
      shiny::observeEvent(injected(), {
        d <- injected()
        if (update_store(ws_add, d$df, d$name %||% "Created worksheet"))
          shiny::showNotification(sprintf("Opened in a new worksheet: %s.", ws_active(store())$name), type = "message")
      })
    }
    # Worksheet bar
    shiny::observe({
      st <- store()
      shiny::updateSelectInput(session, "ws", choices = ws_choices(st), selected = st$active)
    })
    shiny::observeEvent(input$ws, {
      if (!identical(input$ws, store()$active) && input$ws %in% names(store()$sheets)) update_store(ws_select, input$ws)
    })
    shiny::observeEvent(input$ws_new, shiny::showModal(shiny::modalDialog(
      title = "New worksheet",
      shiny::textInput(session$ns("new_name"), "Name", "Worksheet"),
      shiny::numericInput(session$ns("new_rows"), "Rows", 20, min = 1, max = 100000),
      shiny::numericInput(session$ns("new_cols"), "Numeric columns (C1, C2, ...)", 3, min = 1, max = 100),
      footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(session$ns("new_ok"), "Create", class = "btn-primary")))))
    shiny::observeEvent(input$new_ok, {
      r <- input$new_rows; k <- input$new_cols
      if (!isTRUE(r >= 1 && r <= 1e5 && k >= 1 && k <= 100)) return(shiny::showNotification("Use 1 to 100,000 rows and 1 to 100 columns.", type = "warning"))
      df <- as.data.frame(stats::setNames(replicate(k, rep(NA_real_, r), simplify = FALSE), paste0("C", seq_len(k))))
      if (update_store(ws_add, df, input$new_name)) shiny::removeModal()
    })
    shiny::observeEvent(input$ws_dup, update_store(ws_duplicate))
    shiny::observeEvent(input$ws_rename, shiny::showModal(shiny::modalDialog(
      title = "Rename worksheet", shiny::textInput(session$ns("rename_to"), "New name", sheet_name()),
      footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(session$ns("rename_ok"), "Rename", class = "btn-primary")))))
    shiny::observeEvent(input$rename_ok, if (update_store(ws_rename, input$rename_to, redraw_table = FALSE)) shiny::removeModal())
    shiny::observeEvent(input$ws_close, {
      a <- ws_active(store())
      if (length(store()$sheets) > 1 && !isTRUE(a$pristine) && length(a$undo))
        shiny::showModal(shiny::modalDialog(title = "Close worksheet",
          sprintf("Close '%s'? Its changes will be lost unless you exported it or saved the project.", a$name),
          footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(session$ns("close_ok"), "Close it", class = "btn-danger"))))
      else update_store(ws_close)
    })
    shiny::observeEvent(input$close_ok, { update_store(ws_close); shiny::removeModal() })

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
        dropna = col("na_cols", "Check these columns for missing values", TRUE, sel = nm),
        append = shiny::selectInput(ns("other_ws"), "Worksheet to append (columns are matched by name)", other_sheets()),
        join = shiny::tagList(
          shiny::fluidRow(shiny::column(6, shiny::selectInput(ns("other_ws"), "Other worksheet", other_sheets())),
                          shiny::column(6, shiny::selectInput(ns("join_type"), "Keep", c("All rows of this worksheet" = "left",
                                                                                          "Only rows found in both" = "inner",
                                                                                          "All rows of both" = "full")))),
          shiny::fluidRow(shiny::column(6, col("join_key", "Key column here")),
                          shiny::column(6, shiny::uiOutput(ns("join_key2_ui"))))))
    })
    other_sheets <- shiny::reactive({ st <- store(); ch <- ws_choices(st); ch[ch != st$active] })
    other_df <- function() { id <- input$other_ws; if (is.null(id) || !(id %in% names(store()$sheets))) stop("Choose another worksheet (create or open one first).")
                             store()$sheets[[id]]$df }
    output$join_key2_ui <- shiny::renderUI({
      shiny::req(input$other_ws %in% names(store()$sheets))
      nm2 <- names(store()$sheets[[input$other_ws]]$df)
      shiny::selectInput(session$ns("join_key2"), "Key column there", nm2, selected = if (isTRUE(input$join_key %in% nm2)) input$join_key else nm2[1])
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
        dropna = dt_drop_missing(df, if (length(input$na_cols)) input$na_cols else names(df)),
        append = dt_append(df, other_df()),
        join = dt_join(df, other_df(), input$join_key, input$join_key2 %||% input$join_key, input$join_type)),
        error = function(e) { shiny::showNotification(conditionMessage(e), type = "error"); NULL })
      if (is.null(res)) return()
      if (!nrow(res)) return(shiny::showNotification("That would leave no rows; nothing was changed.", type = "warning"))
      change(res)
      shiny::showNotification(sprintf("Done: %d rows x %d columns. Use Undo to reverse.", nrow(res), ncol(res)), type = "message")
    })
    shiny::observeEvent(input$undo, update_store(ws_undo))
    shiny::observeEvent(input$redo, update_store(ws_redo))
    output$history <- shiny::renderText({
      a <- ws_active(store()); if (is.null(a)) return("")
      paste0(if (length(a$undo)) sprintf("  %d change(s) can be undone", length(a$undo)) else "",
             if (length(a$redo)) sprintf("%s%d can be redone", if (length(a$undo)) ", " else "  ", length(a$redo)) else "",
             if (length(a$undo) || length(a$redo)) "." else "")
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

    list(data = shiny::reactive({ shiny::req(current()); current() }),
         get = function() ws_export(shiny::isolate(store())),
         set = function(x) { store(ws_import(x)); redraw() })
  })
}
