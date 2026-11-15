# Project & report: save/open projects (worksheet, FMEA and every screen's
# settings) and build a report from captured result pages.

# Browser-side capture of the visible page: title, the settings in the
# left-hand panel and the results (plots are already embedded images).
report_capture_js <- "
// Restore saved settings: only inputs whose binding can receive a value.
Shiny.addCustomMessageHandler('restore_inputs', function(vals) {
  Object.keys(vals).forEach(function(id) {
    var el = document.getElementById(id);
    if (!el) return;
    var b = $(el).data('shiny-input-binding');
    if (!b || typeof b.receiveMessage !== 'function') return;
    try { b.receiveMessage(el, {value: vals[id]}); } catch (err) {}
  });
});
$(document).on('click', '#add_to_report', function(e) {
  e.preventDefault();
  var pane = $('.navbar ~ .container-fluid .tab-pane.active, .tab-content > .tab-pane.active').filter(':visible').last();
  if (!pane.length) return;
  var title = pane.find('.sidebar-title').first().text().trim() || $('.nav-link.active').last().text().trim();
  var settings = [];
  pane.find('.sidebar .form-group, .sidebar .shiny-input-container').each(function() {
    var g = $(this);
    if (g.find('input[type=file]').length) return;
    var label = g.find('label').first().text().trim();
    if (!label) return;
    var val = '';
    var sel = g.find('select');
    if (sel.length) {
      var items = g.find('.selectize-input .item');
      val = items.length ? items.map(function() { return $(this).text(); }).get().join(', ')
                         : sel.find('option:selected').map(function() { return $(this).text(); }).get().join(', ');
    } else if (g.find('input[type=radio]').length) {
      val = g.find('input[type=radio]:checked').parent().text().trim();
    } else if (g.find('input[type=checkbox]').length) {
      val = g.find('input[type=checkbox]').is(':checked') ? 'Yes' : 'No';
    } else if (g.find('textarea').length) {
      val = g.find('textarea').val();
    } else if (g.find('input').length) {
      val = g.find('input').first().val();
    }
    if (val !== undefined && String(val).trim() !== '' && !g.closest('.shiny-bound-input[style*=\"display: none\"]').length &&
        g.is(':visible')) settings.push([label, String(val)]);
  });
  var main = pane.find('.bslib-sidebar-layout > .main').first().clone();
  if (!main.length) main = pane.clone();
  main.find('.no-report').remove();
  main.find('button, .btn, input, select, textarea, .selectize-control, .shiny-download-link, script').remove();
  main.find('.recalculating').removeClass('recalculating');
  main.find('img').each(function() { $(this).css({'max-width': '100%', 'height': 'auto'}); });
  Shiny.setInputValue('report_add', {title: title, settings: settings, html: main.html(), time: new Date().toLocaleString()},
                      {priority: 'event'});
});
"

projectUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    shiny::tagList(
      bslib::card(bslib::card_header("Project"),
        shiny::helpText("A project file holds all worksheets, the FMEA, the DMAIC templates and the settings of every screen."),
        shiny::textInput(ns("proj_name"), "Project name", "My project"),
        shiny::downloadButton(ns("save"), "Save project"),
        shiny::tags$hr(),
        shiny::fileInput(ns("open"), "Open project (.rds)", accept = ".rds"),
        shiny::uiOutput(ns("open_msg"))),
      bslib::card(bslib::card_header("Report settings"),
        shiny::textInput(ns("rep_title"), "Report title", "Quality analysis report"),
        shiny::textInput(ns("rep_author"), "Author", ""),
        shiny::textAreaInput(ns("rep_intro"), "Introduction / summary", rows = 5),
        shiny::downloadButton(ns("rep_html"), "Download report (HTML)"),
        shiny::uiOutput(ns("docx_ui")),
        shiny::helpText("To get a PDF, open the HTML report in a browser and print it to PDF."))
    ),
    bslib::card(bslib::card_header(shiny::textOutput(ns("items_title"), inline = TRUE)),
      shiny::helpText("Open any analysis, then click 'Add page to report' in the top bar. Each page's results and ",
                      "settings are captured as they are at that moment."),
      shiny::uiOutput(ns("items")))
  )
}

# Inputs worth saving: plain values, not buttons, files, tables or this page.
project_inputs <- function(vals) {
  skip <- "(^proj-)|(_cell_edit|_rows_|_columns_|_cells_|_search|_state|_row_last_clicked|_cell_clicked)|^report_add$|^nav$|^data-pasted$|^data-file$"
  keep <- vals[!grepl(skip, names(vals))]
  keep <- keep[vapply(keep, function(v) (is.atomic(v) && length(v) <= 200) && !inherits(v, "shinyActionButtonValue"), logical(1))]
  keep
}

projectServer <- function(id, root_input, root_session, worksheets, fmea, templates = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    items <- shiny::reactiveVal(list())

    # ---- Projects ----
    output$save <- shiny::downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9_.-]+", "_", input$proj_name %||% "project"), ".rds"),
      content = function(file) {
        vals <- shiny::isolate(shiny::reactiveValuesToList(root_input))
        saveRDS(list(format = "Six Sigma Studio project", version = 2, saved = Sys.time(), name = input$proj_name,
                     worksheets = worksheets$get(),
                     fmea = shiny::isolate(fmea$get()), templates = if (!is.null(templates)) templates$get(),
                     inputs = project_inputs(vals),
                     report = list(title = input$rep_title, author = input$rep_author, intro = input$rep_intro,
                                   items = shiny::isolate(items()))), file)
      })
    open_msg <- shiny::reactiveVal(NULL)
    output$open_msg <- shiny::renderUI(open_msg())
    shiny::observeEvent(input$open, {
      p <- tryCatch(readRDS(input$open$datapath), error = function(e) NULL)
      if (!is.list(p) || !identical(p$format, "Six Sigma Studio project")) {
        open_msg(shiny::div(class = "alert alert-danger", "That is not a Six Sigma Studio project file."))
        return()
      }
      shiny::updateTextInput(session, "proj_name", value = p$name %||% "Project")
      if (!is.null(p$report)) {
        shiny::updateTextInput(session, "rep_title", value = p$report$title %||% "")
        shiny::updateTextInput(session, "rep_author", value = p$report$author %||% "")
        shiny::updateTextAreaInput(session, "rep_intro", value = p$report$intro %||% "")
        items(p$report$items %||% list())
      }
      if (!is.null(p$fmea)) fmea$set(p$fmea)
      if (!is.null(p$templates) && !is.null(templates)) templates$set(p$templates)
      vals <- p$inputs %||% list()
      vals <- vals[!grepl("^data-", names(vals))]
      vals <- lapply(vals, function(v) if (is.numeric(v) && length(v) == 1 && is.na(v)) "" else v)
      send <- function() root_session$sendCustomMessage("restore_inputs", vals)
      # Settings are re-sent after the worksheet has loaded and again a moment
      # later, so column choices exist before they are selected.
      root_session$onFlushed(function() { send(); later::later(send, 1.5); later::later(send, 4) }, once = TRUE)
      # Version 1 projects held a single worksheet.
      ws <- p$worksheets %||% list(active = p$name, sheets = list(list(name = p$name %||% "Project", df = p$worksheet$df)))
      worksheets$set(ws)
      open_msg(shiny::div(class = "alert alert-success",
                          sprintf("Opened '%s' (saved %s): %d worksheet(s), %d report items.", p$name %||% "project",
                                  format(p$saved, "%Y-%m-%d %H:%M"), length(ws$sheets), length(items()))))
    })

    # ---- Report ----
    shiny::observeEvent(root_input$report_add, {
      it <- root_input$report_add
      settings <- if (length(it$settings)) do.call(rbind, lapply(it$settings, function(s) data.frame(Setting = s[[1]], Value = s[[2]])))
      items(c(items(), list(list(id = paste0("i", as.integer(Sys.time()) %% 1e6, sample(1000, 1)),
                                 title = it$title, time = it$time, settings = settings, html = it$html, note = ""))))
      shiny::showNotification(sprintf("Added '%s' to the report (%d items).", it$title, length(items())), type = "message")
    })
    item_button <- function(action, iid, icon, title) {
      shiny::tags$button(type = "button", class = "btn btn-default btn-sm", title = title,
        onclick = sprintf("Shiny.setInputValue('%s', {action: '%s', id: '%s'}, {priority: 'event'})",
                          session$ns("item_action"), action, iid),
        shiny::icon(icon))
    }
    output$items_title <- shiny::renderText(sprintf("Report contents (%d items)", length(items())))
    output$items <- shiny::renderUI({
      its <- items()
      if (!length(its)) return(shiny::helpText("Nothing added yet."))
      shiny::tagList(lapply(seq_along(its), function(i) {
        it <- its[[i]]
        shiny::div(class = "border rounded p-2 mb-2",
          shiny::div(class = "d-flex justify-content-between align-items-center",
            shiny::tags$b(sprintf("%d. %s", i, it$title)), shiny::tags$small(class = "text-muted", it$time),
            shiny::div(
              item_button("up", it$id, "arrow-up", "Move up"),
              item_button("rm", it$id, "trash", "Remove"))),
          shiny::textAreaInput(session$ns(paste0("note_", it$id)), NULL, it$note, rows = 2, width = "100%",
                               placeholder = "Comment or conclusion for this item (optional)"))
      }))
    })
    # One handler for every item's buttons (the buttons send their action and item id).
    shiny::observeEvent(input$item_action, {
      a <- input$item_action; cur <- items(); k <- which(vapply(cur, `[[`, "", "id") == a$id)
      if (!length(k)) return()
      cur <- notes()  # keep typed comments
      if (a$action == "rm") cur <- cur[-k]
      if (a$action == "up" && k > 1) cur[c(k - 1, k)] <- cur[c(k, k - 1)]
      items(cur)
    })
    notes <- function() lapply(items(), function(it) { it$note <- input[[paste0("note_", it$id)]] %||% it$note; it })

    build_html <- function() {
      its <- notes()
      esc <- htmltools::htmlEscape
      body <- vapply(seq_along(its), function(i) {
        it <- its[[i]]
        set <- if (!is.null(it$settings) && nrow(it$settings))
          paste0("<table class='settings'>", paste(sprintf("<tr><th>%s</th><td>%s</td></tr>", esc(it$settings$Setting), esc(it$settings$Value)),
                                                   collapse = ""), "</table>") else ""
        note <- if (nzchar(it$note %||% "")) sprintf("<p class='note'>%s</p>", gsub("\n", "<br>", esc(it$note))) else ""
        sprintf("<section><h2>%d. %s</h2>%s%s<div class='content'>%s</div></section>", i, esc(it$title), note, set, it$html)
      }, "")
      sprintf('<!doctype html><html><head><meta charset="utf-8"><title>%s</title><style>
body{font-family:Segoe UI,Helvetica,Arial,sans-serif;max-width:1000px;margin:24px auto;padding:0 16px;color:#222}
h1{border-bottom:3px solid #18bc9c;padding-bottom:6px} h2{margin-top:36px;border-bottom:1px solid #ccc;padding-bottom:4px}
table{border-collapse:collapse;margin:8px 0;font-size:13px} th,td{border:1px solid #ddd;padding:3px 8px;text-align:right}
th{background:#f3f5f7} table.settings th,table.settings td{text-align:left} table.settings th{width:40%%}
.card{border:none;margin:10px 0} .card-header{font-weight:600;margin:12px 0 4px} .note{background:#fff8e1;padding:8px;border-left:4px solid #f0ad4e}
.alert{padding:8px;border-left:4px solid #3498db;background:#eef6fc} img{max-width:100%%;height:auto}
section{page-break-inside:avoid} @media print{section{page-break-before:auto}}
</style></head><body><h1>%s</h1><p>%s%s</p>%s%s<p style="color:#888;font-size:12px;margin-top:40px">Generated by Six Sigma Studio on %s.</p></body></html>',
        esc(input$rep_title), esc(input$rep_title), if (nzchar(input$rep_author)) paste0("Author: ", esc(input$rep_author), "<br>") else "",
        format(Sys.Date(), "%d %B %Y"),
        if (nzchar(input$rep_intro)) sprintf("<p>%s</p>", gsub("\n", "<br>", esc(input$rep_intro))) else "",
        paste(body, collapse = "\n"), format(Sys.time(), "%Y-%m-%d %H:%M"))
    }
    output$rep_html <- shiny::downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9_.-]+", "_", input$rep_title), ".html"),
      content = function(file) writeLines(build_html(), file, useBytes = TRUE))
    has_pandoc <- requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available()
    output$docx_ui <- shiny::renderUI({
      if (has_pandoc) shiny::downloadButton(session$ns("rep_docx"), "Download report (Word)")
      else shiny::helpText("Word export needs pandoc (included with RStudio).")
    })
    output$rep_docx <- shiny::downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9_.-]+", "_", input$rep_title), ".docx"),
      content = function(file) {
        tmp <- tempfile(fileext = ".html"); writeLines(build_html(), tmp, useBytes = TRUE)
        out <- tempfile(fileext = ".docx")
        rmarkdown::pandoc_convert(tmp, to = "docx", output = out, options = "--from=html")
        file.copy(out, file, overwrite = TRUE)
      })
  })
}
