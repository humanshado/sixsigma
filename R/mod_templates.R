# DMAIC menu: project charter, SIPOC, 5 Whys, control plan, value stream map
# and A3 report. Editing areas carry the class "no-report" so that "Add page
# to report" captures only the formatted preview.

# ---- Generic editable table ------------------------------------------------------------
editTableUI <- function(id, add_label = "Add row") {
  ns <- shiny::NS(id)
  shiny::tagList(
    DT::DTOutput(ns("table")),
    shiny::div(class = "mt-2 d-flex gap-2",
      shiny::actionButton(ns("add"), add_label, icon = shiny::icon("plus"), class = "btn-sm"),
      shiny::actionButton(ns("del"), "Delete selected rows", icon = shiny::icon("trash"), class = "btn-sm"),
      shiny::downloadButton(ns("csv"), "CSV", class = "btn-sm")))
}

# initial: data frame; numeric columns are validated on entry.
editTableServer <- function(id, initial, file_stub = "table") {
  shiny::moduleServer(id, function(input, output, session) {
    tab <- shiny::reactiveVal(initial)
    version <- shiny::reactiveVal(0)
    set_tab <- function(d) { tab(d); version(shiny::isolate(version()) + 1) }
    output$table <- DT::renderDT({
      version()
      DT::datatable(shiny::isolate(tab()), rownames = TRUE, selection = "multiple",
                    editable = list(target = "cell", disable = list(columns = 0)),
                    options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE))
    })
    shiny::observeEvent(input$table_cell_edit, {
      info <- input$table_cell_edit; d <- tab()
      i <- info$row; j <- info$col
      shiny::req(j >= 1, j <= ncol(d), i >= 1, i <= nrow(d))
      val <- trimws(as.character(info$value))
      if (is.numeric(d[[j]])) {
        v <- if (nzchar(val)) suppressWarnings(as.numeric(val)) else NA_real_
        if (nzchar(val) && is.na(v)) {
          shiny::showNotification(sprintf("'%s' needs a number.", names(d)[j]), type = "error")
          return(DT::replaceData(DT::dataTableProxy("table"), d, resetPaging = FALSE, rownames = TRUE))
        }
        d[[j]][i] <- v
      } else d[[j]][i] <- val
      tab(d)
    })
    shiny::observeEvent(input$add, {
      d <- tab(); blank <- d[NA_integer_, , drop = FALSE]
      for (n in names(d)) if (!is.numeric(d[[n]])) blank[[n]] <- ""
      d <- rbind(if (nrow(d)) d, blank); rownames(d) <- NULL
      set_tab(d)
    })
    shiny::observeEvent(input$del, {
      s <- input$table_rows_selected
      if (!length(s)) return(shiny::showNotification("Click rows in the table to select them first.", type = "warning"))
      set_tab(tab()[-s, , drop = FALSE])
    })
    output$csv <- shiny::downloadHandler(filename = function() paste0(file_stub, ".csv"),
                                         content = function(f) utils::write.csv(tab(), f, row.names = FALSE, na = ""))
    list(get = shiny::reactive(tab()), set = function(d) set_tab(as.data.frame(d, stringsAsFactors = FALSE, check.names = FALSE)))
  })
}

edit_card <- function(title, ...) bslib::card(class = "no-report", bslib::card_header(shiny::icon("pen-to-square"), title), ...)
html_table <- function(d) {
  if (is.null(d) || !nrow(d)) return(shiny::helpText("(empty)"))
  d[] <- lapply(d, function(v) ifelse(is.na(v), "", as.character(v)))
  shiny::tags$table(class = "table table-sm table-bordered",
    shiny::tags$thead(shiny::tags$tr(lapply(names(d), shiny::tags$th))),
    shiny::tags$tbody(lapply(seq_len(nrow(d)), function(i) shiny::tags$tr(lapply(d[i, ], shiny::tags$td)))))
}
para <- function(x) if (nzchar(trimws(x %||% ""))) lapply(strsplit(x, "\n", fixed = TRUE)[[1]], shiny::tags$p) else shiny::tags$p(class = "text-muted", "-")
nonblank_rows <- function(d) d[apply(d, 1, function(r) any(!is.na(r) & nzchar(trimws(as.character(r))))), , drop = FALSE]
tpl_sidebar <- function(ns, prefix, title, ...) shiny::tagList(
  shiny::actionButton(ns(paste0(prefix, "_example")), "Load example", icon = shiny::icon("file-lines")),
  shiny::actionButton(ns(paste0(prefix, "_clear")), "Clear", icon = shiny::icon("eraser")),
  ...,
  shiny::helpText("Edit in the cards marked with a pencil; the preview below them is what 'Add page to report' captures. ",
                  "Everything is saved with the project."))

# ---- UIs ----------------------------------------------------------------------------
charterUI <- function(id) {
  ns <- shiny::NS(id)
  ta <- function(n, label, rows = 2) shiny::textAreaInput(ns(paste0("ch_", n)), label, "", rows = rows, width = "100%")
  analysis_layout("Project charter", tpl_sidebar(ns, "ch", "Project charter"), shiny::tagList(
    edit_card("Charter",
      bslib::layout_columns(col_widths = c(6, 3, 3),
        shiny::textInput(ns("ch_title"), "Project title", "", width = "100%"),
        shiny::textInput(ns("ch_leader"), "Project leader", "", width = "100%"),
        shiny::textInput(ns("ch_sponsor"), "Sponsor / champion", "", width = "100%")),
      bslib::layout_columns(col_widths = c(6, 6), ta("business", "Business case"), ta("problem", "Problem statement")),
      bslib::layout_columns(col_widths = c(6, 6), ta("goal", "Goal statement (specific, measurable, time-bound)"),
                            ta("benefits", "Expected benefits")),
      bslib::layout_columns(col_widths = c(6, 6), ta("scope_in", "In scope"), ta("scope_out", "Out of scope")),
      shiny::textInput(ns("ch_start"), "Start date (YYYY-MM-DD)", "")),
    bslib::layout_columns(col_widths = c(6, 6),
      edit_card("Metrics (CTQs)", editTableUI(ns("ch_metrics"))),
      edit_card("Team", editTableUI(ns("ch_team")))),
    edit_card("Milestones (dates as YYYY-MM-DD)", editTableUI(ns("ch_ms"))),
    bslib::card(bslib::card_header("Project charter"), shiny::uiOutput(ns("ch_preview"))),
    bslib::card(shiny::plotOutput(ns("ch_timeline"), height = 300))))
}

sipocUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout("SIPOC", tpl_sidebar(ns, "sp", "SIPOC",
      shiny::textInput(ns("sp_title"), "Process name", ""),
      shiny::textInput(ns("sp_start"), "Process starts when", ""),
      shiny::textInput(ns("sp_end"), "Process ends when", ""),
      shiny::downloadButton(ns("sp_png"), "Download diagram")),
    shiny::tagList(
      edit_card("SIPOC table (one item per cell; list process steps in order)", editTableUI(ns("sp_tab"))),
      bslib::card(bslib::card_header(shiny::textOutput(ns("sp_head"), inline = TRUE)), shiny::plotOutput(ns("sp_plot"), height = 520))))
}

whysUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout("5 Whys", tpl_sidebar(ns, "wy", "5 Whys", shiny::downloadButton(ns("wy_png"), "Download diagram")),
    shiny::tagList(
      edit_card("Ask 'why?' until you reach a cause you can act on",
        shiny::textAreaInput(ns("wy_problem"), "Problem", "", rows = 2, width = "100%"),
        lapply(1:6, function(i) shiny::textInput(ns(paste0("wy_w", i)), sprintf("Why %d?", i), "", width = "100%")),
        shiny::textAreaInput(ns("wy_root"), "Root cause", "", rows = 2, width = "100%"),
        shiny::textAreaInput(ns("wy_action"), "Countermeasure", "", rows = 2, width = "100%")),
      bslib::card(shiny::plotOutput(ns("wy_plot"), height = 620), shiny::uiOutput(ns("wy_action_out")))))
}

controlPlanUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout("Control plan", tpl_sidebar(ns, "cp", "Control plan",
      shiny::textInput(ns("cp_part"), "Part / process", ""),
      shiny::textInput(ns("cp_number"), "Control plan number", ""),
      shiny::selectInput(ns("cp_phase"), "Phase", c("Production", "Pre-launch", "Prototype")),
      shiny::textInput(ns("cp_rev"), "Revision date", ""),
      shiny::textInput(ns("cp_team"), "Core team", "")),
    shiny::tagList(
      edit_card("Control plan", editTableUI(ns("cp_tab"), "Add characteristic")),
      bslib::card(bslib::card_header("Control plan"), shiny::uiOutput(ns("cp_preview"))),
      bslib::card(bslib::card_header("Completeness check"), shiny::uiOutput(ns("cp_check")))))
}

vsmUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout("Value stream map", tpl_sidebar(ns, "vs", "Value stream map",
      shiny::textInput(ns("vs_supplier"), "Supplier", "Supplier"),
      shiny::textInput(ns("vs_customer"), "Customer", "Customer"),
      shiny::numericInput(ns("vs_demand"), "Customer demand (units per day)", 460, min = 1),
      shiny::numericInput(ns("vs_shifts"), "Shifts per day", 1, min = 1, max = 4),
      shiny::numericInput(ns("vs_shift_min"), "Working minutes per shift (excluding breaks)", 460, min = 1),
      shiny::downloadButton(ns("vs_png"), "Download map")),
    shiny::tagList(
      edit_card("Process steps (in flow order; inventory is the queue in front of each step)", editTableUI(ns("vs_tab"), "Add step")),
      bslib::card(shiny::plotOutput(ns("vs_plot"), height = 470)),
      bslib::layout_columns(col_widths = c(4, 8),
        bslib::card(bslib::card_header("Summary"), shiny::tableOutput(ns("vs_summary")),
                    shiny::helpText("Lead time = inventory days (inventory ÷ daily demand) plus value-added time. ",
                                    "Process cycle efficiency = value-added time ÷ lead time in working seconds.")),
        bslib::card(shiny::plotOutput(ns("vs_balance"), height = 330))),
      bslib::card(bslib::card_header("Steps vs takt time"), shiny::tableOutput(ns("vs_table")))))
}

a3UI <- function(id) {
  ns <- shiny::NS(id)
  ta <- function(n, label, rows = 4) shiny::textAreaInput(ns(paste0("a3_", n)), label, "", rows = rows, width = "100%")
  analysis_layout("A3 report", tpl_sidebar(ns, "a3", "A3 report",
      shiny::actionButton(ns("a3_fill"), "Fill from charter and 5 Whys", icon = shiny::icon("wand-magic-sparkles"))),
    shiny::tagList(
      edit_card("A3",
        bslib::layout_columns(col_widths = c(6, 3, 3), shiny::textInput(ns("a3_title"), "Title", "", width = "100%"),
                              shiny::textInput(ns("a3_owner"), "Owner", "", width = "100%"),
                              shiny::textInput(ns("a3_date"), "Date", "", width = "100%")),
        bslib::layout_columns(col_widths = c(6, 6),
          shiny::tagList(ta("background", "1. Background"), ta("current", "2. Current condition"),
                         ta("goal", "3. Goal / target condition"), ta("analysis", "4. Root cause analysis")),
          shiny::tagList(ta("counter", "5. Countermeasures"), shiny::tags$label("6. Implementation plan"),
                         editTableUI(ns("a3_plan"), "Add action"), ta("followup", "7. Follow-up and results")))),
      bslib::card(bslib::card_header(shiny::textOutput(ns("a3_head"), inline = TRUE)), shiny::uiOutput(ns("a3_preview")))))
}

# ---- Server (one module for all templates, so they can share content) ----------------------
dmaicServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    tables <- list(
      ch_metrics = editTableServer("ch_metrics", tpl_blank(c("Metric", "Baseline", "Target"), 2, c("Baseline", "Target")), "charter-metrics"),
      ch_team = editTableServer("ch_team", tpl_blank(c("Role", "Name", "Responsibility"), 2), "charter-team"),
      ch_ms = editTableServer("ch_ms", data.frame(Phase = c("Define", "Measure", "Analyze", "Improve", "Control"),
                                                  Planned = "", Actual = "", stringsAsFactors = FALSE), "charter-milestones"),
      sp_tab = editTableServer("sp_tab", tpl_blank(sipoc_cols, 4), "sipoc"),
      cp_tab = editTableServer("cp_tab", tpl_blank(control_plan_cols, 2), "control-plan"),
      vs_tab = editTableServer("vs_tab", vsm_example()[0, ], "value-stream"),
      a3_plan = editTableServer("a3_plan", tpl_blank(c("Action", "Owner", "Due", "Status"), 2), "a3-plan"))
    tables$vs_tab$set(tpl_blank(names(vsm_example()), 3, names(vsm_example())[-1]))
    upd <- function(id, v) {
      if (grepl("^(ch_(business|problem|goal|benefits|scope_in|scope_out)|wy_(problem|root|action)|a3_(background|current|goal|analysis|counter|followup))$", id))
        shiny::updateTextAreaInput(session, id, value = v) else shiny::updateTextInput(session, id, value = v)
    }
    clear_inputs <- function(prefix) for (n in grep(paste0("^", prefix, "_"), names(input), value = TRUE))
      if (is.character(input[[n]]) && !grepl("_(tab|plan|metrics|team|ms|phase)", n)) upd(n, "")

    # Charter
    shiny::observeEvent(input$ch_example, {
      for (n in names(charter_text_example)) upd(paste0("ch_", n), charter_text_example[[n]])
      upd("ch_start", format(Sys.Date() - 80))
      tables$ch_team$set(charter_team_example()); tables$ch_metrics$set(charter_metrics_example())
      tables$ch_ms$set(charter_milestones_example())
    })
    shiny::observeEvent(input$ch_clear, {
      clear_inputs("ch"); tables$ch_team$set(tpl_blank(c("Role", "Name", "Responsibility"), 2))
      tables$ch_metrics$set(tpl_blank(c("Metric", "Baseline", "Target"), 2, c("Baseline", "Target")))
      tables$ch_ms$set(data.frame(Phase = c("Define", "Measure", "Analyze", "Improve", "Control"), Planned = "", Actual = ""))
    })
    output$ch_preview <- shiny::renderUI({
      sec <- function(h, x) shiny::tagList(shiny::tags$h6(class = "mt-3 fw-bold", h), para(x))
      shiny::tagList(
        shiny::tags$h4(if (nzchar(input$ch_title %||% "")) input$ch_title else "Untitled project"),
        shiny::tags$p(shiny::tags$b("Leader: "), input$ch_leader %||% "", shiny::HTML(" &nbsp; "), shiny::tags$b("Sponsor: "),
                      input$ch_sponsor %||% "", shiny::HTML(" &nbsp; "), shiny::tags$b("Start: "), input$ch_start %||% ""),
        bslib::layout_columns(col_widths = c(6, 6),
          shiny::tagList(sec("Business case", input$ch_business), sec("Problem statement", input$ch_problem),
                         sec("Goal statement", input$ch_goal)),
          shiny::tagList(sec("In scope", input$ch_scope_in), sec("Out of scope", input$ch_scope_out),
                         sec("Expected benefits", input$ch_benefits))),
        bslib::layout_columns(col_widths = c(6, 6),
          shiny::tagList(shiny::tags$h6(class = "fw-bold", "Metrics"), html_table(nonblank_rows(tables$ch_metrics$get()))),
          shiny::tagList(shiny::tags$h6(class = "fw-bold", "Team"), html_table(nonblank_rows(tables$ch_team$get())))),
        shiny::tags$h6(class = "fw-bold", "Milestones"), html_table(tables$ch_ms$get()))
    })
    output$ch_timeline <- shiny::renderPlot(validate_run(charter_timeline_plot(tables$ch_ms$get(), input$ch_start)))

    # SIPOC
    shiny::observeEvent(input$sp_example, {
      tables$sp_tab$set(sipoc_example()); upd("sp_title", "Shaft machining (Line 3)")
      upd("sp_start", "Bar stock is released to the line"); upd("sp_end", "Packed shafts reach the assembly line")
    })
    shiny::observeEvent(input$sp_clear, { clear_inputs("sp"); tables$sp_tab$set(tpl_blank(sipoc_cols, 4)) })
    sp_plot <- shiny::reactive(validate_run(sipoc_plot(tables$sp_tab$get())))
    output$sp_head <- shiny::renderText(paste0("SIPOC", if (nzchar(input$sp_title %||% "")) paste(":", input$sp_title),
      if (nzchar(input$sp_start %||% "") || nzchar(input$sp_end %||% "")) sprintf("  (starts: %s; ends: %s)", input$sp_start, input$sp_end)))
    output$sp_plot <- shiny::renderPlot(sp_plot())
    output$sp_png <- plot_download("sipoc", function() sp_plot(), width = 11, height = 6)

    # 5 Whys
    whys <- shiny::reactive(vapply(1:6, function(i) input[[paste0("wy_w", i)]] %||% "", ""))
    shiny::observeEvent(input$wy_example, {
      upd("wy_problem", whys_example$problem); for (i in 1:6) upd(paste0("wy_w", i), whys_example$whys[i] %||% "")
      upd("wy_root", whys_example$root); upd("wy_action", whys_example$action)
    })
    shiny::observeEvent(input$wy_clear, clear_inputs("wy"))
    wy_plot <- shiny::reactive(validate_run(whys_plot(input$wy_problem %||% "", whys(), input$wy_root %||% "")))
    output$wy_plot <- shiny::renderPlot(wy_plot())
    output$wy_action_out <- shiny::renderUI(if (nzchar(trimws(input$wy_action %||% "")))
      shiny::div(class = "alert alert-success", shiny::tags$b("Countermeasure: "), input$wy_action))
    output$wy_png <- plot_download("five-whys", function() wy_plot(), width = 9, height = 8)

    # Control plan
    shiny::observeEvent(input$cp_example, {
      tables$cp_tab$set(control_plan_example()); upd("cp_part", "Shaft 10 mm, Line 3"); upd("cp_number", "CP-0310")
      upd("cp_rev", format(Sys.Date())); upd("cp_team", "Quality engineer, setter, Line 3 supervisor")
    })
    shiny::observeEvent(input$cp_clear, { clear_inputs("cp"); tables$cp_tab$set(tpl_blank(control_plan_cols, 2)) })
    output$cp_preview <- shiny::renderUI(shiny::tagList(
      shiny::tags$p(shiny::tags$b("Part / process: "), input$cp_part, shiny::HTML(" &nbsp; "), shiny::tags$b("Number: "), input$cp_number,
                    shiny::HTML(" &nbsp; "), shiny::tags$b("Phase: "), input$cp_phase, shiny::HTML(" &nbsp; "),
                    shiny::tags$b("Revised: "), input$cp_rev, shiny::tags$br(), shiny::tags$b("Core team: "), input$cp_team),
      html_table(nonblank_rows(tables$cp_tab$get()))))
    output$cp_check <- shiny::renderUI({
      chk <- control_plan_check(tables$cp_tab$get())
      if (!nrow(chk)) shiny::div(class = "alert alert-success", "Every characteristic has a specification, method, frequency, control, reaction plan and owner.")
      else shiny::tagList(shiny::div(class = "alert alert-warning", sprintf("%d row(s) are incomplete:", nrow(chk))), html_table(chk))
    })

    # Value stream map
    avail <- shiny::reactive(60 * (input$vs_shift_min %||% NA) * (input$vs_shifts %||% NA))
    shiny::observeEvent(input$vs_example, {
      tables$vs_tab$set(vsm_example()); upd("vs_supplier", "Steel mill"); upd("vs_customer", "Assembly")
      shiny::updateNumericInput(session, "vs_demand", value = 460); shiny::updateNumericInput(session, "vs_shifts", value = 1)
      shiny::updateNumericInput(session, "vs_shift_min", value = 460)
    })
    shiny::observeEvent(input$vs_clear, tables$vs_tab$set(tpl_blank(names(vsm_example()), 3, names(vsm_example())[-1])))
    vsm <- shiny::reactive(validate_run(vsm_metrics(tables$vs_tab$get(), input$vs_demand, avail())))
    vs_plot <- shiny::reactive(vsm_plot(vsm(), input$vs_supplier, input$vs_customer, input$vs_demand))
    output$vs_plot <- shiny::renderPlot(vs_plot())
    output$vs_summary <- shiny::renderTable(vsm()$summary)
    output$vs_balance <- shiny::renderPlot(vsm_balance_plot(vsm()))
    output$vs_table <- shiny::renderTable(vsm()$table, digits = 2)
    output$vs_png <- plot_download("value-stream-map", function() vs_plot(), width = 13, height = 6)

    # A3
    shiny::observeEvent(input$a3_example, {
      upd("a3_title", charter_text_example$title); upd("a3_owner", "A. Rahman"); upd("a3_date", format(Sys.Date()))
      upd("a3_background", charter_text_example$business); upd("a3_current", charter_text_example$problem)
      upd("a3_goal", charter_text_example$goal)
      upd("a3_analysis", paste(c(sprintf("Why %d: %s", 1:5, whys_example$whys), paste("Root cause:", whys_example$root)), collapse = "\n"))
      upd("a3_counter", whys_example$action); upd("a3_followup", "Rework fell to 0.8% in May; diameter Ppk 1.41. Review again in September.")
      tables$a3_plan$set(a3_plan_example())
    })
    shiny::observeEvent(input$a3_clear, { clear_inputs("a3"); tables$a3_plan$set(tpl_blank(c("Action", "Owner", "Due", "Status"), 2)) })
    shiny::observeEvent(input$a3_fill, {
      if (nzchar(input$ch_title %||% "")) upd("a3_title", input$ch_title)
      if (nzchar(input$ch_leader %||% "")) upd("a3_owner", input$ch_leader)
      if (nzchar(input$ch_business %||% "")) upd("a3_background", input$ch_business)
      if (nzchar(input$ch_problem %||% "")) upd("a3_current", input$ch_problem)
      if (nzchar(input$ch_goal %||% "")) upd("a3_goal", input$ch_goal)
      w <- trimws(whys()); w <- w[nzchar(w)]
      if (length(w)) upd("a3_analysis", paste(c(sprintf("Why %d: %s", seq_along(w), w),
                                                if (nzchar(input$wy_root %||% "")) paste("Root cause:", input$wy_root)), collapse = "\n"))
      if (nzchar(input$wy_action %||% "")) upd("a3_counter", input$wy_action)
      if (!nzchar(input$a3_date %||% "")) upd("a3_date", format(Sys.Date()))
      shiny::showNotification("Filled the A3 from the project charter and 5 Whys (blank fields there were skipped).", type = "message")
    })
    output$a3_head <- shiny::renderText(sprintf("A3: %s", if (nzchar(input$a3_title %||% "")) input$a3_title else "untitled"))
    output$a3_preview <- shiny::renderUI({
      box <- function(h, ...) shiny::div(class = "border rounded p-2 mb-2", shiny::tags$h6(class = "fw-bold text-uppercase", h), ...)
      shiny::tagList(
        shiny::tags$p(shiny::tags$b("Owner: "), input$a3_owner, shiny::HTML(" &nbsp; "), shiny::tags$b("Date: "), input$a3_date),
        bslib::layout_columns(col_widths = c(6, 6),
          shiny::tagList(box("1. Background", para(input$a3_background)), box("2. Current condition", para(input$a3_current)),
                         box("3. Goal", para(input$a3_goal)), box("4. Root cause analysis", para(input$a3_analysis))),
          shiny::tagList(box("5. Countermeasures", para(input$a3_counter)),
                         box("6. Implementation plan", html_table(nonblank_rows(tables$a3_plan$get()))),
                         box("7. Follow-up", para(input$a3_followup)))))
    })

    list(get = function() lapply(tables, function(t) shiny::isolate(t$get())),
         set = function(x) for (n in intersect(names(x), names(tables))) tables[[n]]$set(x[[n]]))
  })
}
