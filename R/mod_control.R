# Quality Tools > Control Charts.

control_types <- list(
  "Variables" = c("I-MR (individuals)" = "imr", "Xbar-R (subgroups)" = "xbar_r", "Xbar-S (subgroups)" = "xbar_s",
                  "I-MR-R (between/within, batches)" = "imr_r", "I-MR-S (between/within, batches)" = "imr_s",
                  "Z-MR (short runs, several parts)" = "zmr"),
  "Attributes" = c("P (proportion defective)" = "p", "NP (number defective)" = "np", "C (defects per sample)" = "c",
                   "U (defects per unit)" = "u", "Laney P' (large samples)" = "laney_p", "Laney U' (large samples)" = "laney_u"),
  "Rare events" = c("G (opportunities between events)" = "g", "T (time between events)" = "t"),
  "Time-weighted" = c("EWMA (small shifts)" = "ewma", "CUSUM (small shifts)" = "cusum")
)
subgroup_types <- c("xbar_r", "xbar_s", "imr_r", "imr_s")
size_types <- c("p", "np", "u", "laney_p", "laney_u")
ruled_types <- c("imr", "xbar_r", "xbar_s", "imr_r", "imr_s", "zmr", "p", "np", "c", "u", "laney_p", "laney_u")
hist_types <- c("imr", "xbar_r", "xbar_s", "p", "np", "c", "u")

controlUI <- function(id) {
  ns <- shiny::NS(id)
  js_in <- function(v) sprintf("[%s].indexOf(input.type) >= 0", paste0("'", v, "'", collapse = ","))
  analysis_layout(
    "Control charts",
    shiny::tagList(
      shiny::selectInput(ns("type"), "Chart type", control_types),
      column_picker(ns("var"), "Measurement / count column"),
      shiny::conditionalPanel(
        "['ewma','cusum'].indexOf(input.type) >= 0", ns = ns,
        shiny::radioButtons(ns("tw_data"), "Plot", c("Individual values" = "ind", "Subgroup means" = "sg")),
        shiny::conditionalPanel("input.type == 'ewma'", ns = ns,
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput(ns("lambda"), "Lambda (λ)", 0.2, min = 0.01, max = 1, step = 0.05)),
            shiny::column(6, shiny::numericInput(ns("L"), "Limit width L", 3, min = 1, max = 4, step = 0.1)))),
        shiny::conditionalPanel("input.type == 'cusum'", ns = ns,
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput(ns("cusum_k"), "Allowance k", 0.5, min = 0, step = 0.1)),
            shiny::column(6, shiny::numericInput(ns("cusum_h"), "Decision interval h", 5, min = 0.5, step = 0.5)))),
        blank_numeric(ns("mu0"), "Target mean (blank = estimate from data)"),
        blank_numeric(ns("sigma0"), "Process sigma (blank = estimate from data)"),
        shiny::helpText("EWMA and CUSUM detect small, sustained shifts in the mean faster than Shewhart charts. ",
                        "Common choices: λ = 0.05-0.25 with L ≈ 2.7-3; k = 0.5 and h = 4-5.")
      ),
      shiny::conditionalPanel(
        sprintf("%s || (['ewma','cusum'].indexOf(input.type) >= 0 && input.tw_data == 'sg')", js_in(subgroup_types)), ns = ns,
        shiny::radioButtons(ns("sg_mode"), "Subgroups", c("Fixed size" = "size", "Group column" = "col")),
        shiny::conditionalPanel("input.sg_mode == 'size'", ns = ns,
                                shiny::numericInput(ns("sg_size"), "Subgroup size", 5, min = 2, max = 50)),
        shiny::conditionalPanel("input.sg_mode == 'col'", ns = ns,
                                column_picker(ns("sg_col"), "Subgroup column"))
      ),
      shiny::conditionalPanel(js_in(size_types), ns = ns,
        shiny::radioButtons(ns("n_mode"), "Sample size", c("Constant" = "const", "Column" = "col")),
        shiny::conditionalPanel("input.n_mode == 'const'", ns = ns,
                                shiny::numericInput(ns("n_const"), "Sample size", 50, min = 1)),
        shiny::conditionalPanel("input.n_mode == 'col'", ns = ns,
                                column_picker(ns("n_col"), "Sample size column"))
      ),
      shiny::conditionalPanel("input.type == 'zmr'", ns = ns,
        column_picker(ns("part_col"), "Part (product) column"),
        shiny::radioButtons(ns("zmr_sigma"), "Sigma", c("Estimate for each part" = "by_part", "Pooled across parts" = "pooled"))),
      shiny::conditionalPanel("input.type == 'g' || input.type == 't'", ns = ns,
        shiny::helpText("Enter the count (G) or time (T) between successive rare events, e.g. days between injuries ",
                        "or units produced between failures. Limits use 0.135% and 99.865% probabilities.")),
      shiny::conditionalPanel(js_in(hist_types), ns = ns,
        shiny::radioButtons(ns("limits_from"), "Control limits from", c("The data" = "data", "Historical values" = "hist"), inline = TRUE),
        shiny::conditionalPanel("input.limits_from == 'hist'", ns = ns,
          shiny::conditionalPanel(js_in(c("imr", "xbar_r", "xbar_s")), ns = ns,
            shiny::fluidRow(shiny::column(6, blank_numeric(ns("h_mu"), "Mean")), shiny::column(6, blank_numeric(ns("h_sigma"), "Sigma")))),
          shiny::conditionalPanel("input.type == 'p' || input.type == 'np'", ns = ns, blank_numeric(ns("h_p"), "Proportion defective p")),
          shiny::conditionalPanel("input.type == 'c'", ns = ns, blank_numeric(ns("h_c"), "Mean defects per sample")),
          shiny::conditionalPanel("input.type == 'u'", ns = ns, blank_numeric(ns("h_u"), "Mean defects per unit")),
          shiny::helpText("Use limits from an earlier stable period to judge new data (Phase II). Blank values are estimated."))),
      shiny::conditionalPanel("['ewma','cusum','g','t'].indexOf(input.type) < 0", ns = ns,
        shiny::numericInput(ns("k"), "Sigma multiple for limits", 3, min = 1, max = 6, step = 0.5)),
      blank_numeric(ns("n_est"), "Estimate limits from first N points (blank = all)", min = 2),
      column_picker(ns("stage"), "Stage column (optional: separate limits per stage)"),
      shiny::conditionalPanel(js_in(ruled_types), ns = ns,
        shiny::checkboxGroupInput(ns("rules"), "Tests for special causes", stats::setNames(1:8, paste0(1:8, ": ", rule_text)),
                                  selected = 1:3)),
      shiny::downloadButton(ns("download"), "Download chart"),
      shiny::helpText("Red points fail a selected test. EWMA, CUSUM, G and T charts use the limits only.")
    ),
    shiny::tagList(
      bslib::card(shiny::plotOutput(ns("chart"), height = 560)),
      bslib::layout_columns(
        col_widths = c(5, 7),
        bslib::card(bslib::card_header("Limits"), shiny::tableOutput(ns("limits")), shiny::tableOutput(ns("extra"))),
        bslib::card(bslib::card_header("Out-of-control signals"), shiny::tableOutput(ns("signals")))
      )
    )
  )
}

controlServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), update_pickers(session, data(), list(
      var = list(type = "numeric", default = 1),
      sg_col = list(type = "any", default = 1),
      n_col = list(type = "numeric", default = 1),
      part_col = list(type = "any", default = 1),
      stage = list(type = "any", optional = TRUE, default = "")
    )))

    result <- shiny::reactive({
      df <- data()
      shiny::req(input$var %in% names(df))
      k <- input$k
      shiny::validate(shiny::need(input$type %in% c("ewma", "cusum", "g", "t") || isTRUE(k > 0), "Sigma multiple must be positive."))
      n_est <- num_or_na(input$n_est)
      hist <- if (identical(input$limits_from, "hist") && input$type %in% hist_types)
        list(mu = num_or_na(input$h_mu), sigma = num_or_na(input$h_sigma), p = num_or_na(input$h_p),
             c = num_or_na(input$h_c), u = num_or_na(input$h_u)) else list()
      if (!is.null(hist$sigma) && !is.na(hist$sigma)) shiny::validate(shiny::need(hist$sigma > 0, "Historical sigma must be positive."))
      if (!is.null(hist$p) && !is.na(hist$p)) shiny::validate(shiny::need(hist$p > 0 && hist$p < 1, "Historical p must be between 0 and 1."))

      # Build the chart from a subset of rows (all rows, or one stage).
      make <- function(rows) {
        d <- df[rows, , drop = FALSE]
        x <- d[[input$var]]
        sizes <- function() {
          if (input$n_mode == "const") {
            shiny::validate(shiny::need(isTRUE(input$n_const >= 1), "Enter a sample size."))
            rep(input$n_const, length(x))
          } else { shiny::req(input$n_col %in% names(d)); d[[input$n_col]] }
        }
        counts_ok <- function(cnt, n = NULL) {
          keep <- !is.na(cnt) & (if (is.null(n)) TRUE else !is.na(n))
          list(d = cnt[keep], n = if (is.null(n)) NULL else n[keep])
        }
        subgroups <- function() {
          if (input$sg_mode == "size") spc_subgroups(x, size = input$sg_size)
          else { shiny::req(input$sg_col %in% names(d)); spc_subgroups(x, groups = d[[input$sg_col]]) }
        }
        switch(input$type,
          imr = spc_imr(x, k, n_est, hist),
          xbar_r = spc_xbar_r(subgroups(), k, n_est, hist),
          xbar_s = spc_xbar_s(subgroups(), k, n_est, hist),
          imr_r = spc_imr_rs(subgroups(), "R", k, n_est),
          imr_s = spc_imr_rs(subgroups(), "S", k, n_est),
          zmr = { shiny::req(input$part_col %in% names(d)); spc_zmr(x, d[[input$part_col]], input$zmr_sigma, k) },
          p = { v <- counts_ok(x, sizes()); spc_p(v$d, v$n, k, n_est, hist) },
          np = { v <- counts_ok(x, sizes()); spc_np(v$d, v$n, k, n_est, hist) },
          c = { v <- counts_ok(x); spc_c(v$d, k, n_est, hist) },
          u = { v <- counts_ok(x, sizes()); spc_u(v$d, v$n, k, n_est, hist) },
          laney_p = { v <- counts_ok(x, sizes()); spc_laney(v$d, v$n, "p", k, n_est) },
          laney_u = { v <- counts_ok(x, sizes()); spc_laney(v$d, v$n, "u", k, n_est) },
          g = spc_g(x, n_est),
          t = spc_t(x, n_est),
          ewma = spc_ewma(x, if (input$tw_data == "sg") subgroups(), lambda = input$lambda, L = input$L,
                          mu0 = num_or_na(input$mu0), sigma = num_or_na(input$sigma0), n_est = n_est),
          cusum = spc_cusum(x, if (input$tw_data == "sg") subgroups(), k = input$cusum_k, h = input$cusum_h,
                            mu0 = num_or_na(input$mu0), sigma = num_or_na(input$sigma0), n_est = n_est))
      }
      st <- empty_to_null(input$stage)
      r <- validate_run(if (is.null(st)) make(seq_len(nrow(df))) else {
        shiny::req(st %in% names(df))
        spc_by_stage(df[[st]], make)
      })
      if (input$type %in% ruled_types) {
        rules <- as.integer(input$rules %||% integer(0))
        r$charts <- lapply(r$charts, apply_rules, rules = rules)
      }
      r
    })

    draw <- function() {
      charts <- lapply(result()$charts, plot_chart)
      if (length(charts) == 1) print(charts[[1]])
      else {
        grid::grid.newpage()
        grid::pushViewport(grid::viewport(layout = grid::grid.layout(length(charts), 1)))
        for (i in seq_along(charts)) print(charts[[i]], vp = grid::viewport(layout.pos.row = i, layout.pos.col = 1))
      }
      invisible(NULL)
    }
    output$chart <- shiny::renderPlot(draw(), height = function() if (length(result()$charts) > 2) 760 else 560)
    output$download <- plot_download("control-chart", draw, height = 8)

    output$limits <- shiny::renderTable({
      r <- result()
      f <- function(v) formatC(v, digits = 5, format = "g")
      rows <- lapply(r$charts, function(ch) {
        if (!is.null(ch$stage_breaks)) {
          ends <- ch$stage_breaks
          return(data.frame(Chart = paste(ch$title, "-", ch$stage_names), LCL = f(ch$lcl[ends]), CL = f(ch$cl[ends]), UCL = f(ch$ucl[ends])))
        }
        varying <- length(unique(round(stats::na.omit(ch$ucl), 10))) > 1
        last <- length(ch$ucl)
        data.frame(Chart = ch$title,
                   LCL = if (varying) paste0("varies (last ", f(ch$lcl[last]), ")") else f(ch$lcl[1]),
                   CL = f(ch$cl[last]),
                   UCL = if (varying) paste0("varies (last ", f(ch$ucl[last]), ")") else f(ch$ucl[1]))
      })
      out <- do.call(rbind, rows)
      if (!is.null(r$sigma)) out <- rbind(out, data.frame(Chart = if (input$type %in% c("imr_r", "imr_s")) "Sigma (between)" else "Sigma (within)",
                                                          LCL = "", CL = f(r$sigma), UCL = ""))
      if (!is.null(r$stage_sigma) && any(!is.na(r$stage_sigma)))
        out <- rbind(out, data.frame(Chart = paste("Sigma -", names(r$stage_sigma)), LCL = "", CL = f(r$stage_sigma), UCL = ""))
      out
    })
    output$extra <- shiny::renderTable({
      r <- result()
      if (!is.null(r$parts)) return(r$parts)
      if (!is.null(r$sigma_z)) return(data.frame(Statistic = "Laney sigma Z", Value = r$sigma_z))
      NULL
    }, digits = 4)

    output$signals <- shiny::renderTable({
      v <- violation_table(result()$charts)
      if (is.null(v)) return(data.frame(Result = "No out-of-control signals."))
      thin_first(v, 200, "signals")
    })
  })
}
