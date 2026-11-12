# Quality Tools > Control Charts.

control_types <- c(
  "I-MR (individuals)" = "imr",
  "Xbar-R (subgroups)" = "xbar_r",
  "Xbar-S (subgroups)" = "xbar_s",
  "P (proportion defective)" = "p",
  "NP (number defective)" = "np",
  "C (defects per sample)" = "c",
  "U (defects per unit)" = "u"
)

controlUI <- function(id) {
  ns <- shiny::NS(id)
  analysis_layout(
    "Control charts",
    shiny::tagList(
      shiny::selectInput(ns("type"), "Chart type", control_types),
      column_picker(ns("var"), "Measurement / count column"),
      shiny::conditionalPanel(
        "['xbar_r','xbar_s'].indexOf(input.type) >= 0", ns = ns,
        shiny::radioButtons(ns("sg_mode"), "Subgroups", c("Fixed size" = "size", "Group column" = "col")),
        shiny::conditionalPanel("input.sg_mode == 'size'", ns = ns,
                                shiny::numericInput(ns("sg_size"), "Subgroup size", 5, min = 2, max = 50)),
        shiny::conditionalPanel("input.sg_mode == 'col'", ns = ns,
                                column_picker(ns("sg_col"), "Subgroup column"))
      ),
      shiny::conditionalPanel(
        "['p','np','u'].indexOf(input.type) >= 0", ns = ns,
        shiny::radioButtons(ns("n_mode"), "Sample size", c("Constant" = "const", "Column" = "col")),
        shiny::conditionalPanel("input.n_mode == 'const'", ns = ns,
                                shiny::numericInput(ns("n_const"), "Sample size", 50, min = 1)),
        shiny::conditionalPanel("input.n_mode == 'col'", ns = ns,
                                column_picker(ns("n_col"), "Sample size column"))
      ),
      shiny::numericInput(ns("k"), "Sigma multiple for limits", 3, min = 1, max = 6, step = 0.5),
      blank_numeric(ns("n_est"), "Estimate limits from first N points (blank = all)", min = 2),
      shiny::downloadButton(ns("download"), "Download chart"),
      shiny::helpText("Red points break a run rule: beyond the limits, 9 in a row on one side of the ",
                      "centre line, or 6 in a row trending.")
    ),
    shiny::tagList(
      bslib::card(shiny::plotOutput(ns("chart"), height = 560)),
      bslib::layout_columns(
        col_widths = c(5, 7),
        bslib::card(bslib::card_header("Limits"), shiny::tableOutput(ns("limits"))),
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
      n_col = list(type = "numeric", default = 1)
    )))

    result <- shiny::reactive({
      df <- data()
      shiny::req(input$var %in% names(df))
      x <- df[[input$var]]
      k <- input$k
      shiny::validate(shiny::need(isTRUE(k > 0), "Sigma multiple must be positive."))
      n_est <- num_or_na(input$n_est)
      sizes <- function() {
        if (input$n_mode == "const") {
          shiny::validate(shiny::need(isTRUE(input$n_const >= 1), "Enter a sample size."))
          rep(input$n_const, length(x))
        } else {
          shiny::req(input$n_col %in% names(df))
          df[[input$n_col]]
        }
      }
      counts_ok <- function(d, n = NULL) {
        keep <- !is.na(d) & (if (is.null(n)) TRUE else !is.na(n))
        list(d = d[keep], n = if (is.null(n)) NULL else n[keep])
      }
      validate_run(switch(input$type,
        imr = spc_imr(x, k, n_est),
        xbar_r = , xbar_s = {
          sg <- if (input$sg_mode == "size") spc_subgroups(x, size = input$sg_size)
                else spc_subgroups(x, groups = df[[input$sg_col]])
          if (input$type == "xbar_r") spc_xbar_r(sg, k, n_est) else spc_xbar_s(sg, k, n_est)
        },
        p = { v <- counts_ok(x, sizes()); spc_p(v$d, v$n, k, n_est) },
        np = { v <- counts_ok(x, sizes()); spc_np(v$d, v$n, k, n_est) },
        c = { v <- counts_ok(x); spc_c(v$d, k, n_est) },
        u = { v <- counts_ok(x, sizes()); spc_u(v$d, v$n, k, n_est) }
      ))
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
    output$chart <- shiny::renderPlot(draw())
    output$download <- plot_download("control-chart", draw)

    output$limits <- shiny::renderTable({
      r <- result()
      rows <- lapply(r$charts, function(ch) {
        varying <- length(unique(round(ch$ucl, 10))) > 1
        data.frame(Chart = ch$title,
                   LCL = if (varying) "varies" else formatC(ch$lcl[1], digits = 5, format = "g"),
                   CL = formatC(ch$cl[length(ch$cl)], digits = 5, format = "g"),
                   UCL = if (varying) "varies" else formatC(ch$ucl[1], digits = 5, format = "g"))
      })
      out <- do.call(rbind, rows)
      if (!is.null(r$sigma)) out <- rbind(out, data.frame(Chart = "Estimated sigma (within)", LCL = "",
                                                          CL = formatC(r$sigma, digits = 5, format = "g"), UCL = ""))
      out
    })

    output$signals <- shiny::renderTable({
      v <- violation_table(result()$charts)
      if (is.null(v)) data.frame(Result = "No out-of-control signals.") else v
    })
  })
}
