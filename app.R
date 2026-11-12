# Six Sigma Studio: a point-and-click interface for the SixSigma R package.
# Run with:  Rscript -e 'shiny::runApp(".", launch.browser = TRUE)'
# Files in R/ are loaded automatically by Shiny.

# Check package versions first, so an outdated package gives a clear message.
if (!exists("check_requirements")) source(file.path("R", "requirements.R"))
check_requirements()

library(shiny)
library(bslib)
library(ggplot2)
library(SixSigma)

about_page <- function() {
  bslib::card(
    bslib::card_header("About Six Sigma Studio"),
    shiny::markdown("
A graphical interface for quality engineering, built on the open-source
[SixSigma](https://cran.r-project.org/package=SixSigma) R package by Emilio L. Cano
and co-authors. No R knowledge is needed to use it.

**How to use it**

1. Open **Data** and pick a sample dataset, upload a CSV/Excel file, or paste
   columns copied from a spreadsheet. The active worksheet is used by every analysis.
   Double-click a cell to edit it, or add new columns.
2. Choose an analysis from the **Stat** or **Quality Tools** menus and select columns
   and options in the left-hand panel. Results update immediately.
3. Use the download buttons to save charts as PNG images.

**Analyses**

| Menu | Analysis | Engine |
|---|---|---|
| Stat | Descriptive statistics, normality tests, confidence interval | base R, nortest, `ss.ci` |
| Stat | Hypothesis tests: 1-sample, 2-sample and paired t, one-way ANOVA with Tukey, equal variances, Mann-Whitney / Kruskal-Wallis, 1 and 2 proportions, chi-square | base R `stats` |
| Stat | Regression: simple, polynomial and multiple linear regression with fitted line plot, residual plots, unusual observations and prediction | base R `lm` |
| DOE | Create 2-level full and fractional factorial designs (up to 8 factors) with replicates, center points, randomization and alias structure | built in |
| DOE | Analyze factorial designs: effects, Pareto and normal plots of effects, main effects and interaction plots, curvature test, Lenth's method for unreplicated designs | base R `lm` |
| Quality Tools | Capability analysis (Cp, Cpk, Z, PPM) | `ss.study.ca`, `ss.ca.cp`, `ss.ca.cpk`, `ss.ca.z` |
| Quality Tools | Control charts: I-MR, Xbar-R, Xbar-S, P, NP, C, U | Shewhart formulas with SixSigma constants |
| Quality Tools | Gage R&R (crossed or nested) | `ss.rr` |
| Quality Tools | Taguchi loss function | `ss.lfa` |
| Quality Tools | Cause-and-effect diagram | `ss.ceDiag` |
| Quality Tools | Pareto chart with cumulative percentage and 'Other' grouping | ggplot2 |
| Quality Tools | Process yield (FTY, RTY, DPMO) | `ss.ca.yield` |

Licensed under the GPL (version 2 or later), like the SixSigma package.
")
  )
}

ui <- bslib::page_navbar(
  title = "Six Sigma Studio",
  id = "nav",
  theme = bslib::bs_add_rules(bslib::bs_theme(version = 5, bootswatch = "flatly"),
                             ".shiny-html-output { overflow-x: auto; } pre { white-space: pre-wrap; word-break: break-word; }"),
  fillable = FALSE,
  bslib::nav_panel("Data", icon = shiny::icon("table"), dataUI("data")),
  bslib::nav_menu(
    "Stat", icon = shiny::icon("chart-column"),
    bslib::nav_panel("Basic statistics", basicUI("basic")),
    bslib::nav_panel("Hypothesis tests", hypothesisUI("hypothesis")),
    bslib::nav_panel("Regression", regressionUI("regression"))
  ),
  bslib::nav_menu(
    "DOE", icon = shiny::icon("flask"),
    bslib::nav_panel("Create factorial design", doeCreateUI("doe_create")),
    bslib::nav_panel("Analyze factorial design", doeAnalyzeUI("doe_analyze"))
  ),
  bslib::nav_menu(
    "Quality Tools", icon = shiny::icon("screwdriver-wrench"),
    bslib::nav_panel("Capability analysis", capabilityUI("capability")),
    bslib::nav_panel("Control charts", controlUI("control")),
    bslib::nav_panel("Gage R&R", gageUI("gage")),
    bslib::nav_panel("Loss function", lfaUI("lfa")),
    bslib::nav_panel("Cause-and-effect diagram", ceUI("ce")),
    bslib::nav_panel("Pareto chart", paretoUI("pareto")),
    bslib::nav_panel("Process yield", yieldUI("yield"))
  ),
  bslib::nav_spacer(),
  bslib::nav_panel("About", icon = shiny::icon("circle-info"), about_page())
)

server <- function(input, output, session) {
  created_design <- shiny::reactiveVal(NULL)
  data <- dataServer("data", injected = created_design)
  basicServer("basic", data)
  hypothesisServer("hypothesis", data)
  regressionServer("regression", data)
  doeCreateServer("doe_create", on_create = function(design) {
    created_design(design)
    bslib::nav_select("nav", "Data", session = session)
  })
  doeAnalyzeServer("doe_analyze", data)
  capabilityServer("capability", data)
  controlServer("control", data)
  gageServer("gage", data)
  lfaServer("lfa", data)
  ceServer("ce")
  paretoServer("pareto", data)
  yieldServer("yield")
}

shinyApp(ui, server)
