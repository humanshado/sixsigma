# Minimum package versions Six Sigma Studio needs. Each minimum is the oldest
# version that has every function the app uses; newer versions are fine.
#   shiny 1.7.0      markdown(), modules and the inputs used
#   bslib 0.5.0      page_navbar(), nav_panel(), card(), layout_sidebar(), nav_select()
#   htmltools 0.5.2  tagQuery()
#   DT 0.6           editable cells with disabled columns
#   ggplot2 3.4.0    the `linewidth` aesthetic
#   SixSigma 0.10.0  ss.rr() with print_plot and signifstars
# The other packages have had the functions used for many years.

r_min_version <- "4.1.0"

required_packages <- c(
  shiny     = "1.7.0",
  bslib     = "0.5.0",
  htmltools = "0.5.2",
  DT        = "0.6",
  ggplot2   = "3.4.0",
  scales    = "1.0.0",
  SixSigma  = "0.10.0",
  nortest   = "1.0-4",
  e1071     = "1.6",
  readxl    = "1.0.0",
  testthat  = "3.0.0"
)

# Packages that are missing or older than required, with what is installed.
outdated_packages <- function(req = required_packages) {
  have <- vapply(names(req), function(p) {
    v <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
    v
  }, character(1))
  bad <- is.na(have) | mapply(function(h, r) !is.na(h) && utils::compareVersion(h, r) < 0, have, req)
  data.frame(package = names(req)[bad], installed = ifelse(is.na(have[bad]), "not installed", have[bad]),
             required = sprintf(">= %s", unname(req[bad])), stringsAsFactors = FALSE)
}

# Stop with a readable message if the app cannot run with the installed packages.
check_requirements <- function() {
  todo <- outdated_packages(required_packages[setdiff(names(required_packages), "testthat")])
  if (nrow(todo)) {
    lines <- sprintf("  %-10s installed: %-14s needs %s", todo$package, todo$installed, todo$required)
    stop("\n\nSix Sigma Studio needs newer versions of these R packages:\n",
         paste(lines, collapse = "\n"),
         "\n\nFix: from the app folder run   Rscript install.R\n",
         "(or in R: install.packages(c(", paste0('"', todo$package, '"', collapse = ", "), ")) ),\n",
         "then restart R and start the app again.\n", call. = FALSE)
  }
  invisible(TRUE)
}
