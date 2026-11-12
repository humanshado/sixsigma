# Minimum package versions Six Sigma Studio needs.
# bslib 0.5.0 introduced page_navbar(), nav_panel(), card() and the sidebar
# layouts used throughout the app; ggplot2 3.4.0 introduced `linewidth`.

r_min_version <- "4.1.0"

required_packages <- c(
  shiny     = "1.7.5",
  bslib     = "0.5.1",
  htmltools = "0.5.6",
  DT        = "0.28",
  ggplot2   = "3.4.0",
  scales    = "1.2.0",
  SixSigma  = "0.11.1",
  nortest   = "1.0-4",
  e1071     = "1.7-9",
  readxl    = "1.4.0",
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
