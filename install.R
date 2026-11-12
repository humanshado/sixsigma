# Install or upgrade the R packages Six Sigma Studio needs. Run once:
#   Rscript install.R
# Packages that are missing, or older than the version the app needs, are
# installed from CRAN. Newer versions are left alone.

source("R/requirements.R")

repos <- getOption("repos")
if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@")) repos <- c(CRAN = "https://cloud.r-project.org")

if (getRversion() < r_min_version) {
  stop("Six Sigma Studio needs R ", r_min_version, " or later (you have ", getRversion(), "). ",
       "Install a newer R from https://cran.r-project.org/ first.", call. = FALSE)
}

todo <- outdated_packages()
if (!nrow(todo)) {
  cat("All required packages are installed and up to date.\n")
} else {
  cat("Installing or upgrading:", paste0(todo$package, collapse = ", "), "\n")
  install.packages(todo$package, repos = repos, dependencies = c("Depends", "Imports", "LinkingTo"))
  still <- outdated_packages()
  if (nrow(still)) {
    cat("\nThese packages are still missing or too old:\n")
    print(still, row.names = FALSE)
    cat("\nIf a package was in use while upgrading, restart R (or RStudio) and run this script again.\n",
        "If your R is set to an old CRAN snapshot, run:\n",
        "  install.packages(c(", paste0('"', still$package, '"', collapse = ", "),
        "), repos = \"https://cloud.r-project.org\")\n", sep = "")
    quit(status = 1)
  }
  cat("Done. All required packages are installed and up to date.\n")
}
