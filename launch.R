# Start Six Sigma Studio: installs or upgrades packages if needed, then opens
# the app in the web browser. Used by the start-* launchers.
#   Rscript launch.R            (open a browser)
#   Rscript launch.R --no-browser --port=8080
args <- commandArgs(trailingOnly = TRUE)
here <- tryCatch(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))), error = function(e) ".")
setwd(here)
source(file.path("R", "requirements.R"))
if (nrow(outdated_packages())) {
  cat("Installing the R packages Six Sigma Studio needs (first run only; this can take a few minutes)...\n")
  status <- system2(file.path(R.home("bin"), "Rscript"), "install.R")
  if (status != 0) stop("Package installation failed; see the messages above.", call. = FALSE)
}
port <- as.integer(sub("^--port=", "", grep("^--port=", args, value = TRUE)[1]))
if (is.na(port)) port <- httpuv::randomPort()
browser <- !("--no-browser" %in% args) && !nzchar(Sys.getenv("SSS_NO_BROWSER"))
cat(sprintf("Six Sigma Studio is running at http://127.0.0.1:%d  (close this window to stop it)\n", port))
shiny::runApp(here, port = port, host = "127.0.0.1", launch.browser = browser)
