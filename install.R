# Install the R packages Six Sigma Studio needs. Run once:
#   Rscript install.R
pkgs <- c("shiny", "bslib", "DT", "ggplot2", "SixSigma", "nortest", "e1071",
          "readxl", "testthat")
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
cat("All packages installed.\n")
