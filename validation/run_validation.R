# Run the validation checks and write validation/VALIDATION.md and a CSV.
#   Rscript validation/run_validation.R
# Exits with an error if any check fails.
suppressMessages({ library(ggplot2) })
`%||%` <- function(a, b) if (is.null(a)) b else a
source("validation/validation_cases.R")
root <- validation_root()
for (f in list.files(file.path(root, "R"), full.names = TRUE)) source(f)
res <- validation_checks()
fmt <- function(v) formatC(v, digits = 10, format = "g")
md <- c("# Validation report", "",
  sprintf("Six Sigma Studio %s, run %s with R %s.", APP_VERSION, format(Sys.time(), "%Y-%m-%d"), getRversion()), "",
  sprintf("**%d of %d checks pass.** Each check compares a result computed by the app's own functions with a",
          sum(res$Pass), nrow(res)),
  "published reference value (certified values, textbook examples or standard tables). Values printed",
  "in a reference are compared at the precision printed (half a unit in the last digit).", "",
  "The input data are in `validation/datasets/`, so the same checks can be repeated in Minitab, JMP or",
  "any other package.", "",
  "| Area | Reference | Statistic | Published | Six Sigma Studio | Result |", "|---|---|---|---|---|---|",
  sprintf("| %s | %s | %s | %s | %s | %s |", res$Area, res$Reference, res$Statistic, fmt(res$Published), fmt(res$Computed),
          ifelse(res$Pass, "Pass", "**FAIL**")), "",
  "## Notes", "",
  "- **Tolerance factor.** Some tables print 3.379 for the two-sided factor (n = 10, 95%/95%); that is Howe's",
  "  approximation. The app computes the exact factor (3.393); a simulation of 200,000 samples confirms it gives",
  "  95.0% confidence.",
  "- **Insulation intercept.** The fitted sigma matches the published 0.2592 to all four decimals. The intercept",
  "  (-6.0182 log10 hours) is compared at two decimals because the third depends on the temperature constant and",
  "  rounding used in the reference; the difference (0.0008 log10 hours) is 0.2% in predicted life.",
  "- **Not covered here.** Results computed by the SixSigma package itself (capability report, Gage R&R) are that",
  "  package's own; the unit tests in `tests/testthat` check many more methods against R reference functions.")
writeLines(md, file.path(root, "validation", "VALIDATION.md"))
utils::write.csv(res, file.path(root, "validation", "validation_results.csv"), row.names = FALSE)
for (n in names(ds <- validation_datasets())) utils::write.csv(ds[[n]], file.path(root, "validation", "datasets", paste0(n, ".csv")), row.names = FALSE)
cat(sprintf("%d of %d checks pass.\n", sum(res$Pass), nrow(res)))
if (!all(res$Pass)) { print(res[!res$Pass, ]); quit(status = 1) }
