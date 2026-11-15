# Every published reference value in validation/validation_cases.R must be reproduced.
library(ggplot2)
`%||%` <- function(a, b) if (is.null(a)) b else a
for (f in list.files(file.path("..", "..", "R"), full.names = TRUE)) source(f, local = TRUE)
source(file.path("..", "..", "validation", "validation_cases.R"), local = TRUE)

test_that("all validation checks reproduce the published reference values", {
  res <- validation_checks()
  expect_gte(nrow(res), 50)
  failed <- res[!res$Pass, c("Reference", "Statistic", "Published", "Computed")]
  expect_equal(nrow(failed), 0, info = paste(capture.output(print(failed)), collapse = "\n"))
})
