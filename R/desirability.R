# Response optimizer with Derringer & Suich (1980) desirability functions.
# Each response gets a goal (maximize, minimize or hit a target), its own
# fitted model, and an importance; the optimizer searches the experimental
# region for settings that maximise the overall desirability
#   D = (prod d_i ^ r_i) ^ (1 / sum r_i).

desirability_d <- function(y, goal, lower, target, upper, weight = 1, weight2 = weight) {
  switch(goal,
    maximize = ifelse(y <= lower, 0, ifelse(y >= target, 1, ((y - lower) / (target - lower))^weight)),
    minimize = ifelse(y <= target, 1, ifelse(y >= upper, 0, ((upper - y) / (upper - target))^weight)),
    target = ifelse(y < lower | y > upper, 0,
                    ifelse(y <= target, ((y - lower) / (target - lower))^weight,
                           ((upper - y) / (upper - target))^weight2)),
    stop("Unknown goal."))
}

check_goal <- function(g) {
  ok <- switch(g$goal,
    maximize = !is.na(g$lower) && !is.na(g$target) && g$lower < g$target,
    minimize = !is.na(g$target) && !is.na(g$upper) && g$target < g$upper,
    target = !any(is.na(c(g$lower, g$target, g$upper))) && g$lower < g$target && g$target < g$upper)
  if (!isTRUE(ok)) stop("Check the limits for ", g$response, ": ",
                        switch(g$goal, maximize = "lower < target", minimize = "target < upper",
                               target = "lower < target < upper"), ".")
}

# models: list of rsm_fit() results sharing the same factors.
# goals: list of list(response, goal, lower, target, upper, weight, importance).
optimize_desirability <- function(models, goals, starts = 40, seed = 1) {
  for (g in goals) check_goal(g)
  vars <- models[[1]]$vars; k <- length(vars)
  predict_all <- function(x) {
    nd <- as.data.frame(matrix(x, 1, k, dimnames = list(NULL, vars)))
    vapply(models, function(m) stats::predict(m$fit, nd)[[1]], numeric(1))
  }
  overall <- function(x) {
    y <- predict_all(x)
    d <- mapply(function(yy, g) desirability_d(yy, g$goal, g$lower, g$target, g$upper, g$weight), y, goals)
    r <- vapply(goals, function(g) g$importance, numeric(1))
    if (any(d <= 0)) return(0)
    exp(sum(r * log(d)) / sum(r))
  }
  # A smooth surrogate that still improves when some d_i are 0, so the search
  # can climb out of flat regions.
  soft <- function(x) {
    y <- predict_all(x)
    pen <- mapply(function(yy, g) {
      span <- diff(range(c(g$lower, g$target, g$upper), na.rm = TRUE))
      lo <- if (g$goal == "minimize") -Inf else g$lower; hi <- if (g$goal == "maximize") Inf else g$upper
      (max(0, lo - yy) + max(0, yy - hi)) / span
    }, y, goals)
    overall(x) - sum(pen)
  }
  set.seed(seed)
  grid_starts <- as.matrix(expand.grid(rep(list(c(-1, 0, 1)), min(k, 4))))
  if (k > 4) grid_starts <- cbind(grid_starts, matrix(0, nrow(grid_starts), k - 4))
  st <- rbind(grid_starts, matrix(stats::runif(starts * k, -1, 1), starts, k))
  best <- NULL
  for (i in seq_len(nrow(st))) {
    o <- tryCatch(stats::optim(st[i, ], function(x) -soft(x), method = "L-BFGS-B", lower = -1, upper = 1),
                  error = function(e) NULL)
    if (!is.null(o) && (is.null(best) || -o$value > best$value)) best <- list(par = o$par, value = -o$value)
  }
  x <- best$par
  y <- predict_all(x)
  d <- mapply(function(yy, g) desirability_d(yy, g$goal, g$lower, g$target, g$upper, g$weight), y, goals)
  coding <- models[[1]]$coding
  list(
    settings = data.frame(Factor = models[[1]]$factors, Value = rsm_uncode(x, coding), Coded = x, row.names = NULL),
    responses = data.frame(Response = vapply(goals, `[[`, "", "response"),
                           Goal = vapply(goals, `[[`, "", "goal"), Predicted = y, Desirability = d, row.names = NULL),
    D = overall(x), coded = x, predict_all = predict_all, overall = overall
  )
}

# Optimization plot: each response (rows) against each factor (columns), with
# the other factors held at the optimum, as in Minitab's response optimizer.
optimizer_plot <- function(models, opt, goals) {
  k <- length(opt$coded)
  rows <- list()
  for (j in seq_len(k)) {
    grid <- seq(-1, 1, length.out = 61)
    for (i in seq_along(models)) {
      yv <- vapply(grid, function(v) { x <- opt$coded; x[j] <- v; opt$predict_all(x)[i] }, numeric(1))
      cd <- models[[1]]$coding[[j]]
      rows[[length(rows) + 1]] <- data.frame(Factor = models[[1]]$factors[j], Response = goals[[i]]$response,
                                             x = cd$mid + cd$half * grid, y = yv)
    }
  }
  d <- do.call(rbind, rows)
  d$Factor <- factor(d$Factor, levels = models[[1]]$factors)
  d$Response <- factor(d$Response, levels = vapply(goals, `[[`, "", "response"))
  opt_pts <- do.call(rbind, lapply(seq_len(k), function(j) data.frame(
    Factor = models[[1]]$factors[j], Response = vapply(goals, `[[`, "", "response"),
    x = opt$settings$Value[j], y = opt$responses$Predicted)))
  opt_pts$Factor <- factor(opt_pts$Factor, levels = levels(d$Factor))
  opt_pts$Response <- factor(opt_pts$Response, levels = levels(d$Response))
  ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_line(colour = "#4a6fa5", linewidth = 1) +
    ggplot2::geom_vline(data = opt_pts, ggplot2::aes(xintercept = x), colour = "#c0392b", linetype = 2) +
    ggplot2::geom_point(data = opt_pts, colour = "#c0392b", size = 2.5) +
    ggplot2::facet_grid(Response ~ Factor, scales = "free") +
    ggplot2::labs(title = sprintf("Optimization plot (overall desirability D = %.3f)", opt$D),
                  subtitle = "Each panel varies one factor with the others held at the optimum (red)",
                  x = NULL, y = NULL) +
    ggplot2::theme_bw(base_size = 12) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}
