# Calibration simulation for infer_curve()
#
# Checks the type-I error rate (beta = 0: no specification has an effect) and the
# power (beta > 0) of the three test statistics of summary.specr.infer().
# Not part of the package build (see .Rbuildignore). Meant to be run on a machine
# with many cores, with specr installed from this source tree:
#
#   R CMD INSTALL .
#   Rscript dev/sim_calibration.R <reps> <n_samples> <beta> <scenario> <cores> [out.rds]
#
# Scenarios:
#   basic      x1/x2 x y1/y2 x controls c1/c2 (16 specifications, fast lm route)
#   subsets_na as basic with y1 only, subsets of a grouping variable and 10% missing
#              values in c1 (24 specifications, fast lm route)
#   lmer       random-intercept models (general route, slow), data clustered in 50 groups

args <- commandArgs(trailingOnly = TRUE)
reps <- as.integer(args[1])
n_samples <- as.integer(args[2])
beta <- as.numeric(args[3])
scenario <- args[4]
cores <- as.integer(args[5])
out_file <- if (length(args) >= 6) args[6] else NULL

suppressMessages(library(specr))

# All specifications have a true effect of `beta` (0 = global null). c1 is related
# to x but not y, c2 predicts y but not x
simulate_data <- function(n, beta, scenario) {
  c1 <- rnorm(n)
  c2 <- rnorm(n)
  g <- sample(1:50, n, replace = TRUE)
  u <- if (scenario == "lmer") rnorm(50, sd = .7)[g] else 0
  x1 <- 0.5 * c1 + rnorm(n)
  x2 <- 0.6 * x1 + rnorm(n)
  d <- data.frame(x1, x2, c1, c2,
                  y1 = beta * x1 + 0.5 * c2 + u + rnorm(n),
                  y2 = beta * x2 + 0.4 * c2 + u + rnorm(n),
                  group = sample(c("a", "b"), n, replace = TRUE),
                  cluster = g)
  if (scenario == "subsets_na") d$c1[sample(n, n / 10)] <- NA
  d
}

setup_scenario <- function(d, scenario) {
  switch(scenario,
    basic = setup(data = d, x = c("x1", "x2"), y = c("y1", "y2"), model = "lm",
                  controls = c("c1", "c2"), fun2 = NULL),
    subsets_na = setup(data = d, x = c("x1", "x2"), y = "y1", model = "lm",
                       controls = c("c1", "c2"), subsets = list(group = c("a", "b")),
                       fun2 = NULL),
    lmer = setup(data = d, x = c("x1", "x2"), y = "y1", model = "lmer",
                 controls = c("c1", "c2"), add_to_formula = "(1|cluster)",
                 fun1 = function(x) {
                   fit <- broom.mixed::tidy(x, effects = "fixed")
                   fit$p.value <- 2 * pnorm(-abs(fit$statistic))   # Wald p-values
                   fit
                 },
                 fun2 = NULL))
}

one_rep <- function(r) {
  set.seed(1000 + r)
  d <- simulate_data(500, beta, scenario)
  specs <- setup_scenario(d, scenario)
  results <- suppressMessages(specr(specs))
  time <- system.time(inference <- infer_curve(results, specs, n_samples = n_samples))[["elapsed"]]
  s <- summary(inference)
  data.frame(rep = r,
             statistic = c("median", "share significant", "aggregated z"),
             p.value = s$p.value,
             n_specs = s$n_specs,
             seconds = time)
}

res <- do.call(rbind, parallel::mclapply(seq_len(reps), function(r) {
  tryCatch(one_rep(r), error = function(e) NULL)
}, mc.cores = cores))

if (!is.null(out_file)) saveRDS(res, out_file)

cat(sprintf("scenario = %s | beta = %s | reps = %d | n_samples = %d | specs = %d\n",
            scenario, beta, length(unique(res$rep)), n_samples, res$n_specs[1]))
summary_table <- aggregate(cbind(`p < .05` = p.value < .05, `p < .10` = p.value < .10,
                                 `p < .50` = p.value < .50) ~ statistic,
                           data = res, FUN = mean)
print(summary_table, digits = 3, row.names = FALSE)
cat(sprintf("mean seconds per infer_curve() call: %.2f\n", mean(res$seconds)))
