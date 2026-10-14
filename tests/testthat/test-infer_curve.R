# Fixtures (incl. `inference`): see helper-specs.R
specs <- specs_infer
results <- results_infer


# Structure ----

test_that("infer_curve returns a specr.infer object with one column per specification", {
  expect_s3_class(inference, "specr.infer")
  expect_equal(dim(inference$estimates), c(50, nrow(results$data)))
  expect_equal(dim(inference$p_values), c(50, nrow(results$data)))
  expect_equal(inference$observed$estimate, results$data$estimate)
  expect_equal(inference$n_fast, nrow(results$data))
  expect_equal(inference$n_failed, 0)
})

test_that("data can be passed as setup object or data frame", {
  set.seed(2)
  a <- infer_curve(results, specs, n_samples = 5)
  set.seed(2)
  b <- infer_curve(results, example_data, n_samples = 5)
  expect_identical(a$estimates, b$estimates)
})


# The null is imposed correctly ----

test_that("refitting on the null outcome yields an estimate of zero", {
  prep <- prepare_null_specs(results$data, as.data.frame(example_data))
  est0 <- vapply(prep$specs, function(s) {
    d <- prep$designs[[s$design]]
    ols_coef_p(d$X, s$ystar, s$j)[1]
  }, numeric(1))
  expect_true(all(abs(est0) < 1e-10))
})

test_that("each specification is refitted on its own number of observations", {
  d <- example_data
  d$c1[1:50] <- NA
  specs_na <- setup(data = d, x = c("x1", "x2"), y = "y1", model = "lm",
                    controls = c("c1", "c2"), subsets = list(group2 = c("female", "male")))
  results_na <- specr(specs_na)
  prep <- prepare_null_specs(results_na$data, as.data.frame(d))
  n_rows <- vapply(prep$specs, function(s) nrow(prep$designs[[s$design]]$X), numeric(1))
  expect_equal(n_rows, results_na$data$fit_nobs)
})

test_that("the spread under the null matches the observed standard errors", {
  # Regression test: stacking the data of all specifications made this ratio
  # 0.5-0.9 in earlier versions of boot_null()
  set.seed(3)
  inf <- infer_curve(results, specs, n_samples = 300)
  ratio <- apply(inf$estimates, 2, sd) / results$data$std.error
  expect_true(all(ratio > .75 & ratio < 1.3))
  expect_true(all(abs(colMeans(inf$estimates)) < 4 * results$data$std.error / sqrt(300)))
})


# Same universe of specifications ----

test_that("the curves under the null use exactly the observed specifications", {
  specs_complex <- setup(data = example_data,
                         x = c("x1", "x2"),
                         y = "y1",
                         model = "lm",
                         controls = c("c1", "c2", "c3"),
                         subsets = list(group2 = c("female", "male")),
                         add_to_formula = "c4",
                         simplify = TRUE)
  results_complex <- specr(specs_complex)
  set.seed(4)
  inf <- infer_curve(results_complex, specs_complex, n_samples = 5)
  expect_equal(ncol(inf$estimates), nrow(results_complex$data))
  expect_equal(inf$observed[c("x", "y", "model", "controls", "subsets")],
               tibble::as_tibble(results_complex$data[c("x", "y", "model", "controls", "subsets")]))
  expect_equal(inf$n_failed, 0)
})

test_that("filtered specifications are respected", {
  specs_filtered <- specs
  specs_filtered$specs <- dplyr::filter(specs$specs, controls != "c1 + c2")
  results_filtered <- specr(specs_filtered)
  set.seed(5)
  inf <- infer_curve(results_filtered, specs_filtered, n_samples = 5)
  expect_equal(ncol(inf$estimates), nrow(results_filtered$data))
  expect_false("c1 + c2" %in% inf$observed$controls)
})


# Fast and general route ----

test_that("the fast route gives the same results as refitting with lm()", {
  set.seed(6)
  fast <- infer_curve(results, specs, n_samples = 10, fast = TRUE)
  set.seed(6)
  general <- infer_curve(results, specs, n_samples = 10, fast = FALSE)
  expect_equal(general$n_fast, 0)
  expect_equal(fast$estimates, general$estimates, tolerance = 1e-8)
  expect_equal(fast$p_values, general$p_values, tolerance = 1e-8)
})

test_that("the general route works for multilevel models", {
  skip_if_not_installed("broom.mixed")
  specs_lmer <- setup(data = example_data,
                      x = c("x1", "x2"),
                      y = "y1",
                      model = "lmer",
                      controls = "c1",
                      add_to_formula = "(1|group1)",
                      fun1 = function(x) broom.mixed::tidy(x, conf.int = TRUE),
                      fun2 = NULL)
  results_lmer <- specr(specs_lmer)
  set.seed(7)
  inf <- infer_curve(results_lmer, specs_lmer, n_samples = 5)
  expect_equal(inf$n_fast, 0)
  expect_equal(inf$n_failed, 0)
  s <- summary(inf)
  expect_true(s$p.value[s$statistic == "median"] > 0)
})


# Reproducibility ----

test_that("results are reproducible with set.seed()", {
  set.seed(8)
  a <- infer_curve(results, specs, n_samples = 10)
  set.seed(8)
  b <- infer_curve(results, specs, n_samples = 10)
  set.seed(9)
  c <- infer_curve(results, specs, n_samples = 10)
  expect_identical(a$estimates, b$estimates)
  expect_false(identical(a$estimates, c$estimates))
})

test_that("parallel and sequential resampling give identical results", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not(future::supportsMulticore())
  set.seed(10)
  sequential <- infer_curve(results, specs, n_samples = 10)
  old_plan <- future::plan(future::multicore, workers = 2)
  on.exit(future::plan(old_plan), add = TRUE)
  set.seed(10)
  parallel <- infer_curve(results, specs, n_samples = 10)
  expect_identical(sequential$estimates, parallel$estimates)
})


# Invalid input ----

test_that("informative errors for invalid input", {
  expect_error(infer_curve(results), "data set used in the analysis")
  expect_error(infer_curve(1, specs), "must be the result of `specr\\(\\)`")
  expect_error(infer_curve(results, specs, n_samples = 0), "positive number")
  expect_error(infer_curve(results, specs, .options = furrr::furrr_options()), "cannot be set")

  sem <- results$data
  sem$op <- "~"
  expect_error(infer_curve(sem, example_data), "structural equation models")

  d <- example_data
  d$x1 <- as.character(d$x1)
  expect_error(infer_curve(results, d), "must be numeric")
})

test_that("models that are not linear in the outcome are refused", {
  d <- example_data
  d$y_binary <- as.integer(d$y1 > 0)
  assign("logit_model",
         function(formula, data) stats::glm(formula, data = data, family = stats::binomial()),
         envir = globalenv())
  on.exit(rm("logit_model", envir = globalenv()), add = TRUE)
  specs_logit <- setup(data = d, x = "x1", y = "y_binary", model = "logit_model", fun2 = NULL)
  results_logit <- suppressWarnings(specr(specs_logit))
  expect_error(infer_curve(results_logit, specs_logit, n_samples = 5),
               "could not be imposed")
})


# Summary, plot and other methods ----

test_that("summary returns the three statistics with valid p-values", {
  s <- summary(inference)
  expect_equal(nrow(s), 3)
  expect_equal(s$statistic[c(1, 3)], c("median", "aggregated z"))
  expect_true(all(s$p.value > 0 & s$p.value <= 1))
  expect_true(all(s$null_lower <= s$null_upper))
  expect_equal(s$n_specs, rep(nrow(results$data), 3))
})

test_that("summary respects direction, alpha and group", {
  expect_equal(summary(inference, direction = "greater")$statistic[2], "share significant (positive)")
  expect_equal(summary(inference, direction = "less")$statistic[2], "share significant (negative)")
  expect_lte(summary(inference, alpha = .001)$estimate[2], summary(inference)$estimate[2])
  s <- summary(inference, group = "y")
  expect_equal(nrow(s), 6)
  expect_equal(unique(s$y), c("y1", "y2"))
  expect_error(summary(inference, group = "nonsense"), "names of analytical choices")
})

test_that("p-values are never exactly zero", {
  specs_y1 <- setup(data = example_data, x = c("x1", "x2"), y = "y1", model = "lm",
                    controls = c("c1", "c2"))
  set.seed(11)
  inf <- infer_curve(specr(specs_y1), specs_y1, n_samples = 20)
  s <- summary(inf)
  # No curve under the null is as extreme as the observed one: p = 1 / (1 + 20)
  expect_equal(s$p.value[s$statistic %in% c("median", "aggregated z")], rep(1 / 21, 2))
  expect_true(all(s$p.value >= 1 / 21))
})

test_that("print, plot and as_tibble work", {
  expect_output(print(inference), "Inference on the specification curve")
  expect_s3_class(plot(inference), "ggplot")
  long <- as_tibble(inference)
  expect_equal(nrow(long), 50 * nrow(results$data))
  expect_true(all(c("sample", "x", "y", "estimate", "p.value") %in% names(long)))
})


# Calibration ----

test_that("the tests are calibrated when no specification has an effect", {
  skip_on_cran()
  set.seed(12)
  p <- vapply(seq_len(100), function(r) {
    n <- 200
    c1 <- rnorm(n); c2 <- rnorm(n)
    x1 <- 0.5 * c1 + rnorm(n)
    x2 <- 0.6 * x1 + rnorm(n)
    d <- data.frame(x1, x2, c1, c2, y1 = 0.5 * c2 + rnorm(n))
    specs_null <- setup(data = d, x = c("x1", "x2"), y = "y1", model = "lm",
                        controls = c("c1", "c2"), fun2 = NULL)
    s <- summary(infer_curve(specr(specs_null), specs_null, n_samples = 100))
    s$p.value[s$statistic == "median"]
  }, numeric(1))
  # Earlier versions of boot_null() rejected the null in about 30% of the cases
  expect_lt(mean(p < .05), .12)
  expect_gt(mean(p < .5), .3)
  expect_lt(mean(p < .5), .7)
})


# Models given as a named list (overhaul phase 4) ----

test_that("lm given as a function in a named list uses the fast route", {
  specs_list <- setup(data = example_data, x = c("x1", "x2"), y = "y1",
                      model = list(ols = lm), controls = "c1")
  set.seed(13)
  inf <- infer_curve(specr(specs_list), specs_list, n_samples = 10)
  expect_equal(inf$n_fast, 4)
  expect_equal(inf$n_failed, 0)
})

test_that("plots and summaries work for a single specification", {
  specs_one <- setup(data = example_data, x = "x1", y = "y1", model = "lm")
  set.seed(14)
  inf <- infer_curve(specr(specs_one), specs_one, n_samples = 20)
  expect_s3_class(plot(inf), "ggplot")
  expect_equal(nrow(summary(inf)), 3)
})

test_that("without p-values, only the median is tested", {
  specs_nop <- setup(data = example_data, x = c("x1", "x2"), y = "y1", model = "lm",
                     fun1 = function(x) broom::tidy(x)[, c("term", "estimate")])
  set.seed(15)
  s <- summary(infer_curve(specr(specs_nop), specs_nop, n_samples = 20))
  expect_false(is.na(s$p.value[s$statistic == "median"]))
  expect_true(all(is.na(s$p.value[s$statistic != "median"])))
})
